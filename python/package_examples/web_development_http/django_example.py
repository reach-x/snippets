"""
django - Full-stack web framework
Install: pip install django

Full-featured web framework with ORM, admin panel, auth, forms, templates

NOTE: Django requires a project structure. This file shows example code snippets.
To create a Django project:
    django-admin startproject myproject
    cd myproject
    python manage.py startapp myapp
"""

# ===== Models (models.py) =====

from django.db import models
from django.contrib.auth.models import User


class Category(models.Model):
    """Category model example"""
    name = models.CharField(max_length=100, unique=True)
    slug = models.SlugField(unique=True)
    description = models.TextField(blank=True)
    created_at = models.DateTimeField(auto_now_add=True)

    class Meta:
        verbose_name_plural = "Categories"
        ordering = ['name']

    def __str__(self):
        return self.name


class Article(models.Model):
    """Article model with relationships"""
    title = models.CharField(max_length=200)
    slug = models.SlugField(unique=True)
    content = models.TextField()
    author = models.ForeignKey(User, on_delete=models.CASCADE, related_name='articles')
    category = models.ForeignKey(Category, on_delete=models.SET_NULL, null=True, related_name='articles')
    tags = models.ManyToManyField('Tag', blank=True)
    published = models.BooleanField(default=False)
    created_at = models.DateTimeField(auto_now_add=True)
    updated_at = models.DateTimeField(auto_now=True)

    class Meta:
        ordering = ['-created_at']

    def __str__(self):
        return self.title


class Tag(models.Model):
    """Tag model"""
    name = models.CharField(max_length=50, unique=True)

    def __str__(self):
        return self.name


# ===== Views (views.py) =====

from django.shortcuts import render, get_object_or_404, redirect
from django.http import JsonResponse, HttpResponse
from django.views import View
from django.views.generic import ListView, DetailView, CreateView, UpdateView, DeleteView
from django.contrib.auth.decorators import login_required
from django.contrib.auth.mixins import LoginRequiredMixin


# Function-based views
def home_view(request):
    """Simple function-based view"""
    context = {
        'title': 'Home Page',
        'message': 'Welcome to Django!'
    }
    return render(request, 'home.html', context)


def article_list_view(request):
    """List view"""
    articles = Article.objects.filter(published=True).select_related('author', 'category')
    context = {'articles': articles}
    return render(request, 'articles/list.html', context)


def article_detail_view(request, slug):
    """Detail view with get_object_or_404"""
    article = get_object_or_404(Article, slug=slug, published=True)
    context = {'article': article}
    return render(request, 'articles/detail.html', context)


@login_required
def create_article_view(request):
    """Create view with form handling"""
    if request.method == 'POST':
        form = ArticleForm(request.POST)
        if form.is_valid():
            article = form.save(commit=False)
            article.author = request.user
            article.save()
            form.save_m2m()  # Save many-to-many relationships
            return redirect('article_detail', slug=article.slug)
    else:
        form = ArticleForm()

    return render(request, 'articles/create.html', {'form': form})


# Class-based views
class ArticleListView(ListView):
    """Generic list view"""
    model = Article
    template_name = 'articles/list.html'
    context_object_name = 'articles'
    paginate_by = 10

    def get_queryset(self):
        return Article.objects.filter(published=True).select_related('author', 'category')


class ArticleDetailView(DetailView):
    """Generic detail view"""
    model = Article
    template_name = 'articles/detail.html'
    context_object_name = 'article'

    def get_queryset(self):
        return Article.objects.filter(published=True)


class ArticleCreateView(LoginRequiredMixin, CreateView):
    """Generic create view"""
    model = Article
    fields = ['title', 'content', 'category', 'tags', 'published']
    template_name = 'articles/form.html'

    def form_valid(self, form):
        form.instance.author = self.request.user
        return super().form_valid(form)


# API views (JSON responses)
def api_article_list(request):
    """JSON API endpoint"""
    articles = Article.objects.filter(published=True).values('id', 'title', 'slug', 'created_at')
    return JsonResponse(list(articles), safe=False)


def api_article_detail(request, article_id):
    """JSON API detail endpoint"""
    article = get_object_or_404(Article, id=article_id, published=True)

    data = {
        'id': article.id,
        'title': article.title,
        'content': article.content,
        'author': article.author.username,
        'created_at': article.created_at.isoformat()
    }

    return JsonResponse(data)


# ===== Forms (forms.py) =====

from django import forms


class ArticleForm(forms.ModelForm):
    """Model form for Article"""

    class Meta:
        model = Article
        fields = ['title', 'content', 'category', 'tags', 'published']
        widgets = {
            'content': forms.Textarea(attrs={'rows': 10}),
            'tags': forms.CheckboxSelectMultiple(),
        }

    def clean_title(self):
        """Custom field validation"""
        title = self.cleaned_data.get('title')
        if len(title) < 5:
            raise forms.ValidationError("Title must be at least 5 characters")
        return title


class ContactForm(forms.Form):
    """Regular form (not model form)"""
    name = forms.CharField(max_length=100)
    email = forms.EmailField()
    message = forms.CharField(widget=forms.Textarea)

    def clean_email(self):
        """Custom validation"""
        email = self.cleaned_data.get('email')
        if not email.endswith('@example.com'):
            raise forms.ValidationError("Must use @example.com email")
        return email


# ===== URLs (urls.py) =====

from django.urls import path, include

urlpatterns = [
    # Function-based views
    path('', home_view, name='home'),
    path('articles/', article_list_view, name='article_list'),
    path('articles/<slug:slug>/', article_detail_view, name='article_detail'),
    path('articles/create/', create_article_view, name='article_create'),

    # Class-based views
    path('cbv/articles/', ArticleListView.as_view(), name='article_list_cbv'),
    path('cbv/articles/<slug:slug>/', ArticleDetailView.as_view(), name='article_detail_cbv'),
    path('cbv/articles/create/', ArticleCreateView.as_view(), name='article_create_cbv'),

    # API endpoints
    path('api/articles/', api_article_list, name='api_article_list'),
    path('api/articles/<int:article_id>/', api_article_detail, name='api_article_detail'),
]


# ===== Admin (admin.py) =====

from django.contrib import admin


@admin.register(Category)
class CategoryAdmin(admin.ModelAdmin):
    """Category admin configuration"""
    list_display = ['name', 'slug', 'created_at']
    prepopulated_fields = {'slug': ('name',)}
    search_fields = ['name']


@admin.register(Article)
class ArticleAdmin(admin.ModelAdmin):
    """Article admin configuration"""
    list_display = ['title', 'author', 'category', 'published', 'created_at']
    list_filter = ['published', 'category', 'created_at']
    search_fields = ['title', 'content']
    prepopulated_fields = {'slug': ('title',)}
    date_hierarchy = 'created_at'
    filter_horizontal = ['tags']

    fieldsets = (
        ('Basic Information', {
            'fields': ('title', 'slug', 'content')
        }),
        ('Relationships', {
            'fields': ('author', 'category', 'tags')
        }),
        ('Publishing', {
            'fields': ('published',)
        }),
    )


# ===== ORM Query Examples =====

def orm_examples():
    """Django ORM query examples"""

    # Create
    article = Article.objects.create(
        title="New Article",
        content="Article content",
        author_id=1,
        published=True
    )

    # Get single object
    article = Article.objects.get(id=1)
    article = Article.objects.get(slug='my-article')

    # Filter
    articles = Article.objects.filter(published=True)
    articles = Article.objects.filter(author__username='john')
    articles = Article.objects.filter(title__icontains='django')

    # Exclude
    articles = Article.objects.exclude(published=False)

    # Chaining filters
    articles = Article.objects.filter(published=True).filter(category__name='Tech')

    # Ordering
    articles = Article.objects.order_by('-created_at')
    articles = Article.objects.order_by('category', '-created_at')

    # Limit/Offset
    articles = Article.objects.all()[:10]  # First 10
    articles = Article.objects.all()[10:20]  # Next 10

    # Select related (for ForeignKey - reduces queries)
    articles = Article.objects.select_related('author', 'category')

    # Prefetch related (for ManyToMany - reduces queries)
    articles = Article.objects.prefetch_related('tags')

    # Aggregation
    from django.db.models import Count, Avg

    Article.objects.count()
    Article.objects.filter(published=True).count()

    # Annotation
    categories = Category.objects.annotate(article_count=Count('articles'))

    # Update
    article.title = "Updated Title"
    article.save()

    # Bulk update
    Article.objects.filter(category_id=1).update(published=True)

    # Delete
    article.delete()

    # Bulk delete
    Article.objects.filter(created_at__year=2020).delete()

    # Q objects (complex queries)
    from django.db.models import Q

    articles = Article.objects.filter(
        Q(title__icontains='django') | Q(content__icontains='django')
    )

    # F objects (field references)
    from django.db.models import F

    Article.objects.filter(created_at__gt=F('updated_at'))

    # Exists
    has_articles = Article.objects.filter(published=True).exists()

    # Values (return dictionaries)
    articles = Article.objects.values('id', 'title', 'slug')

    # Values list (return tuples)
    titles = Article.objects.values_list('title', flat=True)


# ===== Middleware Example (middleware.py) =====

class CustomMiddleware:
    """Custom middleware"""

    def __init__(self, get_response):
        self.get_response = get_response

    def __call__(self, request):
        # Code executed before view
        print(f"Request: {request.method} {request.path}")

        response = self.get_response(request)

        # Code executed after view
        response['X-Custom-Header'] = 'Django App'

        return response


# ===== Signals Example (signals.py) =====

from django.db.models.signals import post_save, pre_delete
from django.dispatch import receiver


@receiver(post_save, sender=Article)
def article_post_save(sender, instance, created, **kwargs):
    """Signal handler for article save"""
    if created:
        print(f"New article created: {instance.title}")
    else:
        print(f"Article updated: {instance.title}")


# ===== Management Command Example (management/commands/import_articles.py) =====

from django.core.management.base import BaseCommand


class Command(BaseCommand):
    """Custom management command"""
    help = 'Import articles from CSV file'

    def add_arguments(self, parser):
        parser.add_argument('csv_file', type=str, help='Path to CSV file')

    def handle(self, *args, **options):
        csv_file = options['csv_file']
        self.stdout.write(f"Importing from {csv_file}...")

        # Import logic here

        self.stdout.write(self.style.SUCCESS('Successfully imported articles'))


if __name__ == "__main__":
    print("Django Framework Examples")
    print("\nThis file contains code snippets. To use Django:")
    print("\n1. Create project:")
    print("   django-admin startproject myproject")
    print("\n2. Create app:")
    print("   python manage.py startapp myapp")
    print("\n3. Run migrations:")
    print("   python manage.py makemigrations")
    print("   python manage.py migrate")
    print("\n4. Create superuser:")
    print("   python manage.py createsuperuser")
    print("\n5. Run server:")
    print("   python manage.py runserver")
    print("\nFeatures demonstrated:")
    print("  - Models (ORM)")
    print("  - Views (function and class-based)")
    print("  - Forms (model and regular)")
    print("  - URLs routing")
    print("  - Admin interface")
    print("  - Middleware")
    print("  - Signals")
    print("  - Management commands")
