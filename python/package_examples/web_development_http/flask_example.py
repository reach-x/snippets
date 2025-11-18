"""
flask - Micro web framework
Install: pip install flask

Lightweight, flexible web framework for building web applications and APIs
"""

from flask import Flask, request, jsonify, render_template_string, session, redirect, url_for
from functools import wraps

app = Flask(__name__)
app.secret_key = 'your-secret-key-here'  # Required for sessions

# In-memory database for demo
users_database = {}
todos_database = []


# ===== Basic Routes =====

@app.route('/')
def home():
    """Basic route returning HTML"""
    return '<h1>Welcome to Flask!</h1><p>Visit /api/hello for JSON response</p>'


@app.route('/hello/<name>')
def hello_name(name):
    """Route with URL parameter"""
    return f'<h1>Hello, {name}!</h1>'


# ===== JSON API Routes =====

@app.route('/api/hello')
def api_hello():
    """JSON response"""
    return jsonify({
        'message': 'Hello from Flask API',
        'status': 'success'
    })


@app.route('/api/users', methods=['GET'])
def get_users():
    """GET request - list all users"""
    return jsonify({
        'users': list(users_database.values()),
        'count': len(users_database)
    })


@app.route('/api/users', methods=['POST'])
def create_user():
    """POST request - create new user"""
    data = request.get_json()

    if not data or 'name' not in data:
        return jsonify({'error': 'Name is required'}), 400

    user_id = len(users_database) + 1
    user = {
        'id': user_id,
        'name': data['name'],
        'email': data.get('email', '')
    }

    users_database[user_id] = user
    return jsonify(user), 201


@app.route('/api/users/<int:user_id>', methods=['GET'])
def get_user(user_id):
    """GET single user by ID"""
    user = users_database.get(user_id)

    if not user:
        return jsonify({'error': 'User not found'}), 404

    return jsonify(user)


@app.route('/api/users/<int:user_id>', methods=['PUT'])
def update_user(user_id):
    """UPDATE user"""
    user = users_database.get(user_id)

    if not user:
        return jsonify({'error': 'User not found'}), 404

    data = request.get_json()
    user.update({
        'name': data.get('name', user['name']),
        'email': data.get('email', user['email'])
    })

    return jsonify(user)


@app.route('/api/users/<int:user_id>', methods=['DELETE'])
def delete_user(user_id):
    """DELETE user"""
    if user_id not in users_database:
        return jsonify({'error': 'User not found'}), 404

    del users_database[user_id]
    return jsonify({'message': 'User deleted successfully'})


# ===== Query Parameters =====

@app.route('/api/search')
def search():
    """Handle query parameters"""
    query = request.args.get('q', '')
    page = request.args.get('page', 1, type=int)
    limit = request.args.get('limit', 10, type=int)

    return jsonify({
        'query': query,
        'page': page,
        'limit': limit,
        'results': []
    })


# ===== Form Handling =====

@app.route('/form', methods=['GET', 'POST'])
def form_example():
    """Handle form submissions"""
    if request.method == 'POST':
        name = request.form.get('name')
        email = request.form.get('email')

        return jsonify({
            'message': 'Form submitted successfully',
            'data': {'name': name, 'email': email}
        })

    # GET request - show form
    form_html = '''
        <form method="POST">
            <input type="text" name="name" placeholder="Name" required><br>
            <input type="email" name="email" placeholder="Email" required><br>
            <button type="submit">Submit</button>
        </form>
    '''
    return render_template_string(form_html)


# ===== Sessions and Cookies =====

@app.route('/login', methods=['POST'])
def login():
    """Set session data"""
    data = request.get_json()
    session['user_id'] = data.get('user_id')
    session['username'] = data.get('username')

    return jsonify({'message': 'Logged in successfully'})


@app.route('/profile')
def profile():
    """Access session data"""
    if 'user_id' not in session:
        return jsonify({'error': 'Not logged in'}), 401

    return jsonify({
        'user_id': session['user_id'],
        'username': session['username']
    })


@app.route('/logout')
def logout():
    """Clear session"""
    session.clear()
    return jsonify({'message': 'Logged out successfully'})


# ===== Middleware / Decorators =====

def require_api_key(func):
    """Custom decorator to require API key"""
    @wraps(func)
    def wrapper(*args, **kwargs):
        api_key = request.headers.get('X-API-Key')

        if api_key != 'secret-api-key':
            return jsonify({'error': 'Invalid API key'}), 401

        return func(*args, **kwargs)

    return wrapper


@app.route('/api/protected')
@require_api_key
def protected_route():
    """Protected route requiring API key"""
    return jsonify({'message': 'Access granted to protected resource'})


# ===== Error Handlers =====

@app.errorhandler(404)
def not_found(error):
    """Custom 404 handler"""
    return jsonify({'error': 'Resource not found'}), 404


@app.errorhandler(500)
def internal_error(error):
    """Custom 500 handler"""
    return jsonify({'error': 'Internal server error'}), 500


# ===== Before/After Request Hooks =====

@app.before_request
def before_request_func():
    """Runs before each request"""
    print(f"Request: {request.method} {request.path}")


@app.after_request
def after_request_func(response):
    """Runs after each request"""
    response.headers['X-Custom-Header'] = 'Flask App'
    return response


# ===== Blueprints Example =====

from flask import Blueprint

api_blueprint = Blueprint('api_v2', __name__, url_prefix='/api/v2')

@api_blueprint.route('/status')
def api_status():
    """Blueprint route"""
    return jsonify({'status': 'API v2 is running'})

app.register_blueprint(api_blueprint)


# ===== File Upload =====

@app.route('/upload', methods=['POST'])
def upload_file():
    """Handle file upload"""
    if 'file' not in request.files:
        return jsonify({'error': 'No file provided'}), 400

    file = request.files['file']

    if file.filename == '':
        return jsonify({'error': 'No file selected'}), 400

    # Save file
    file.save(f'/tmp/{file.filename}')

    return jsonify({
        'message': 'File uploaded successfully',
        'filename': file.filename
    })


if __name__ == '__main__':
    print("Flask application starting...")
    print("Available routes:")
    print("  GET  /                     - Home page")
    print("  GET  /hello/<name>         - Greeting")
    print("  GET  /api/hello            - JSON API")
    print("  GET  /api/users            - List users")
    print("  POST /api/users            - Create user")
    print("  GET  /api/users/<id>       - Get user")
    print("  PUT  /api/users/<id>       - Update user")
    print("  DELETE /api/users/<id>     - Delete user")
    print("  GET  /api/search?q=...     - Search with query params")
    print("  POST /login                - Login (set session)")
    print("  GET  /profile              - Get profile (requires session)")
    print("  GET  /logout               - Logout (clear session)")
    print("  GET  /api/protected        - Protected (requires X-API-Key header)")
    print("\nRun with: python flask_example.py")
    print("Then visit: http://localhost:5000")

    # Run the app
    # app.run(debug=True, port=5000)
