"""
Generate Python Algorithm Library Reference PDF

Creates a comprehensive reference mapping interview algorithms to Python standard
and third-party libraries.
"""

from reportlab.lib.pagesizes import letter
from reportlab.lib.styles import getSampleStyleSheet, ParagraphStyle
from reportlab.lib.units import inch
from reportlab.platypus import SimpleDocTemplate, Paragraph, Spacer, PageBreak, Table, TableStyle
from reportlab.lib.enums import TA_LEFT, TA_CENTER
from reportlab.lib import colors
from datetime import datetime


def create_algorithm_library_reference():
    """Create PDF reference guide."""

    # Create PDF
    pdf_path = "references/python_algorithm_library_reference.pdf"
    doc = SimpleDocTemplate(
        pdf_path,
        pagesize=letter,
        rightMargin=0.75*inch,
        leftMargin=0.75*inch,
        topMargin=0.75*inch,
        bottomMargin=0.75*inch
    )

    # Container for flowable objects
    story = []

    # Styles
    styles = getSampleStyleSheet()
    title_style = ParagraphStyle(
        'CustomTitle',
        parent=styles['Heading1'],
        fontSize=24,
        textColor=colors.HexColor('#1a1a1a'),
        spaceAfter=30,
        alignment=TA_CENTER
    )

    heading_style = ParagraphStyle(
        'CustomHeading',
        parent=styles['Heading2'],
        fontSize=16,
        textColor=colors.HexColor('#2c3e50'),
        spaceAfter=12,
        spaceBefore=12
    )

    subheading_style = ParagraphStyle(
        'CustomSubHeading',
        parent=styles['Heading3'],
        fontSize=13,
        textColor=colors.HexColor('#34495e'),
        spaceAfter=8,
        spaceBefore=8
    )

    body_style = ParagraphStyle(
        'CustomBody',
        parent=styles['Normal'],
        fontSize=10,
        leading=14
    )

    code_style = ParagraphStyle(
        'Code',
        parent=styles['Code'],
        fontSize=9,
        fontName='Courier',
        textColor=colors.HexColor('#c7254e'),
        backColor=colors.HexColor('#f9f2f4'),
        leftIndent=20,
        rightIndent=20
    )

    # Title
    story.append(Paragraph("Python Algorithm Library Reference", title_style))
    story.append(Paragraph(
        f"Comprehensive Guide to Built-in Algorithm Implementations<br/>Generated: {datetime.now().strftime('%B %d, %Y')}",
        styles['Normal']
    ))
    story.append(Spacer(1, 0.3*inch))

    # Introduction
    story.append(Paragraph("Introduction", heading_style))
    story.append(Paragraph(
        "This reference maps common interview algorithms and data structures to Python's "
        "standard library and popular third-party packages. Instead of implementing from scratch, "
        "leverage these battle-tested, optimized implementations.",
        body_style
    ))
    story.append(Spacer(1, 0.2*inch))

    # ========================================================================
    # STANDARD LIBRARY SECTION
    # ========================================================================

    story.append(Paragraph("Part 1: Python Standard Library", heading_style))
    story.append(Spacer(1, 0.1*inch))

    # Collections module
    story.append(Paragraph("1. collections - High-Performance Data Structures", subheading_style))

    collections_data = [
        ['Data Structure', 'Use Case', 'Key Methods'],
        ['Counter', 'Frequency counting, hash map', 'most_common(), elements()'],
        ['defaultdict', 'Hash map with default values', 'Auto-initialization'],
        ['deque', 'Queue, stack (O(1) both ends)', 'append(), appendleft(), pop(), popleft()'],
        ['OrderedDict', 'Maintain insertion order (3.7+ dict does this)', 'move_to_end(), popitem()'],
        ['ChainMap', 'Multiple dicts as single unit', 'maps, new_child()'],
    ]

    t = Table(collections_data, colWidths=[1.5*inch, 2.2*inch, 2.3*inch])
    t.setStyle(TableStyle([
        ('BACKGROUND', (0, 0), (-1, 0), colors.HexColor('#3498db')),
        ('TEXTCOLOR', (0, 0), (-1, 0), colors.whitesmoke),
        ('ALIGN', (0, 0), (-1, -1), 'LEFT'),
        ('FONTNAME', (0, 0), (-1, 0), 'Helvetica-Bold'),
        ('FONTSIZE', (0, 0), (-1, 0), 10),
        ('BOTTOMPADDING', (0, 0), (-1, 0), 12),
        ('BACKGROUND', (0, 1), (-1, -1), colors.beige),
        ('GRID', (0, 0), (-1, -1), 1, colors.black),
        ('FONTSIZE', (0, 1), (-1, -1), 9),
        ('VALIGN', (0, 0), (-1, -1), 'TOP'),
    ]))
    story.append(t)
    story.append(Spacer(1, 0.15*inch))

    story.append(Paragraph("Example: Counter for frequency counting", body_style))
    story.append(Paragraph(
        "from collections import Counter<br/>"
        "nums = [1, 1, 1, 2, 2, 3]<br/>"
        "freq = Counter(nums)<br/>"
        "top_k = freq.most_common(2)  # [(1, 3), (2, 2)]",
        code_style
    ))
    story.append(Spacer(1, 0.2*inch))

    # heapq module
    story.append(Paragraph("2. heapq - Heap/Priority Queue", subheading_style))

    heapq_data = [
        ['Function', 'Purpose', 'Time Complexity'],
        ['heappush(heap, item)', 'Add item to heap', 'O(log n)'],
        ['heappop(heap)', 'Remove and return smallest', 'O(log n)'],
        ['heapify(list)', 'Convert list to heap in-place', 'O(n)'],
        ['nlargest(n, iterable)', 'Return n largest elements', 'O(n log k)'],
        ['nsmallest(n, iterable)', 'Return n smallest elements', 'O(n log k)'],
        ['heapreplace(heap, item)', 'Pop and push in one operation', 'O(log n)'],
    ]

    t = Table(heapq_data, colWidths=[2.2*inch, 2.5*inch, 1.3*inch])
    t.setStyle(TableStyle([
        ('BACKGROUND', (0, 0), (-1, 0), colors.HexColor('#3498db')),
        ('TEXTCOLOR', (0, 0), (-1, 0), colors.whitesmoke),
        ('ALIGN', (0, 0), (-1, -1), 'LEFT'),
        ('FONTNAME', (0, 0), (-1, 0), 'Helvetica-Bold'),
        ('FONTSIZE', (0, 0), (-1, 0), 10),
        ('BOTTOMPADDING', (0, 0), (-1, 0), 12),
        ('BACKGROUND', (0, 1), (-1, -1), colors.beige),
        ('GRID', (0, 0), (-1, -1), 1, colors.black),
        ('FONTSIZE', (0, 1), (-1, -1), 9),
    ]))
    story.append(t)
    story.append(Spacer(1, 0.15*inch))

    story.append(Paragraph("Note: Python's heapq is a min-heap. For max-heap, negate values.", body_style))
    story.append(Paragraph(
        "import heapq<br/>"
        "# Min heap<br/>"
        "min_heap = [3, 1, 4, 1, 5]<br/>"
        "heapq.heapify(min_heap)<br/><br/>"
        "# Max heap (negate values)<br/>"
        "max_heap = [-x for x in [3, 1, 4, 1, 5]]<br/>"
        "heapq.heapify(max_heap)<br/>"
        "largest = -heapq.heappop(max_heap)",
        code_style
    ))
    story.append(Spacer(1, 0.2*inch))

    # bisect module
    story.append(Paragraph("3. bisect - Binary Search", subheading_style))

    bisect_data = [
        ['Function', 'Purpose', 'Use Case'],
        ['bisect_left(a, x)', 'Leftmost insertion point', 'Find first occurrence'],
        ['bisect_right(a, x)', 'Rightmost insertion point', 'Find last occurrence + 1'],
        ['insort_left(a, x)', 'Insert maintaining order (left)', 'Sorted insertion'],
        ['insort_right(a, x)', 'Insert maintaining order (right)', 'Sorted insertion'],
    ]

    t = Table(bisect_data, colWidths=[2*inch, 2.3*inch, 1.7*inch])
    t.setStyle(TableStyle([
        ('BACKGROUND', (0, 0), (-1, 0), colors.HexColor('#3498db')),
        ('TEXTCOLOR', (0, 0), (-1, 0), colors.whitesmoke),
        ('ALIGN', (0, 0), (-1, -1), 'LEFT'),
        ('FONTNAME', (0, 0), (-1, 0), 'Helvetica-Bold'),
        ('FONTSIZE', (0, 0), (-1, 0), 10),
        ('BOTTOMPADDING', (0, 0), (-1, 0), 12),
        ('BACKGROUND', (0, 1), (-1, -1), colors.beige),
        ('GRID', (0, 0), (-1, -1), 1, colors.black),
        ('FONTSIZE', (0, 1), (-1, -1), 9),
    ]))
    story.append(t)
    story.append(Spacer(1, 0.15*inch))

    story.append(Paragraph(
        "import bisect<br/>"
        "sorted_list = [1, 3, 3, 3, 5, 7]<br/>"
        "# Find first occurrence of 3<br/>"
        "left = bisect.bisect_left(sorted_list, 3)  # 1<br/>"
        "# Find position after last 3<br/>"
        "right = bisect.bisect_right(sorted_list, 3)  # 4",
        code_style
    ))
    story.append(Spacer(1, 0.2*inch))

    # itertools
    story.append(Paragraph("4. itertools - Combinatorics & Iteration", subheading_style))

    itertools_data = [
        ['Function', 'Purpose', 'Example'],
        ['combinations(it, r)', 'All r-length combinations', 'C(n,r) - no repeats'],
        ['combinations_with_replacement', 'Combinations with repeats', 'Choose with replacement'],
        ['permutations(it, r)', 'All r-length permutations', 'P(n,r) - order matters'],
        ['product(*iterables)', 'Cartesian product', 'Nested loops'],
        ['accumulate(it, func)', 'Running totals', 'Prefix sums'],
        ['chain(*iterables)', 'Flatten iterables', 'Concatenate sequences'],
        ['groupby(it, key)', 'Group consecutive elements', 'Group by key function'],
    ]

    t = Table(itertools_data, colWidths=[2.3*inch, 2*inch, 1.7*inch])
    t.setStyle(TableStyle([
        ('BACKGROUND', (0, 0), (-1, 0), colors.HexColor('#3498db')),
        ('TEXTCOLOR', (0, 0), (-1, 0), colors.whitesmoke),
        ('ALIGN', (0, 0), (-1, -1), 'LEFT'),
        ('FONTNAME', (0, 0), (-1, 0), 'Helvetica-Bold'),
        ('FONTSIZE', (0, 0), (-1, 0), 10),
        ('BOTTOMPADDING', (0, 0), (-1, 0), 12),
        ('BACKGROUND', (0, 1), (-1, -1), colors.beige),
        ('GRID', (0, 0), (-1, -1), 1, colors.black),
        ('FONTSIZE', (0, 1), (-1, -1), 9),
    ]))
    story.append(t)

    story.append(PageBreak())

    # functools
    story.append(Paragraph("5. functools - Function Tools", subheading_style))

    functools_data = [
        ['Function', 'Purpose', 'Interview Use'],
        ['lru_cache(maxsize)', 'Memoization decorator', 'DP problems, cache results'],
        ['cache', 'Unlimited memoization (3.9+)', 'Simple DP caching'],
        ['reduce(func, iterable)', 'Reduce to single value', 'Accumulation operations'],
        ['cmp_to_key(func)', 'Convert comparison to key', 'Custom sorting'],
    ]

    t = Table(functools_data, colWidths=[2*inch, 2.3*inch, 1.7*inch])
    t.setStyle(TableStyle([
        ('BACKGROUND', (0, 0), (-1, 0), colors.HexColor('#3498db')),
        ('TEXTCOLOR', (0, 0), (-1, 0), colors.whitesmoke),
        ('ALIGN', (0, 0), (-1, -1), 'LEFT'),
        ('FONTNAME', (0, 0), (-1, 0), 'Helvetica-Bold'),
        ('FONTSIZE', (0, 0), (-1, 0), 10),
        ('BOTTOMPADDING', (0, 0), (-1, 0), 12),
        ('BACKGROUND', (0, 1), (-1, -1), colors.beige),
        ('GRID', (0, 0), (-1, -1), 1, colors.black),
        ('FONTSIZE', (0, 1), (-1, -1), 9),
    ]))
    story.append(t)
    story.append(Spacer(1, 0.15*inch))

    story.append(Paragraph(
        "from functools import lru_cache<br/><br/>"
        "@lru_cache(maxsize=None)<br/>"
        "def fibonacci(n):<br/>"
        "&nbsp;&nbsp;&nbsp;&nbsp;if n &lt;= 1: return n<br/>"
        "&nbsp;&nbsp;&nbsp;&nbsp;return fibonacci(n-1) + fibonacci(n-2)",
        code_style
    ))
    story.append(Spacer(1, 0.2*inch))

    # math module
    story.append(Paragraph("6. math - Mathematical Functions", subheading_style))

    math_data = [
        ['Function', 'Purpose', 'Complexity'],
        ['gcd(a, b)', 'Greatest common divisor', 'O(log min(a,b))'],
        ['lcm(a, b)', 'Least common multiple (3.9+)', 'O(log min(a,b))'],
        ['factorial(n)', 'n! factorial', 'O(n)'],
        ['comb(n, k)', 'Binomial coefficient C(n,k)', 'O(k)'],
        ['perm(n, k)', 'Permutations P(n,k)', 'O(k)'],
        ['isqrt(n)', 'Integer square root', 'O(1)'],
        ['ceil(x), floor(x)', 'Ceiling and floor', 'O(1)'],
    ]

    t = Table(math_data, colWidths=[2*inch, 2.5*inch, 1.5*inch])
    t.setStyle(TableStyle([
        ('BACKGROUND', (0, 0), (-1, 0), colors.HexColor('#3498db')),
        ('TEXTCOLOR', (0, 0), (-1, 0), colors.whitesmoke),
        ('ALIGN', (0, 0), (-1, -1), 'LEFT'),
        ('FONTNAME', (0, 0), (-1, 0), 'Helvetica-Bold'),
        ('FONTSIZE', (0, 0), (-1, 0), 10),
        ('BOTTOMPADDING', (0, 0), (-1, 0), 12),
        ('BACKGROUND', (0, 1), (-1, -1), colors.beige),
        ('GRID', (0, 0), (-1, -1), 1, colors.black),
        ('FONTSIZE', (0, 1), (-1, -1), 9),
    ]))
    story.append(t)
    story.append(Spacer(1, 0.2*inch))

    # ========================================================================
    # ALGORITHM PATTERN MAPPING
    # ========================================================================

    story.append(PageBreak())
    story.append(Paragraph("Part 2: Algorithm Pattern to Library Mapping", heading_style))
    story.append(Spacer(1, 0.1*inch))

    # Arrays and Hashing
    story.append(Paragraph("Arrays & Hash Map Patterns", subheading_style))

    array_patterns = [
        ['Pattern/Problem', 'Python Library Solution', 'Manual Implementation'],
        ['Frequency counting', 'collections.Counter', 'dict with get()'],
        ['Two Sum (sorted)', 'bisect module', 'Two pointers'],
        ['Top K elements', 'heapq.nlargest()', 'Min heap of size k'],
        ['Group anagrams', 'collections.defaultdict', 'dict with sorted key'],
        ['Running sum/prefix', 'itertools.accumulate()', 'Manual accumulation'],
        ['Median of stream', 'Use two heapq heaps', 'Max heap + Min heap'],
    ]

    t = Table(array_patterns, colWidths=[2*inch, 2.2*inch, 1.8*inch])
    t.setStyle(TableStyle([
        ('BACKGROUND', (0, 0), (-1, 0), colors.HexColor('#2ecc71')),
        ('TEXTCOLOR', (0, 0), (-1, 0), colors.whitesmoke),
        ('ALIGN', (0, 0), (-1, -1), 'LEFT'),
        ('FONTNAME', (0, 0), (-1, 0), 'Helvetica-Bold'),
        ('FONTSIZE', (0, 0), (-1, 0), 10),
        ('BOTTOMPADDING', (0, 0), (-1, 0), 12),
        ('BACKGROUND', (0, 1), (-1, -1), colors.lightgreen),
        ('GRID', (0, 0), (-1, -1), 1, colors.black),
        ('FONTSIZE', (0, 1), (-1, -1), 9),
    ]))
    story.append(t)
    story.append(Spacer(1, 0.2*inch))

    # Binary Search
    story.append(Paragraph("Binary Search Patterns", subheading_style))

    binary_patterns = [
        ['Pattern', 'Library Function', 'Notes'],
        ['Find insertion point', 'bisect.bisect_left()', 'O(log n)'],
        ['Find range', 'bisect_left() + bisect_right()', 'First and last occurrence'],
        ['Insert in sorted order', 'bisect.insort()', 'O(n) due to insertion'],
        ['Check if exists', 'index = bisect_left(); check a[index]', 'Then verify'],
    ]

    t = Table(binary_patterns, colWidths=[2*inch, 2.3*inch, 1.7*inch])
    t.setStyle(TableStyle([
        ('BACKGROUND', (0, 0), (-1, 0), colors.HexColor('#2ecc71')),
        ('TEXTCOLOR', (0, 0), (-1, 0), colors.whitesmoke),
        ('ALIGN', (0, 0), (-1, -1), 'LEFT'),
        ('FONTNAME', (0, 0), (-1, 0), 'Helvetica-Bold'),
        ('FONTSIZE', (0, 0), (-1, 0), 10),
        ('BOTTOMPADDING', (0, 0), (-1, 0), 12),
        ('BACKGROUND', (0, 1), (-1, -1), colors.lightgreen),
        ('GRID', (0, 0), (-1, -1), 1, colors.black),
        ('FONTSIZE', (0, 1), (-1, -1), 9),
    ]))
    story.append(t)
    story.append(Spacer(1, 0.2*inch))

    # Dynamic Programming
    story.append(Paragraph("Dynamic Programming Patterns", subheading_style))

    dp_patterns = [
        ['Pattern', 'Library Tool', 'Example'],
        ['Memoization', '@functools.lru_cache', 'Fibonacci, DFS with cache'],
        ['Combinations count', 'math.comb(n, k)', 'Choose k from n'],
        ['Permutations count', 'math.perm(n, k)', 'Arrange k from n'],
        ['All combinations', 'itertools.combinations()', 'Generate all subsets'],
        ['All permutations', 'itertools.permutations()', 'Generate all arrangements'],
    ]

    t = Table(dp_patterns, colWidths=[2*inch, 2*inch, 2*inch])
    t.setStyle(TableStyle([
        ('BACKGROUND', (0, 0), (-1, 0), colors.HexColor('#2ecc71')),
        ('TEXTCOLOR', (0, 0), (-1, 0), colors.whitesmoke),
        ('ALIGN', (0, 0), (-1, -1), 'LEFT'),
        ('FONTNAME', (0, 0), (-1, 0), 'Helvetica-Bold'),
        ('FONTSIZE', (0, 0), (-1, 0), 10),
        ('BOTTOMPADDING', (0, 0), (-1, 0), 12),
        ('BACKGROUND', (0, 1), (-1, -1), colors.lightgreen),
        ('GRID', (0, 0), (-1, -1), 1, colors.black),
        ('FONTSIZE', (0, 1), (-1, -1), 9),
    ]))
    story.append(t)
    story.append(Spacer(1, 0.2*inch))

    # Stacks and Queues
    story.append(Paragraph("Stack & Queue Patterns", subheading_style))

    stack_patterns = [
        ['Data Structure', 'Library Implementation', 'Operations'],
        ['Stack', 'list with append()/pop()', 'O(1) both ends'],
        ['Queue', 'collections.deque', 'O(1) both ends'],
        ['Priority Queue (min)', 'heapq module', 'O(log n) push/pop'],
        ['Priority Queue (max)', 'heapq with negated values', 'O(log n) push/pop'],
        ['Deque (double-ended)', 'collections.deque', 'appendleft(), popleft()'],
    ]

    t = Table(stack_patterns, colWidths=[2*inch, 2.2*inch, 1.8*inch])
    t.setStyle(TableStyle([
        ('BACKGROUND', (0, 0), (-1, 0), colors.HexColor('#2ecc71')),
        ('TEXTCOLOR', (0, 0), (-1, 0), colors.whitesmoke),
        ('ALIGN', (0, 0), (-1, -1), 'LEFT'),
        ('FONTNAME', (0, 0), (-1, 0), 'Helvetica-Bold'),
        ('FONTSIZE', (0, 0), (-1, 0), 10),
        ('BOTTOMPADDING', (0, 0), (-1, 0), 12),
        ('BACKGROUND', (0, 1), (-1, -1), colors.lightgreen),
        ('GRID', (0, 0), (-1, -1), 1, colors.black),
        ('FONTSIZE', (0, 1), (-1, -1), 9),
    ]))
    story.append(t)

    story.append(PageBreak())

    # ========================================================================
    # THIRD PARTY LIBRARIES
    # ========================================================================

    story.append(Paragraph("Part 3: Third-Party Libraries", heading_style))
    story.append(Spacer(1, 0.1*inch))

    # NumPy
    story.append(Paragraph("1. NumPy - Numerical Computing", subheading_style))
    story.append(Paragraph(
        "Install: <b>pip install numpy</b>",
        body_style
    ))
    story.append(Spacer(1, 0.1*inch))

    numpy_features = [
        ['Feature', 'NumPy Function', 'Use Case'],
        ['Array operations', 'np.array(), np.zeros(), np.ones()', 'Efficient arrays'],
        ['Matrix operations', 'np.dot(), np.matmul(), @', 'Linear algebra'],
        ['Sorting', 'np.sort(), np.argsort()', 'O(n log n) optimized'],
        ['Searching', 'np.searchsorted()', 'Binary search on arrays'],
        ['Statistics', 'np.mean(), np.median(), np.std()', 'Quick stats'],
        ['Unique elements', 'np.unique()', 'Deduplicate with counts'],
        ['Argmax/Argmin', 'np.argmax(), np.argmin()', 'Find index of max/min'],
    ]

    t = Table(numpy_features, colWidths=[1.8*inch, 2.3*inch, 1.9*inch])
    t.setStyle(TableStyle([
        ('BACKGROUND', (0, 0), (-1, 0), colors.HexColor('#e74c3c')),
        ('TEXTCOLOR', (0, 0), (-1, 0), colors.whitesmoke),
        ('ALIGN', (0, 0), (-1, -1), 'LEFT'),
        ('FONTNAME', (0, 0), (-1, 0), 'Helvetica-Bold'),
        ('FONTSIZE', (0, 0), (-1, 0), 10),
        ('BOTTOMPADDING', (0, 0), (-1, 0), 12),
        ('BACKGROUND', (0, 1), (-1, -1), colors.lightcoral),
        ('GRID', (0, 0), (-1, -1), 1, colors.black),
        ('FONTSIZE', (0, 1), (-1, -1), 9),
    ]))
    story.append(t)
    story.append(Spacer(1, 0.2*inch))

    # NetworkX
    story.append(Paragraph("2. NetworkX - Graph Algorithms", subheading_style))
    story.append(Paragraph(
        "Install: <b>pip install networkx</b>",
        body_style
    ))
    story.append(Spacer(1, 0.1*inch))

    networkx_features = [
        ['Algorithm', 'NetworkX Function', 'Purpose'],
        ['Shortest path', 'nx.shortest_path()', 'BFS/Dijkstra'],
        ['All shortest paths', 'nx.all_shortest_paths()', 'Find all paths'],
        ['Cycle detection', 'nx.find_cycle()', 'Detect cycles'],
        ['Topological sort', 'nx.topological_sort()', 'DAG ordering'],
        ['Connected components', 'nx.connected_components()', 'Find components'],
        ['Minimum spanning tree', 'nx.minimum_spanning_tree()', "Kruskal's/Prim's"],
        ['Graph coloring', 'nx.greedy_color()', 'Vertex coloring'],
        ['Centrality', 'nx.betweenness_centrality()', 'Important nodes'],
    ]

    t = Table(networkx_features, colWidths=[2*inch, 2.2*inch, 1.8*inch])
    t.setStyle(TableStyle([
        ('BACKGROUND', (0, 0), (-1, 0), colors.HexColor('#e74c3c')),
        ('TEXTCOLOR', (0, 0), (-1, 0), colors.whitesmoke),
        ('ALIGN', (0, 0), (-1, -1), 'LEFT'),
        ('FONTNAME', (0, 0), (-1, 0), 'Helvetica-Bold'),
        ('FONTSIZE', (0, 0), (-1, 0), 10),
        ('BOTTOMPADDING', (0, 0), (-1, 0), 12),
        ('BACKGROUND', (0, 1), (-1, -1), colors.lightcoral),
        ('GRID', (0, 0), (-1, -1), 1, colors.black),
        ('FONTSIZE', (0, 1), (-1, -1), 9),
    ]))
    story.append(t)
    story.append(Spacer(1, 0.2*inch))

    # SciPy
    story.append(Paragraph("3. SciPy - Scientific Computing", subheading_style))
    story.append(Paragraph(
        "Install: <b>pip install scipy</b>",
        body_style
    ))
    story.append(Spacer(1, 0.1*inch))

    scipy_features = [
        ['Module', 'Key Functions', 'Algorithm'],
        ['scipy.sparse', 'Sparse matrices', 'Memory-efficient matrices'],
        ['scipy.spatial', 'KDTree, distance metrics', 'Nearest neighbors'],
        ['scipy.optimize', 'minimize(), linear_sum_assignment()', 'Optimization'],
        ['scipy.sparse.csgraph', 'shortest_path(), dijkstra()', 'Graph algorithms'],
        ['scipy.special', 'comb(), perm()', 'Combinatorics'],
    ]

    t = Table(scipy_features, colWidths=[2*inch, 2.3*inch, 1.7*inch])
    t.setStyle(TableStyle([
        ('BACKGROUND', (0, 0), (-1, 0), colors.HexColor('#e74c3c')),
        ('TEXTCOLOR', (0, 0), (-1, 0), colors.whitesmoke),
        ('ALIGN', (0, 0), (-1, -1), 'LEFT'),
        ('FONTNAME', (0, 0), (-1, 0), 'Helvetica-Bold'),
        ('FONTSIZE', (0, 0), (-1, 0), 10),
        ('BOTTOMPADDING', (0, 0), (-1, 0), 12),
        ('BACKGROUND', (0, 1), (-1, -1), colors.lightcoral),
        ('GRID', (0, 0), (-1, -1), 1, colors.black),
        ('FONTSIZE', (0, 1), (-1, -1), 9),
    ]))
    story.append(t)

    story.append(PageBreak())

    # ========================================================================
    # QUICK REFERENCE
    # ========================================================================

    story.append(Paragraph("Part 4: Quick Problem-to-Library Mapping", heading_style))
    story.append(Spacer(1, 0.1*inch))

    quick_ref = [
        ['Interview Problem', 'Python Library Solution'],
        ['Two Sum (sorted array)', 'Use bisect.bisect_left() for each element'],
        ['Top K Frequent Elements', 'collections.Counter + heapq.nlargest()'],
        ['Group Anagrams', 'collections.defaultdict with sorted string key'],
        ['Valid Anagram', 'collections.Counter comparison'],
        ['Merge K Sorted Lists', 'heapq.merge(*lists)'],
        ['Find Median from Stream', 'Two heaps: heapq for min, negate for max'],
        ['Course Schedule', 'Manual (or use networkx.topological_sort)'],
        ['Number of Islands', 'Manual BFS/DFS (or networkx for graphs)'],
        ['Word Ladder', 'collections.deque for BFS'],
        ['LRU Cache', '@functools.lru_cache or manual OrderedDict'],
        ['Binary Search', 'bisect.bisect_left() / bisect_right()'],
        ['Permutations', 'itertools.permutations()'],
        ['Combinations', 'itertools.combinations()'],
        ['Subsets (Power Set)', 'itertools.combinations for each size'],
        ['Coin Change (DP)', '@functools.lru_cache for memoization'],
        ['Climbing Stairs', '@functools.lru_cache for memoization'],
        ['GCD / LCM', 'math.gcd() / math.lcm()'],
        ['Factorial', 'math.factorial()'],
        ['Prefix Sum', 'itertools.accumulate()'],
    ]

    t = Table(quick_ref, colWidths=[3*inch, 3*inch])
    t.setStyle(TableStyle([
        ('BACKGROUND', (0, 0), (-1, 0), colors.HexColor('#9b59b6')),
        ('TEXTCOLOR', (0, 0), (-1, 0), colors.whitesmoke),
        ('ALIGN', (0, 0), (-1, -1), 'LEFT'),
        ('FONTNAME', (0, 0), (-1, 0), 'Helvetica-Bold'),
        ('FONTSIZE', (0, 0), (-1, 0), 10),
        ('BOTTOMPADDING', (0, 0), (-1, 0), 12),
        ('BACKGROUND', (0, 1), (-1, -1), colors.lavender),
        ('GRID', (0, 0), (-1, -1), 1, colors.black),
        ('FONTSIZE', (0, 1), (-1, -1), 9),
        ('VALIGN', (0, 0), (-1, -1), 'TOP'),
    ]))
    story.append(t)

    story.append(PageBreak())

    # ========================================================================
    # CODE EXAMPLES
    # ========================================================================

    story.append(Paragraph("Part 5: Complete Code Examples", heading_style))
    story.append(Spacer(1, 0.1*inch))

    story.append(Paragraph("Example 1: Top K Frequent Elements", subheading_style))
    story.append(Paragraph(
        "from collections import Counter<br/>"
        "import heapq<br/><br/>"
        "def top_k_frequent(nums, k):<br/>"
        "&nbsp;&nbsp;&nbsp;&nbsp;# Count frequencies<br/>"
        "&nbsp;&nbsp;&nbsp;&nbsp;freq = Counter(nums)<br/>"
        "&nbsp;&nbsp;&nbsp;&nbsp;# Get k most common<br/>"
        "&nbsp;&nbsp;&nbsp;&nbsp;return [num for num, count in freq.most_common(k)]<br/><br/>"
        "# Alternative with heapq<br/>"
        "def top_k_heap(nums, k):<br/>"
        "&nbsp;&nbsp;&nbsp;&nbsp;freq = Counter(nums)<br/>"
        "&nbsp;&nbsp;&nbsp;&nbsp;return heapq.nlargest(k, freq.keys(), key=freq.get)",
        code_style
    ))
    story.append(Spacer(1, 0.2*inch))

    story.append(Paragraph("Example 2: Binary Search with bisect", subheading_style))
    story.append(Paragraph(
        "import bisect<br/><br/>"
        "def search_range(nums, target):<br/>"
        "&nbsp;&nbsp;&nbsp;&nbsp;\"\"\"Find first and last position of target\"\"\"<br/>"
        "&nbsp;&nbsp;&nbsp;&nbsp;left = bisect.bisect_left(nums, target)<br/>"
        "&nbsp;&nbsp;&nbsp;&nbsp;right = bisect.bisect_right(nums, target) - 1<br/>"
        "&nbsp;&nbsp;&nbsp;&nbsp;<br/>"
        "&nbsp;&nbsp;&nbsp;&nbsp;if left &lt;= right:<br/>"
        "&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;return [left, right]<br/>"
        "&nbsp;&nbsp;&nbsp;&nbsp;return [-1, -1]",
        code_style
    ))
    story.append(Spacer(1, 0.2*inch))

    story.append(Paragraph("Example 3: Memoization with functools", subheading_style))
    story.append(Paragraph(
        "from functools import lru_cache<br/><br/>"
        "@lru_cache(maxsize=None)<br/>"
        "def coin_change(amount, coins):<br/>"
        "&nbsp;&nbsp;&nbsp;&nbsp;\"\"\"Min coins to make amount\"\"\"<br/>"
        "&nbsp;&nbsp;&nbsp;&nbsp;if amount == 0: return 0<br/>"
        "&nbsp;&nbsp;&nbsp;&nbsp;if amount &lt; 0: return float('inf')<br/>"
        "&nbsp;&nbsp;&nbsp;&nbsp;<br/>"
        "&nbsp;&nbsp;&nbsp;&nbsp;min_coins = float('inf')<br/>"
        "&nbsp;&nbsp;&nbsp;&nbsp;for coin in coins:<br/>"
        "&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;result = coin_change(amount - coin, coins)<br/>"
        "&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;min_coins = min(min_coins, result + 1)<br/>"
        "&nbsp;&nbsp;&nbsp;&nbsp;return min_coins<br/><br/>"
        "# Note: Convert list to tuple for hashability<br/>"
        "coin_change(11, tuple([1, 2, 5]))",
        code_style
    ))
    story.append(Spacer(1, 0.2*inch))

    story.append(Paragraph("Example 4: Graph with NetworkX", subheading_style))
    story.append(Paragraph(
        "import networkx as nx<br/><br/>"
        "# Create graph<br/>"
        "G = nx.DiGraph()<br/>"
        "G.add_edges_from([(0, 1), (0, 2), (1, 3), (2, 3)])<br/><br/>"
        "# Topological sort (Course Schedule)<br/>"
        "try:<br/>"
        "&nbsp;&nbsp;&nbsp;&nbsp;order = list(nx.topological_sort(G))<br/>"
        "&nbsp;&nbsp;&nbsp;&nbsp;print(f'Valid order: {order}')<br/>"
        "except nx.NetworkXError:<br/>"
        "&nbsp;&nbsp;&nbsp;&nbsp;print('Cycle detected!')<br/><br/>"
        "# Shortest path<br/>"
        "path = nx.shortest_path(G, source=0, target=3)<br/>"
        "print(f'Shortest path: {path}')",
        code_style
    ))

    story.append(PageBreak())

    # ========================================================================
    # BEST PRACTICES
    # ========================================================================

    story.append(Paragraph("Part 6: Interview Best Practices", heading_style))
    story.append(Spacer(1, 0.1*inch))

    story.append(Paragraph("When to Use Libraries vs Manual Implementation", subheading_style))

    best_practices = [
        ['Scenario', 'Recommendation', 'Reason'],
        ['Phone/initial screen', 'Ask if libraries allowed', 'Some allow, some don\'t'],
        ['Take-home assignment', 'Use libraries liberally', 'Shows practical knowledge'],
        ['Onsite whiteboard', 'Usually manual', 'Tests algorithm knowledge'],
        ['Pair programming', 'Mix: use for utilities', 'Balance practicality and knowledge'],
        ['LeetCode practice', 'Both: manual first, then lib', 'Learn both approaches'],
        ['Production code', 'Always use libraries', 'Battle-tested, optimized'],
    ]

    t = Table(best_practices, colWidths=[2*inch, 2.2*inch, 1.8*inch])
    t.setStyle(TableStyle([
        ('BACKGROUND', (0, 0), (-1, 0), colors.HexColor('#f39c12')),
        ('TEXTCOLOR', (0, 0), (-1, 0), colors.whitesmoke),
        ('ALIGN', (0, 0), (-1, -1), 'LEFT'),
        ('FONTNAME', (0, 0), (-1, 0), 'Helvetica-Bold'),
        ('FONTSIZE', (0, 0), (-1, 0), 10),
        ('BOTTOMPADDING', (0, 0), (-1, 0), 12),
        ('BACKGROUND', (0, 1), (-1, -1), colors.bisque),
        ('GRID', (0, 0), (-1, -1), 1, colors.black),
        ('FONTSIZE', (0, 1), (-1, -1), 9),
        ('VALIGN', (0, 0), (-1, -1), 'TOP'),
    ]))
    story.append(t)
    story.append(Spacer(1, 0.2*inch))

    story.append(Paragraph("Key Takeaways", subheading_style))
    story.append(Paragraph(
        "1. <b>Know what exists</b>: Awareness of library functions shows maturity<br/>"
        "2. <b>Understand the algorithm</b>: Even if using a library, explain how it works<br/>"
        "3. <b>Discuss tradeoffs</b>: Library vs manual, time vs space, readability vs performance<br/>"
        "4. <b>Ask first</b>: 'Can I use Python's heapq module for this?' shows good communication<br/>"
        "5. <b>Be prepared for both</b>: Practice manual implementation even if libraries exist<br/>"
        "6. <b>Production mindset</b>: In real code, use libraries (don't reinvent the wheel)<br/>"
        "7. <b>Time complexity still matters</b>: Know the complexity of library functions",
        body_style
    ))
    story.append(Spacer(1, 0.2*inch))

    story.append(Paragraph("Recommended Study Approach", subheading_style))
    story.append(Paragraph(
        "1. <b>First pass</b>: Implement algorithm manually to understand it<br/>"
        "2. <b>Second pass</b>: Refactor using appropriate library functions<br/>"
        "3. <b>Compare</b>: Note differences in code length, readability, performance<br/>"
        "4. <b>Memorize key libraries</b>: collections, heapq, bisect, itertools, functools<br/>"
        "5. <b>Practice both ways</b>: Be fluent in manual and library approaches",
        body_style
    ))
    story.append(Spacer(1, 0.3*inch))

    # Footer
    story.append(Paragraph(
        "<br/><br/>"
        "────────────────────────────────────────────────────<br/>"
        "<i>This reference was generated as a companion to interview preparation scripts.</i><br/>"
        "<i>For the complete collection of interview problems and patterns, see the scripts/ directory.</i>",
        styles['Normal']
    ))

    # Build PDF
    doc.build(story)
    print(f"✅ PDF generated: {pdf_path}")
    return pdf_path


if __name__ == "__main__":
    try:
        pdf_path = create_algorithm_library_reference()
        print(f"\nSuccess! Open the PDF at: {pdf_path}")
    except Exception as e:
        print(f"Error generating PDF: {e}")
        import traceback
        traceback.print_exc()
