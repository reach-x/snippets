"""
matplotlib - Data visualization and plotting
Install: pip install matplotlib

Comprehensive 2D plotting library for creating static, animated, and interactive visualizations
"""

import matplotlib.pyplot as plt
import numpy as np


def line_plots():
    """Create basic line plots"""
    # Sample data
    x = np.linspace(0, 10, 100)
    y1 = np.sin(x)
    y2 = np.cos(x)
    y3 = x ** 0.5

    # Single line plot
    fig, ax = plt.subplots()
    ax.plot(x, y1, label='sin(x)', linewidth=2)
    ax.set_xlabel('X-axis')
    ax.set_ylabel('Y-axis')
    ax.set_title('Simple Line Plot')
    ax.legend()
    ax.grid(True, alpha=0.3)
    # plt.show()  # Uncomment to display

    # Multiple lines
    fig, ax = plt.subplots()
    ax.plot(x, y1, label='sin(x)', linewidth=2)
    ax.plot(x, y2, label='cos(x)', linewidth=2)
    ax.plot(x, y3, label='sqrt(x)', linewidth=2)
    ax.set_xlabel('X-axis')
    ax.set_ylabel('Y-axis')
    ax.set_title('Multiple Line Plots')
    ax.legend()
    ax.grid(True, alpha=0.3)
    # plt.show()  # Uncomment to display

    # Line styles and markers
    fig, ax = plt.subplots()
    ax.plot(x, y1, linestyle='-', marker='o', label='solid with circles')
    ax.plot(x, y2, linestyle='--', marker='s', label='dashed with squares')
    ax.plot(x, y3, linestyle=':', marker='^', label='dotted with triangles')
    ax.set_title('Different Line Styles and Markers')
    ax.legend()
    ax.grid(True, alpha=0.3)
    # plt.show()  # Uncomment to display


def scatter_plots():
    """Create scatter plots"""
    # Basic scatter plot
    np.random.seed(42)
    x = np.random.randn(100)
    y = np.random.randn(100)
    colors = np.random.rand(100)
    sizes = np.random.randint(20, 200, 100)

    fig, ax = plt.subplots()
    scatter = ax.scatter(x, y, c=colors, s=sizes, alpha=0.6, cmap='viridis', edgecolors='black')
    ax.set_xlabel('X-axis')
    ax.set_ylabel('Y-axis')
    ax.set_title('Scatter Plot with Color and Size')
    plt.colorbar(scatter, ax=ax, label='Color values')
    # plt.show()  # Uncomment to display

    # Scatter plot with different categories
    fig, ax = plt.subplots()
    categories = np.random.choice(['A', 'B', 'C'], 100)
    colors_map = {'A': 'red', 'B': 'blue', 'C': 'green'}
    for category in ['A', 'B', 'C']:
        mask = categories == category
        ax.scatter(x[mask], y[mask], label=category, s=100, alpha=0.6, color=colors_map[category])
    ax.set_title('Scatter Plot with Categories')
    ax.legend()
    ax.grid(True, alpha=0.3)
    # plt.show()  # Uncomment to display


def bar_charts():
    """Create bar charts"""
    # Basic bar chart
    categories = ['A', 'B', 'C', 'D', 'E']
    values = [10, 24, 36, 18, 28]

    fig, ax = plt.subplots()
    bars = ax.bar(categories, values, color='steelblue', edgecolor='black')
    ax.set_ylabel('Values')
    ax.set_title('Basic Bar Chart')
    ax.set_ylim(0, max(values) * 1.1)
    # Add value labels on bars
    for bar in bars:
        height = bar.get_height()
        ax.text(bar.get_x() + bar.get_width()/2., height,
                f'{int(height)}', ha='center', va='bottom')
    # plt.show()  # Uncomment to display

    # Horizontal bar chart
    fig, ax = plt.subplots()
    ax.barh(categories, values, color=['red', 'blue', 'green', 'orange', 'purple'], edgecolor='black')
    ax.set_xlabel('Values')
    ax.set_title('Horizontal Bar Chart')
    ax.set_xlim(0, max(values) * 1.1)
    # plt.show()  # Uncomment to display

    # Grouped bar chart
    fig, ax = plt.subplots()
    x_pos = np.arange(len(categories))
    values1 = [10, 24, 36, 18, 28]
    values2 = [15, 20, 32, 22, 25]
    width = 0.35
    ax.bar(x_pos - width/2, values1, width, label='Group 1', color='steelblue')
    ax.bar(x_pos + width/2, values2, width, label='Group 2', color='coral')
    ax.set_ylabel('Values')
    ax.set_title('Grouped Bar Chart')
    ax.set_xticks(x_pos)
    ax.set_xticklabels(categories)
    ax.legend()
    # plt.show()  # Uncomment to display

    # Stacked bar chart
    fig, ax = plt.subplots()
    ax.bar(categories, values1, label='Group 1', color='steelblue')
    ax.bar(categories, values2, bottom=values1, label='Group 2', color='coral')
    ax.set_ylabel('Values')
    ax.set_title('Stacked Bar Chart')
    ax.legend()
    # plt.show()  # Uncomment to display


def histograms():
    """Create histograms"""
    # Generate sample data
    data = np.random.normal(loc=100, scale=15, size=1000)

    # Basic histogram
    fig, ax = plt.subplots()
    ax.hist(data, bins=30, color='steelblue', edgecolor='black', alpha=0.7)
    ax.set_xlabel('Values')
    ax.set_ylabel('Frequency')
    ax.set_title('Basic Histogram')
    ax.grid(True, alpha=0.3, axis='y')
    # plt.show()  # Uncomment to display

    # Multiple histograms
    fig, ax = plt.subplots()
    data1 = np.random.normal(loc=100, scale=15, size=500)
    data2 = np.random.normal(loc=110, scale=20, size=500)
    ax.hist(data1, bins=20, alpha=0.6, label='Distribution 1', color='steelblue')
    ax.hist(data2, bins=20, alpha=0.6, label='Distribution 2', color='coral')
    ax.set_xlabel('Values')
    ax.set_ylabel('Frequency')
    ax.set_title('Multiple Histograms')
    ax.legend()
    # plt.show()  # Uncomment to display

    # Histogram with custom bins
    fig, ax = plt.subplots()
    bins = [70, 80, 90, 100, 110, 120, 130]
    ax.hist(data, bins=bins, color='green', edgecolor='black', alpha=0.7)
    ax.set_xlabel('Values')
    ax.set_ylabel('Frequency')
    ax.set_title('Histogram with Custom Bins')
    # plt.show()  # Uncomment to display


def subplots():
    """Create multiple subplots"""
    x = np.linspace(0, 10, 100)
    y1 = np.sin(x)
    y2 = np.cos(x)
    y3 = np.tan(x)
    y4 = x ** 2

    # 2x2 subplots
    fig, axes = plt.subplots(2, 2, figsize=(10, 8))

    axes[0, 0].plot(x, y1, color='red', linewidth=2)
    axes[0, 0].set_title('sin(x)')
    axes[0, 0].grid(True, alpha=0.3)

    axes[0, 1].plot(x, y2, color='blue', linewidth=2)
    axes[0, 1].set_title('cos(x)')
    axes[0, 1].grid(True, alpha=0.3)

    axes[1, 0].plot(x, y4, color='green', linewidth=2)
    axes[1, 0].set_title('x^2')
    axes[1, 0].grid(True, alpha=0.3)

    axes[1, 1].scatter(x, y1, alpha=0.6, s=50)
    axes[1, 1].set_title('sin(x) - Scatter')
    axes[1, 1].grid(True, alpha=0.3)

    fig.suptitle('2x2 Subplots Example', fontsize=16, fontweight='bold')
    plt.tight_layout()
    # plt.show()  # Uncomment to display

    # 1x3 subplots
    fig, axes = plt.subplots(1, 3, figsize=(15, 4))

    axes[0].plot(x, y1, color='purple')
    axes[0].set_title('sin(x)')
    axes[0].grid(True, alpha=0.3)

    axes[1].plot(x, y2, color='orange')
    axes[1].set_title('cos(x)')
    axes[1].grid(True, alpha=0.3)

    axes[2].plot(x, y4, color='brown')
    axes[2].set_title('x^2')
    axes[2].grid(True, alpha=0.3)

    fig.suptitle('1x3 Subplots Example', fontsize=16, fontweight='bold')
    plt.tight_layout()
    # plt.show()  # Uncomment to display


def customization_colors_labels():
    """Customize plots with colors, labels, titles, and legends"""
    x = np.linspace(0, 10, 100)

    fig, ax = plt.subplots(figsize=(10, 6))

    # Plot with custom colors and line properties
    ax.plot(x, np.sin(x), color='#FF6B6B', linewidth=3, label='sin(x)', alpha=0.8)
    ax.plot(x, np.cos(x), color='#4ECDC4', linewidth=2.5, linestyle='--', label='cos(x)', alpha=0.8)
    ax.plot(x, np.tan(x), color='#95E1D3', linewidth=2, linestyle=':', label='tan(x)', alpha=0.8)

    # Title and labels
    ax.set_title('Trigonometric Functions', fontsize=18, fontweight='bold', color='#333333')
    ax.set_xlabel('X-axis', fontsize=14, fontweight='bold')
    ax.set_ylabel('Y-axis', fontsize=14, fontweight='bold')

    # Legend customization
    ax.legend(loc='upper left', fontsize=12, framealpha=0.9, shadow=True)

    # Grid customization
    ax.grid(True, alpha=0.3, linestyle='--', linewidth=0.5)

    # Axes limits
    ax.set_xlim(0, 10)
    ax.set_ylim(-2, 2)

    # Tick customization
    ax.tick_params(axis='both', labelsize=11, colors='#333333')

    # Spine customization
    ax.spines['top'].set_visible(False)
    ax.spines['right'].set_visible(False)

    # plt.show()  # Uncomment to display


def pie_charts():
    """Create pie charts"""
    # Basic pie chart
    labels = ['Python', 'JavaScript', 'Java', 'C++', 'Ruby']
    sizes = [35, 25, 20, 15, 5]
    colors = ['#FF6B6B', '#4ECDC4', '#95E1D3', '#FFA07A', '#DDA0DD']
    explode = (0.1, 0, 0, 0, 0)  # Explode first slice

    fig, ax = plt.subplots(figsize=(8, 8))
    wedges, texts, autotexts = ax.pie(sizes, labels=labels, colors=colors, autopct='%1.1f%%',
                                        startangle=90, explode=explode, textprops={'fontsize': 11})
    ax.set_title('Programming Languages Usage', fontsize=14, fontweight='bold')

    # Format percentage text
    for autotext in autotexts:
        autotext.set_color('white')
        autotext.set_fontweight('bold')
        autotext.set_fontsize(10)

    # plt.show()  # Uncomment to display

    # Donut chart
    fig, ax = plt.subplots(figsize=(8, 8))
    wedges, texts, autotexts = ax.pie(sizes, labels=labels, colors=colors, autopct='%1.1f%%',
                                        startangle=90, textprops={'fontsize': 11})
    # Draw circle for donut effect
    circle = plt.Circle((0, 0), 0.70, fc='white')
    ax.add_artist(circle)
    ax.set_title('Programming Languages Usage - Donut', fontsize=14, fontweight='bold')

    # plt.show()  # Uncomment to display


def box_plots():
    """Create box plots"""
    # Generate sample data
    data1 = np.random.normal(100, 15, 100)
    data2 = np.random.normal(110, 20, 100)
    data3 = np.random.normal(95, 10, 100)
    data_list = [data1, data2, data3]

    fig, ax = plt.subplots(figsize=(10, 6))
    bp = ax.boxplot(data_list, labels=['Dataset 1', 'Dataset 2', 'Dataset 3'],
                     patch_artist=True, notch=True)

    # Customize box colors
    colors = ['#FF6B6B', '#4ECDC4', '#95E1D3']
    for patch, color in zip(bp['boxes'], colors):
        patch.set_facecolor(color)
        patch.set_alpha(0.7)

    ax.set_ylabel('Values')
    ax.set_title('Box Plots Comparison')
    ax.grid(True, alpha=0.3, axis='y')

    # plt.show()  # Uncomment to display


def heatmap():
    """Create heatmaps"""
    # Generate random data
    data = np.random.rand(10, 10)

    fig, ax = plt.subplots(figsize=(8, 6))
    im = ax.imshow(data, cmap='viridis', aspect='auto', interpolation='nearest')
    ax.set_title('Heatmap Example')
    ax.set_xlabel('X-axis')
    ax.set_ylabel('Y-axis')

    # Add colorbar
    cbar = plt.colorbar(im, ax=ax)
    cbar.set_label('Values', rotation=270, labelpad=20)

    # plt.show()  # Uncomment to display


def saving_figures():
    """Save figures to files"""
    x = np.linspace(0, 10, 100)
    y = np.sin(x)

    fig, ax = plt.subplots(figsize=(10, 6))
    ax.plot(x, y, linewidth=2, color='steelblue')
    ax.set_title('Plot to be Saved')
    ax.set_xlabel('X-axis')
    ax.set_ylabel('Y-axis')
    ax.grid(True, alpha=0.3)

    # Save as PNG (raster format)
    fig.savefig('/tmp/matplotlib_plot.png', dpi=300, bbox_inches='tight')
    print("Saved: /tmp/matplotlib_plot.png")

    # Save as PDF (vector format)
    fig.savefig('/tmp/matplotlib_plot.pdf', bbox_inches='tight')
    print("Saved: /tmp/matplotlib_plot.pdf")

    # Save as SVG (vector format)
    fig.savefig('/tmp/matplotlib_plot.svg', bbox_inches='tight')
    print("Saved: /tmp/matplotlib_plot.svg")

    plt.close(fig)


def styling():
    """Apply different styles to plots"""
    x = np.linspace(0, 10, 100)

    # Available styles can be checked with: plt.style.available

    # Style 1: Default style
    fig, ax = plt.subplots(figsize=(10, 6))
    ax.plot(x, np.sin(x), linewidth=2, label='sin(x)')
    ax.plot(x, np.cos(x), linewidth=2, label='cos(x)')
    ax.set_title('Default Style')
    ax.legend()
    ax.grid(True, alpha=0.3)
    # plt.show()  # Uncomment to display

    # Style 2: Using seaborn-style theme
    with plt.style.context('seaborn-v0_8-darkgrid'):
        fig, ax = plt.subplots(figsize=(10, 6))
        ax.plot(x, np.sin(x), linewidth=2, label='sin(x)')
        ax.plot(x, np.cos(x), linewidth=2, label='cos(x)')
        ax.set_title('Seaborn Style')
        ax.legend()
        # plt.show()  # Uncomment to display

    # Style 3: Minimal style
    with plt.style.context('_mpl-gallery-nogrid'):
        fig, ax = plt.subplots(figsize=(10, 6))
        ax.plot(x, np.sin(x), linewidth=2, label='sin(x)')
        ax.plot(x, np.cos(x), linewidth=2, label='cos(x)')
        ax.set_title('Minimal Style')
        ax.legend()
        # plt.show()  # Uncomment to display


def multiple_plots_advanced():
    """Advanced multiple plots with different types"""
    # Create figure with subplots of different sizes
    fig = plt.figure(figsize=(14, 8))
    gs = fig.add_gridspec(3, 3, hspace=0.3, wspace=0.3)

    # Generate sample data
    x = np.linspace(0, 10, 100)
    categories = ['A', 'B', 'C', 'D', 'E']
    values = [10, 24, 36, 18, 28]

    # Line plot (top left, spanning 2x2)
    ax1 = fig.add_subplot(gs[0:2, 0:2])
    ax1.plot(x, np.sin(x), label='sin(x)', linewidth=2)
    ax1.plot(x, np.cos(x), label='cos(x)', linewidth=2)
    ax1.set_title('Line Plots')
    ax1.legend()
    ax1.grid(True, alpha=0.3)

    # Scatter plot (top right)
    ax2 = fig.add_subplot(gs[0, 2])
    x_scatter = np.random.randn(50)
    y_scatter = np.random.randn(50)
    ax2.scatter(x_scatter, y_scatter, alpha=0.6, s=100)
    ax2.set_title('Scatter Plot')
    ax2.grid(True, alpha=0.3)

    # Histogram (middle right)
    ax3 = fig.add_subplot(gs[1, 2])
    data = np.random.normal(loc=100, scale=15, size=500)
    ax3.hist(data, bins=20, color='steelblue', edgecolor='black')
    ax3.set_title('Histogram')

    # Bar chart (bottom left)
    ax4 = fig.add_subplot(gs[2, 0])
    ax4.bar(categories, values, color='coral', edgecolor='black')
    ax4.set_title('Bar Chart')

    # Pie chart (bottom middle)
    ax5 = fig.add_subplot(gs[2, 1])
    ax5.pie(values, labels=categories, autopct='%1.1f%%')
    ax5.set_title('Pie Chart')

    # Box plot (bottom right)
    ax6 = fig.add_subplot(gs[2, 2])
    data_list = [np.random.normal(100, 15, 100), np.random.normal(110, 20, 100)]
    ax6.boxplot(data_list, labels=['Data1', 'Data2'])
    ax6.set_title('Box Plot')

    fig.suptitle('Advanced Multiple Plots', fontsize=16, fontweight='bold')
    # plt.show()  # Uncomment to display


def colormaps():
    """Demonstrate different colormaps"""
    # Create sample data
    x = np.linspace(-5, 5, 100)
    y = np.linspace(-5, 5, 100)
    X, Y = np.meshgrid(x, y)
    Z = np.sin(np.sqrt(X**2 + Y**2))

    colormaps = ['viridis', 'plasma', 'coolwarm', 'RdYlBu', 'Spectral']

    fig, axes = plt.subplots(2, 3, figsize=(15, 10))
    axes = axes.flatten()

    for idx, cmap_name in enumerate(colormaps):
        im = axes[idx].contourf(X, Y, Z, levels=20, cmap=cmap_name)
        axes[idx].set_title(f'Colormap: {cmap_name}')
        plt.colorbar(im, ax=axes[idx])

    # Remove extra subplot
    fig.delaxes(axes[-1])

    fig.suptitle('Different Colormaps', fontsize=16, fontweight='bold')
    plt.tight_layout()
    # plt.show()  # Uncomment to display


def annotations():
    """Add annotations to plots"""
    x = np.linspace(0, 10, 100)
    y = np.sin(x)

    fig, ax = plt.subplots(figsize=(10, 6))
    ax.plot(x, y, linewidth=2, color='steelblue', label='sin(x)')

    # Annotate specific points
    ax.annotate('Peak', xy=(np.pi/2, 1), xytext=(np.pi/2 + 1, 1.3),
                arrowprops=dict(arrowstyle='->', color='red', lw=2),
                fontsize=12, color='red', fontweight='bold')

    ax.annotate('Zero crossing', xy=(np.pi, 0), xytext=(np.pi + 0.5, -0.5),
                arrowprops=dict(arrowstyle='->', color='blue', lw=2),
                fontsize=12, color='blue', fontweight='bold')

    # Add text box
    textstr = 'Sine wave: y = sin(x)'
    props = dict(boxstyle='round', facecolor='wheat', alpha=0.5)
    ax.text(0.05, 0.95, textstr, transform=ax.transAxes, fontsize=12,
            verticalalignment='top', bbox=props)

    ax.set_title('Plot with Annotations')
    ax.set_xlabel('X-axis')
    ax.set_ylabel('Y-axis')
    ax.grid(True, alpha=0.3)
    ax.legend()

    # plt.show()  # Uncomment to display


if __name__ == "__main__":
    print("=== Matplotlib Examples ===\n")

    print("1. Line Plots")
    line_plots()

    print("\n2. Scatter Plots")
    scatter_plots()

    print("\n3. Bar Charts")
    bar_charts()

    print("\n4. Histograms")
    histograms()

    print("\n5. Subplots")
    subplots()

    print("\n6. Customization (Colors, Labels, Titles)")
    customization_colors_labels()

    print("\n7. Pie Charts")
    pie_charts()

    print("\n8. Box Plots")
    box_plots()

    print("\n9. Heatmaps")
    heatmap()

    print("\n10. Saving Figures")
    saving_figures()

    print("\n11. Styling")
    styling()

    print("\n12. Multiple Plots (Advanced)")
    multiple_plots_advanced()

    print("\n13. Colormaps")
    colormaps()

    print("\n14. Annotations")
    annotations()

    print("\nAll examples completed! Uncomment plt.show() calls to display plots interactively.")
