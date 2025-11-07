#!/usr/bin/env python
from gimpfu import *

def create_pm_logo(output_path, letter1, letter2):
    # === Setup ===
    width, height = 1024, 1024
    inset = 24
    dpi = 72
    circle_width = width - 2 * inset
    font = "Sans Bold"

    # Create image
    image = gimp.Image(width, height, RGB)
    layer = gimp.Layer(image, "Background", width, height, RGBA_IMAGE, 100, NORMAL_MODE)
    image.add_layer(layer, 0)
    pdb.gimp_drawable_fill(layer, TRANSPARENT_FILL)
    image.resolution = (dpi, dpi)

    # Circle with two gaps
    center = width / 2
    radius = circle_width / 2
    arc_path = pdb.gimp_vectors_new(image, "partial_circle")
    image.add_vectors(arc_path)
    pdb.gimp_vectors_stroke_arc(arc_path, center, center, radius, 45, 115)
    pdb.gimp_vectors_stroke_arc(arc_path, center, center, radius, 230, 115)
    pdb.gimp_context_set_foreground((0, 0, 0))
    pdb.gimp_context_set_brush_size(8)
    pdb.gimp_edit_stroke_vectors(layer, arc_path)

    # Add center text
    center_text = letter1 + letter2
    font_size = 280
    text_layer = pdb.gimp_text_fontname(image, layer, 0, 0, center_text, 0, True, font_size, PIXELS, font)
    text_w = pdb.gimp_drawable_width(text_layer)
    text_h = pdb.gimp_drawable_height(text_layer)
    pdb.gimp_layer_set_offsets(text_layer, (width - text_w) // 2, (height - text_h) // 2)

    # Circle path for text-on-path
    text_path = pdb.gimp_vectors_new(image, "text_circle")
    image.add_vectors(text_path)
    pdb.gimp_vectors_stroke_ellipse(text_path, inset, inset, circle_width, circle_width)

    # Arced "POPULAR" text (top right, red)
    pdb.gimp_context_set_foreground((255, 0, 0))
    pdb.gimp_text_layer_new_from_path(image, text_path, "POPULAR", 0, 1, 60, True, font)

    # Arced "MARKETING" text (bottom right, black)
    pdb.gimp_context_set_foreground((0, 0, 0))
    pdb.gimp_text_layer_new_from_path(image, text_path, "MARKETING", 180, 1, 60, True, font)

    # Save the PNG
    pdb.file_png_save(image, layer, output_path, output_path, 0, 9, 1, 0, 0, 1, 1)
    gimp.delete(image)

register(
    "python_fu_generate_pm_logo",
    "Generate PM-style logo",
    "Creates a transparent PNG logo with circle, text, and arced words",
    "John Brahy", "John Brahy", "2025",
    "",  # Not in menu (CLI only)
    "",
    [
        (PF_STRING, "output_path", "Output PNG Path", "/tmp/logo.png"),
        (PF_STRING, "letter1", "First Letter", "P"),
        (PF_STRING, "letter2", "Second Letter", "M"),
    ],
    [],
    create_pm_logo
)

main()

