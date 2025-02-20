from PIL import Image, ImageDraw

# Load the original image
original_image_path = "/Users/jbrahy/Projects/snippets/python/tmp/image.jpg"
modified_image_textured_fin_path = "/Users/jbrahy/Projects/snippets/python/tmp/image_with_fin.jpg"

# Reload the original image
original_image = Image.open(original_image_path)

# Define the shark fin coordinates to place it lower and more to the right between the kids in the water
fin_coordinates = [(830, 880), (850, 830), (870, 880)]

# Create a function to add texture to the fin
def draw_textured_fin(image, fin_coords, texture_color="darkgray"):
    draw = ImageDraw.Draw(image)
    
    # Draw the base fin
    draw.polygon(fin_coords, fill=texture_color)
    
    # Add texture (simple lines to simulate texture)
    for i in range(5):
        offset = i * 3
        draw.line([(fin_coords[0][0] + offset, fin_coords[0][1]),
                   (fin_coords[1][0] + offset, fin_coords[1][1])], fill="black", width=1)
        draw.line([(fin_coords[1][0] + offset, fin_coords[1][1]),
                   (fin_coords[2][0] + offset, fin_coords[2][1])], fill="black", width=1)
    
    return image

# Add the textured shark fin to the image
textured_fin_image = draw_textured_fin(original_image, fin_coordinates)

# Save the modified image with the textured fin
textured_fin_image.save(modified_image_textured_fin_path)

modified_image_textured_fin_path

