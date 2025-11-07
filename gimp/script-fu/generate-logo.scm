(define (create-circle-logo output-path text1 text2)
  (let* (
      (width 1024)
      (height 1024)
      (dpi 72)
      (image (car (gimp-image-new width height RGB)))
      (layer (car (gimp-layer-new image width height RGBA-IMAGE "Base Layer" 100 NORMAL-MODE)))
    )
    (gimp-image-add-layer image layer 0)
    (gimp-drawable-fill layer TRANSPARENT-FILL)

    ;; Set DPI
    (gimp-image-set-resolution image dpi dpi)

    ;; Draw the circle
    (let* (
        (inset 24)
        (circle-x inset)
        (circle-y inset)
        (circle-diameter (- width (* 2 inset)))
      )
      (gimp-ellipse-select image circle-x circle-y circle-diameter circle-diameter CHANNEL-OP-REPLACE TRUE FALSE 0)
      (gimp-context-set-foreground '(0 0 0)) ; black
      (gimp-context-set-brush-size 10.0)
      (gimp-edit-stroke layer)
      (gimp-selection-none image)
    )

    ;; Add text (centered)
    (let* (
        (font-size 200)
        (font "Sans Bold")
        (text (string-append text1 text2))
        (text-layer (car (gimp-text-fontname image -1 0 0 text 10 TRUE font-size PIXELS font)))
        (text-width (car (gimp-drawable-width text-layer)))
        (text-height (car (gimp-drawable-height text-layer)))
        (offset-x (/ (- width text-width) 2))
        (offset-y (/ (- height text-height) 2))
      )
      (gimp-layer-set-offsets text-layer offset-x offset-y)
    )

    ;; Export the image as PNG
    (file-png-save 1 image (car (gimp-image-get-active-layer image))
      output-path output-path 0 9 1 0 0 1 1)
    
    (gimp-image-delete image)
  )
)

(script-fu-register
 "create-circle-logo"
 "Create Circle Logo"
 "Creates a transparent 1024x1024 PNG with a bold circle and text."
 "John Brahy"
 "Ad Trust"
 "2025"
 ""
 SF-FILENAME "Output File" "circle_logo.png"
 SF-STRING "First Letter" "A"
 SF-STRING "Second Letter" "B"
)

(script-fu-menu-register "create-circle-logo" "<Image>/File/Create")

