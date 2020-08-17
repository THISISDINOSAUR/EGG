#lang racket

(require racket/gui/base)

(define DRAWING-SCALE 100)
(define BITMAP-SCALE 30)
(define SCREEN-WIDTH 500)
(define SCREEN-HEIGHT 500)
(define EGG-COLOUR (make-object color% 245 245 235 1))
(define EGG-OUTLINE-COLOUR (make-object color% 15 15 15 1))

(define (egg ellipticity asymmetry step)
  (define half
    (map (lambda (x)
           (cons x (egg-function x ellipticity asymmetry)))
         (range -1 1 step)))
  (append
   half
    (reverse
     (map (lambda (point)
            (cons (car point) (- (cdr point))))
       half))))

;https://academic.oup.com/auk/article/119/4/1179/5562071
(define (egg-function x ellipticity asymmetry)
  (*
   ellipticity
   (expt (+ 1 x) (/ 1 (+ 1 asymmetry)))
   (expt (- 1 x) (/ asymmetry (+ 1 asymmetry)))))

(define (scale-egg egg scale)
  (map (lambda (point)
         (cons (* (car point) scale) (* (cdr point) scale)))
       egg))

(define (egg->path points)
  (let ([p (new dc-path%)])
    (define firstPoint (car points))
    (send p move-to (car firstPoint) (cdr firstPoint))
    (for ([(point) points])
      (send p line-to (car point) (cdr point)))
    (send p line-to (car firstPoint) (cdr firstPoint))
    (send p rotate (degrees->radians -90))
    p))

(define (draw-egg dc egg)
  (define egg-path (egg->path egg))

  (send dc set-brush EGG-COLOUR 'solid)
  (send dc set-pen EGG-OUTLINE-COLOUR 0 'solid)
  (send dc draw-path egg-path))

(define (create-and-show-egg-canvas egg)
  (define frame-width SCREEN-WIDTH)
  (define frame-height SCREEN-HEIGHT)
  (define frame (new frame%
                     [label "EGG"]
                     [width frame-width]
                     [height frame-height]))
  
  (define canvas
    (new egg-canvas%
         [parent frame]
         [style '(transparent)]
         [egg egg]))
 
  (send frame show #t))

(define egg-canvas%
  (class canvas%
    (inherit get-width get-height get-dc refresh)

    (init-field
     [egg #f])
    
    (super-new)

    (send (get-dc) translate (/ SCREEN-WIDTH 2) (/ SCREEN-HEIGHT 2))

    (define/override (on-paint)
      (define dc (get-dc))
      (draw-egg dc egg))
    ))

(define (create-bitmap egg)
  (define-values (left top width height) (send (egg->path egg) get-bounding-box))
  
  (define bitmap (make-bitmap (exact-ceiling width) (exact-ceiling height)))
  (define dc (new bitmap-dc% [bitmap bitmap]))
  (send dc translate (/ (ceiling width) 2) (/ (ceiling height) 2))
  (draw-egg dc egg)
  bitmap)

(create-and-show-egg-canvas (scale-egg (egg 0.7 0.8 0.001) DRAWING-SCALE))

(println (create-bitmap (scale-egg (egg 0.7 0.8 0.001) BITMAP-SCALE)))
(println (create-bitmap (scale-egg (egg 0.8 1 0.001) BITMAP-SCALE)))
(println (create-bitmap (scale-egg (egg 0.9 0.8 0.001) BITMAP-SCALE)))
(println (create-bitmap (scale-egg (egg 1 1 0.001) BITMAP-SCALE)))