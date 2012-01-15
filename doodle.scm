;; Doodle - Drawing and gaming made easy
;;
;; Copyright (c) 2012, Christian Kellermann
;; All rights reserved.

;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:

;;     Redistributions of source code must retain the above copyright
;;     notice, this list of conditions and the following disclaimer.
;;
;;     Redistributions in binary form must reproduce the above
;;     copyright notice, this list of conditions and the following
;;     disclaimer in the documentation and/or other materials provided
;;     with the distribution.
;;
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;; "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;; LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
;; FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE
;; COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT,
;; INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
;; (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
;; SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
;; HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
;; STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
;; ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED
;; OF THE POSSIBILITY OF SUCH DAMAGE.

(module doodle
        (*sprites*
         add-sprite!
         check-for-collisions
         circle
         clear-screen
         current-background
         draw-line
         event-handler
         filled-rectangle
         font-color
         font-size
         make-sprite
         new-doodle
         rectangle
         remove-sprite!
         run-event-loop
         set-font!
         show!
         solid-black
         solid-white
         text
         update-sprite!
         world-changes
         world-inits
         world-ends)

(import chicken scheme)
(use (srfi 1 4 18) cairo data-structures extras sdl miscmacros)

(define font-color (make-parameter (list 0 0 0 1)))
(define font-size (make-parameter 12))

(define current-background (make-parameter (list 0 0 0 1)))
(define solid-black (list 0 0 0 1))
(define solid-white (list 1 1 1 1))

(define *c* #f)
(define *c-surface* #f)
(define *s* #f)

(define (expect-procedure p)
  (unless (procedure? p)
    (error "This parameter can be set to procedures only, you gave it " p))
  p)

(define world-ends (make-parameter void expect-procedure))
(define world-changes (make-parameter void expect-procedure))
(define world-inits (make-parameter void expect-procedure))

(define-syntax set-color
  (syntax-rules ()
    ((_ color) (apply cairo-set-source-rgba `(,*c* ,@color)))))

(define (set-font! font size color)
  (font-color color)
  (font-size size)
  (cairo-select-font-face *c* font CAIRO_FONT_SLANT_NORMAL CAIRO_FONT_WEIGHT_NORMAL)
  (cairo-set-font-size *c* (font-size)))

(define (text-width text)
  (define ex (make-cairo-text-extents-type))
  (cairo-set-font-size *c* (font-size))
  (cairo-text-extents *c* text ex)
  (values (cairo-text-extents-width ex)
          (cairo-text-extents-height ex)))

(define (circle x y diameter color)
  (set-color color)
  (doto *c*
        (cairo-arc x y (/ diameter 2) 0 (* 2 cairo-pi))
        (cairo-fill)))

(define (filled-rectangle x1 y1 width height color)
  (set-color color)
  (doto *c*
        (cairo-new-path)
        (cairo-set-dash (make-f64vector 0) 0 0)
        (cairo-rectangle x1 y1 width height)
        (cairo-fill)
        (cairo-close-path)))

(define (rectangle x1 y1 width height color)
  (set-color color)
  (doto *c*
        (cairo-new-path)
        (cairo-set-dash (make-f64vector 0) 0 0)
        (cairo-rectangle x1 y1 width height)
        (cairo-stroke)
        (cairo-close-path)))

(define (draw-line x1 y1 x2 y2
                   #!key
                   (style #:solid))
  (doto *c*
        (cairo-new-path)
        (cairo-move-to x1 y1))
  (set-color solid-white)
  (case style
    ((#:solid)
     (cairo-set-dash *c* (make-f64vector 0) 0 0))
    ((#:dashed)
     (cairo-set-dash *c* (make-f64vector 1 5) 1 0))
    (else (error "unknown line style " style)))
  (doto *c*
        (cairo-line-to x2 y2)
        (cairo-stroke)))

(define (text x y text #!key (align #:left))
  (define (overall-height text)
    (let ((l (if (list? text) text (list text))))
      (fold (lambda (t s)
              (let-values (((w h) (text-width t)))
                          (+ s (* 1.5 h))))
            0
            l)))

  (define (draw-text x y text align)
    (let-values (((w h) (text-width text)))
                (let ((fx (case align
                            ((#:center) (- x (/ w 2)))
                            ((#:left) x)
                            ((#:right (- x w)))
                            (else x)))
                      (fy y))
                  (apply cairo-set-source-rgba `(,*c* ,@(font-color)))
                  (doto *c*
                        (cairo-move-to fx fy)
                        (cairo-set-font-size (font-size))
                        (cairo-show-text text))
                  h)))
  (if (list? text)
      (for-each (let ((y (if (eq? align #:center)
                             (- y (/ (overall-height text) 4))
                             y)))
                  (lambda (t)
                    (set! y (+ y (* 1.5 (draw-text x y t align))))))
                text)
      (draw-text x y text align)))

(define (new-doodle #!key
                    (width 680)
                    (height 460)
                    (title "Doodle")
                    (background solid-black)
                    (fullscreen #f))

  (sdl-init SDL_INIT_EVERYTHING)

  (set! *s* (sdl-set-video-mode width height 32 (+ SDL_HWSURFACE
                                                  SDL_HWPALETTE
                                                  SDL_DOUBLEBUF
                                                  (if fullscreen
                                                      SDL_FULLSCREEN
                                                      0))))
  (set! *c-surface* (cairo-image-surface-create-for-data
              (sdl-surface-pixels *s*)
              CAIRO_FORMAT_RGB24 width height
              (sdl-surface-pitch *s*)))

  (set! *c* (cairo-create *c-surface*))

  (sdl-wm-set-caption title "Pretty Pics Inc.")
  (current-background background)
  (apply cairo-set-source-rgba `(,*c* ,@solid-black))
  (doto *c*
        (cairo-rectangle 0 0 width height)
        (cairo-fill)
        (cairo-stroke))
  (sdl-flip *s*))

(define (clear-screen #!optional (color (current-background)))
  (let ((width (cairo-image-surface-get-width *c-surface*))
        (height (cairo-image-surface-get-height *c-surface*)))
    (apply cairo-set-source-rgba `(,*c* ,@color))
    (doto *c*
          (cairo-rectangle 0 0 width height)
          (cairo-fill)
          (cairo-stroke))))

(define (show!)
  (cairo-surface-flush *c-surface*)
  (sdl-flip *s*))


;; Collision detection

;; sprites are very crude rectangles atm consisting of a name, x, y,
;; width and height in pixels
(define-record sprite name x y width height)
(define *sprites* '())

(define-record-printer (sprite s p)
  (fprintf p "#,(sprite ~s ~s ~s ~s ~s)"
           (sprite-name s)
           (sprite-x s)
           (sprite-y s)
           (sprite-width s)
           (sprite-height s)))

(define (collides? s ss)
  (define (inside? x y s)
     (and (< (sprite-x s) x (+ (sprite-x s) (sprite-width s)))
          (< (sprite-y s) y (+ (sprite-y s) (sprite-height s)))))

  (define (sprite->points s)
    `((,(sprite-x s) ,(sprite-y s))
      (,(+ (sprite-x s) (sprite-width s)) ,(sprite-y s))
      (,(sprite-x s) ,(+ (sprite-height s) (sprite-y s)))
      (,(+ (sprite-x s) (sprite-width s)) ,(+ (sprite-y s) (sprite-height s)))))

  (define (overlap s1 s2)
    (or (any (lambda (xy)
               (inside? (first xy) (second xy) s1))
             (sprite->points s2))
        (any (lambda (xy)
               (inside? (first xy) (second xy) s2))
             (sprite->points s1))))

  (let ((cs (map (cut sprite-name <>)
                  (filter (lambda (s2)
                            (overlap s s2))
                          ss))))
    (and (not (null? cs)) cs)))

(define (check-for-collisions)
  (let loop ((s (map cdr *sprites*))
             (cols '()))
    (cond ((null? s) cols)
          ((collides? (car s) (remove (cut equal? (car s) <>) (map cdr  *sprites*)))
           => (lambda (o) (loop (cdr s) (cons (list (sprite-name (car s)) o) cols))))
          (else (loop (cdr s) cols)))))

(define (remove-sprite! s-name)
  (alist-delete! s-name *sprites* equal?))

(define (update-sprite! s)
  (set! *sprites* (alist-update! (sprite-name s) s *sprites*)))

(define add-sprite! update-sprite!)

;; Event stuff

(define (translate-key-event type event)
  (let ((k (sdl-event-sym event)))
    (list 
     (if (equal? type SDL_KEYUP)
         'released 'pressed)
     (cond ((equal? k SDLK_UP)
            'up)
           ((equal? k SDLK_DOWN)
            'down)
           ((equal? k SDLK_LEFT)
            'left)
           ((equal? k SDLK_RIGHT)
            'right)
           ((integer->char k)
            => identity)
           (else 'unknown)))))

(define (translate-events event escape)
  (if (not event)
      #f
      (let ((t (sdl-event-type event)))
        (cond ((equal? t SDL_QUIT)
               (escape 'quit))
              ((or (equal? t SDL_KEYDOWN)
                   (equal? t SDL_KEYUP))
               (translate-key-event t event))
              (else (list 'unknown event))))))

(define (event-handler #!optional minimum-wait)
  (lambda ()
    (let ((event (make-sdl-event))
          (last (current-milliseconds)))
      (call-with-current-continuation
       (lambda (escape)
         (let loop ()
           (let* ((now (current-milliseconds))
                  (dt (min (/ 1 30) (/ (- now last)
                                       1000))))
             (sdl-pump-events)
             ((world-changes)
              (translate-events (if (sdl-poll-event! event) event #f) escape)
              dt
              escape)
             (show!)
             (set! last now)
             (when
                 minimum-wait
               (thread-sleep! minimum-wait))
             (loop)))))
      ((world-ends))
      (sdl-quit))))

(define (run-event-loop #!key
                        (run-in-background #f)
                        (minimum-wait 0))
  ((world-inits))
  (sdl-flip *s*)
  (if run-in-background
      (thread-start!
       (make-thread (event-handler minimum-wait) "doodle-event-loop"))
      (thread-join!
       (thread-start!
        (make-thread (event-handler minimum-wait) "doodle-event-loop"))))))