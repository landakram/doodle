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
         blit-image
         check-for-collisions
         circle
         clear-screen
         current-background
         doodle-width
         doodle-height
         doodle-context
         draw-line
         define-resource
         filled-circle
         filled-polygon
         filled-rectangle
         font-color
         font-size
         make-sprite
         line-width
         new-doodle
         polygon
         rectangle
         remove-sprite!
         run-event-loop
         save-screenshot
         set-font!
         show!
         solid-black
         solid-white
         text
         text-width
         update-sprite!
         world-changes
         world-inits
         world-ends
         world-fps)

(import chicken scheme)
(use (srfi 1 4 18) cairo data-structures extras sdl-base sdl-gfx clojurian-syntax matchable)

(define *font-color* '(1 1 1 1))
(define (font-color . c)
 (if (null? c)
     *font-color*
     (set! *font-color* (car c))))
(define *font-size* 12)
(define (font-size . s)
  (if (null? s)
      *font-size*
      (set! *font-size* (car s))))

(define *line-width* 2.0)
(define (line-width . w)
  (if (null? w)
      *line-width*
      (set! *line-width* (car w))))

(define *current-background* '(0 0 0 1))
(define (current-background . c)
  (if (null? c)
      *current-background*
      (set! *current-background* (car c))))

(define solid-black (list 0 0 0 1))
(define solid-white (list 1 1 1 1))

(define *c* #f)
(define *c-surface* #f)
(define *s* #f)

(define (doodle-context) *c*)
(define doodle-width #f)
(define doodle-height #f)


(define *world-inits* (cons values values))
(define *world-changes* (cons values values))
(define *world-ends* (cons values values))
(define *fps* 60)
(define *fps-manager* #f)

(define (world-ends f)
  (set-cdr! *world-ends* (car *world-ends*))
  (set-car! *world-ends* f))
(define (world-changes f)
  (set-cdr! *world-changes* (car *world-changes*))
  (set-car! *world-changes* f))
(define (world-inits f)
  (set-cdr! *world-inits* (car *world-inits*))
  (set-car! *world-inits* f))

(define (world-fps . d)
  (if (null? d)
      *fps*
      (if (and (number? (car d)) (<= FPS_LOWER_LIMIT (car d) FPS_UPPER_LIMIT))
	  (set! *fps* (car d))
	  (error "Please provide a number for the world-fps within the FPS upper and lower limits, given " (car d)))))

(define-syntax set-color
  (syntax-rules ()
    ((_ color) (apply cairo-set-source-rgba `(,*c* ,@color)))))

(define (set-font! font size color)
  (set! *font-color* color)
  (set! *font-size* size)
  (cairo-select-font-face *c* font CAIRO_FONT_SLANT_NORMAL CAIRO_FONT_WEIGHT_NORMAL)
  (cairo-set-font-size *c* *font-size*))

(define (text-width text)
  (define ex (make-cairo-text-extents-type))
  (cairo-set-font-size *c* *font-size*)
  (cairo-text-extents *c* text ex)
  (values (cairo-text-extents-x-advance ex)
          (cairo-text-extents-y-advance ex)))

(define (circle x y diameter color)
  (set-color color)
  (doto *c*
        (cairo-new-path)
        (cairo-set-line-width *line-width*)
        (cairo-arc x y (/ diameter 2) 0 (* 2 cairo-pi))
        (cairo-stroke)
        (cairo-close-path)))

(define (filled-circle x y diameter color)
  (set-color color)
  (doto *c*
        (cairo-new-path)
        (cairo-set-line-width *line-width*)
        (cairo-arc x y (/ diameter 2) 0 (* 2 cairo-pi))
        (cairo-fill)
        (cairo-close-path)))

(define (filled-rectangle x1 y1 width height color)
  (set-color color)
  (doto *c*
        (cairo-new-path)
        (cairo-set-line-width *line-width*)
        (cairo-set-dash (make-f64vector 0) 0 0)
        (cairo-rectangle x1 y1 width height)
        (cairo-fill)
        (cairo-close-path)))

(define (rectangle x1 y1 width height color)
  (set-color color)
  (doto *c*
        (cairo-new-path)
        (cairo-set-line-width *line-width*)
        (cairo-set-dash (make-f64vector 0) 0 0)
        (cairo-rectangle x1 y1 width height)
        (cairo-stroke)
        (cairo-close-path)))

(define (draw-line x1 y1 x2 y2
                   #!key
                   (width *line-width*)
                   (color solid-white)
                   (style #:solid))
  (doto *c*
        (cairo-set-line-width width)
        (cairo-new-path)
        (cairo-move-to x1 y1))
  (set-color color)
  (case style
    ((#:solid)
     (cairo-set-dash *c* (make-f64vector 0) 0 0))
    ((#:dashed)
     (cairo-set-dash *c* (make-f64vector 1 5) 1 0))
    (else (error "unknown line style " style)))
  (doto *c*
        (cairo-line-to x2 y2)
        (cairo-stroke)))

(define (%polygon points color closed? method)
  (doto *c*
        (cairo-set-line-width *line-width*)
        (cairo-new-path))
  (when (and (not (null? points))
             (pair? (car points)))
    (cairo-move-to *c* (caar points) (cadar points)))
  (set-color color)
  (for-each
   (lambda (p)
     (cairo-line-to *c* (car p) (cadr p)))
   (cdr points))
  (if closed?
      (cairo-close-path *c*))
  (method *c*))

(define (polygon points color #!key (closed? #t))
  (%polygon points color closed? cairo-stroke))

(define (filled-polygon points color #!key (closed? #t))
  (%polygon points color closed? cairo-fill))

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
                            ((#:right) (- x w))
                            (else x)))
                      (fy y))
                  (apply cairo-set-source-rgba (cons *c* *font-color*))
                  (doto *c*
                        (cairo-move-to fx fy)
                        (cairo-set-font-size *font-size*)
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

(define (save-screenshot filename)
  (cairo-surface-write-to-png *c-surface* filename))

(define (sdl-colorspace->cairo bytes-per-pixel)
  (case (* 8 bytes-per-pixel)
    ((8) CAIRO_FORMAT_A8)
    ((24) CAIRO_FORMAT_RGB24)
    ((32) CAIRO_FORMAT_ARGB32)
    (else CAIRO_FORMAT_ARGB32)))

(define (new-doodle #!key
                    (width 680)
                    (height 460)
                    (title "Doodle")
                    (background solid-black)
                    (fullscreen #f))

  (sdl-init SDL_INIT_EVERYTHING)

  (set! *s* (sdl-set-video-mode width height 0 (+ SDL_HWSURFACE
                                                  SDL_HWPALETTE
                                                  SDL_DOUBLEBUF
                                                  (if fullscreen
                                                      SDL_FULLSCREEN
                                                      0))))

  (set! *c-surface* (cairo-image-surface-create-for-data
              (sdl-surface-pixels *s*)
              (sdl-colorspace->cairo (sdl-pixel-format-bytes-per-pixel
                                      (sdl-surface-pixel-format *s*)))
              width
              height
              (sdl-surface-pitch *s*)))

  (set! *c* (cairo-create *c-surface*))

  (set! doodle-width width)
  (set! doodle-height height)

  (sdl-wm-set-caption title title)

  (if (string? background)
        (let* ((width (cairo-image-surface-get-width *c-surface*))
               (height (cairo-image-surface-get-height *c-surface*))
               (img (cairo-image-surface-create-from-png background))
               (img-width (cairo-image-surface-get-width img))
               (img-height (cairo-image-surface-get-height img)))
          (print "width " width " img-width " img-width)
          (define-resource (string->symbol background) #:image background 0 0  (/ height img-height)))
        (apply cairo-set-source-rgba `(,*c* ,@background)))

  (current-background background)

  (doto *c*
        (cairo-rectangle 0 0 width height)
        (cairo-fill)
        (cairo-stroke)
        (cairo-paint))

  (sdl-flip *s*))

(define (clear-screen #!optional (color *current-background*))
(let ((width (cairo-image-surface-get-width *c-surface*))
      (height (cairo-image-surface-get-height *c-surface*)))
  (if (list? color)
      (begin
        (apply cairo-set-source-rgba (cons *c* color))
        (doto *c*
              (cairo-rectangle 0 0 width height)
              (cairo-fill)
              (cairo-stroke)))
      (blit-image (string->symbol color) 0 0))))

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

;; blitting tiles in

(define *resources* '())

(define-record img-res name file surface w h x-offset y-offset scale-factor)
(define-record-printer (img-res i out)
  (fprintf out "#,(img-res ~a ~s ~ax~a px (~ax~a) x ~a"
           (img-res-name i)
           (img-res-file i)
           (img-res-w i)
           (img-res-h i)
           (img-res-x-offset i)
           (img-res-y-offset i)
           (img-res-scale-factor i)))


(define (define-resource name type file . data)
  (when (not (file-exists? file))
    (error "Resource does not exist " file))
  (match type
         ('#:image
          (let* ((s (cairo-image-surface-create-from-png file))
                 (w (cairo-image-surface-get-width s))
                 (h (cairo-image-surface-get-height s))
                 (offset? (and (not (null? data))
                              (>= (length data) 2)))
                 (x-off (if offset? (car data) 0))
                 (y-off (if offset? (cadr data) 0))
                 (scale-factor (and (= (length data) 3)
                                    (caddr data))))
            (set! *resources*
                  (alist-update! name (make-img-res name file s w h x-off y-off scale-factor)
                                *resources*))))
         ('#:tileset
          (when (or (null? data)
                    (not (= (length data) 2)))
                (error "Tileset needs tile-size and list of tiles but got " data))
          (let* ((ts (cairo-image-surface-create-from-png file))
                 (w (cairo-image-surface-get-width ts))
                 (h (cairo-image-surface-get-height ts))
                 (tile-size
                  (if (number? (car data))
                      (car data)
                      (error "Tile-set parameter needs to be a number")))
                 (tiles (if (list? (cadr data))
                            (cadr data)
                            (error "Second data parameter must be a list of tiles"))))
            (for-each
             (lambda (t)
               (if (and (list t)
                        (= (length t) 2))
                   (let* ((tile-name (car t))
                          (tile-number (cadr t))
                          (tile-x (modulo (* tile-number tile-size) w))
                          (tile-y (* tile-size (quotient (* tile-size tile-number) w)))
                          (tile-img
                           (cairo-surface-create-for-rectangle ts tile-x tile-y tile-size tile-size)))
                     (set! *resources*
                           (alist-update! (car t) (make-img-res tile-name file tile-img tile-size tile-size 0 0 #f)
                                          *resources*)))
                   (error "Malformed tile information entry " t)))
             tiles)))
         (else (error "Unkown resource type " type))))

(define (blit-image name x y #!key (rotation 0))
  (let ((img (alist-ref name *resources* equal?)))
    (unless img
      (error "No resource found with this name " name))
    (let* ((scale (img-res-scale-factor img))
           (scale (if scale scale 1))
           (h (img-res-h img))
           (w (img-res-w img))
           (t (/ (- 1 scale) 2))
           (x (+ x (* t w)
                 (img-res-x-offset img)))
           (y (+ y (* t h)
                 (img-res-y-offset img))))
      (cairo-save *c*)
      (if (zero? rotation)
              (cairo-translate *c* x y)
              (doto *c*
                    (cairo-translate (+ (/ w 2) x)
                                     (+ (/ h 2) y))
                    (cairo-rotate (* (/ cairo-pi 180) rotation))
                    (cairo-translate (- (/ w 2)) (- (/ h 2)))))
      (doto *c*
            (cairo-scale scale scale)
            (cairo-set-source-surface (img-res-surface img) 0 0)
            (cairo-mask-surface (img-res-surface img) 0 0)
            (cairo-restore)))))

;; Event stuff

(define (translate-mouse-event type event)
(let ((x (sdl-event-x event))
      (y (sdl-event-y event)))
  (if (equal? type SDL_MOUSEMOTION)
      (list 'mouse 'moved x y)
      (let ((b (sdl-event-button event)))
        (list 'mouse
              (if (equal? type SDL_MOUSEBUTTONDOWN)
                  'pressed 'released)
              x y b)))))

;; Key translation tables
(include "doodle-keys.scm")

(define (get-modifiers mod)
  (filter-map (lambda (m)
                (and (not (zero? (bitwise-and mod (car m))))
                     (cdr m)))
          modifier-table))

(define (translate-key-event type event)
  (let ((k (sdl-event-sym event))
        (mods (get-modifiers (sdl-event-mod event))))
    (list 'key
     (if (equal? type SDL_KEYUP)
         'released 'pressed)
     mods
     (cond ((alist-ref k translation-table equal?) => identity)
           ((integer->char k) => identity)
           (else 'unknown)))))

(define (translate-events event escape)
  (if (not event)
      #f
      (let ((t (sdl-event-type event)))
        (cond ((equal? t SDL_QUIT)
               (escape 'quit))
              ((equal? t SDL_VIDEOEXPOSE)
               '(redraw))
              ((or (equal? t SDL_MOUSEBUTTONUP)
                   (equal? t SDL_MOUSEBUTTONDOWN)
                   (equal? t SDL_MOUSEMOTION))
               (translate-mouse-event t event))
              ((or (equal? t SDL_KEYDOWN)
                   (equal? t SDL_KEYUP))
               (translate-key-event t event))
              (else (list 'unknown event))))))

(define (collect-events)
  (let pump ((events '()))
    (let ((event (make-sdl-event)))
      (if (sdl-poll-event! event)
          (pump (cons event events))
          (reverse events)))))

(define (event-handler)
  (lambda ()
    (call-with-current-continuation
     (lambda (escape)
       (let loop ()
         (let* ((dt (sdl-get-framerate-delay *fps-manager*)))
           (call-with-current-continuation
            (lambda (k)
              (with-exception-handler
               (lambda (e)
                 (fprintf (current-error-port)
                          "Exception in world-changes: ~a ~a.~%"
                          ((condition-property-accessor 'exn 'message) e)
                          ((condition-property-accessor 'exn 'arguments) e))
                 (print-call-chain (current-error-port))
                 (k (world-changes (cdr *world-changes*))))
               (lambda ()
                 ((car *world-changes*)
                  (map (cut translate-events <> escape)
                       (collect-events))
                  dt
                  escape)))))
           (show!)
           (loop)))))
    (call-with-current-continuation
     (lambda (k)
       (with-exception-handler
        (lambda (e)
          (fprintf (current-error-port) "Exception in world-ends: ~a ~a~%"
                   ((condition-property-accessor 'exn 'message) e)
                   ((condition-property-accessor 'exn 'arguments) e))
          (print-call-chain (current-error-port))
          (k (world-ends (cdr *world-ends*))))
        (lambda ()
          ((car *world-ends*))))))
    (sdl-quit)))

(define (run-event-loop #!key
                        (run-in-background #f)
                        (fps *fps*))

  (set! *fps* fps)
  (set! *fps-manager* (make-fps-manager))
  (sdl-init-Framerate *fps-manager*)
  (sdl-set-framerate *fps-manager* *fps*)

  (call-with-current-continuation
   (lambda (k)
     (with-exception-handler
      (lambda (e)
        (fprintf (current-error-port) "Exception in world-inits: ~a ~a.~%"
                 ((condition-property-accessor 'exn 'message) e)
                 ((condition-property-accessor 'exn 'arguments) e))
        (print-call-chain (current-error-port))
        (k (cdr *world-inits*)))
      (lambda ()
        ((car *world-inits*))))))
  (sdl-flip *s*)
  (if run-in-background
      (thread-start!
       (make-thread (event-handler) "doodle-event-loop"))
      (thread-join!
       (thread-start!
        (make-thread (event-handler) "doodle-event-loop"))))))
