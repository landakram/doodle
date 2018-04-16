(use doodle sdl-base matchable miscmacros)
(new-doodle)

(define circle-max-diameter 40)
(define circle-diameter circle-max-diameter)

(define circle-x (/ doodle-width 2))
(define circle-y (- doodle-height circle-diameter))

(world-inits
 (lambda ()
   (clear-screen)
   (filled-circle circle-x circle-y circle-diameter '(1 0 0 1))))

(define direction -1) ;; -1 is up +1 is down
(define speed 20)      ;; should be FPS

(define pulsing-speed 2)
(define pulsing-direction -1)

(define (maybe-change-pulse-dir)
  (cond ((<= circle-diameter 0)
         (set! pulsing-direction (* pulsing-direction -1))
         (set! circle-diameter 0))
        ((>= circle-diameter circle-max-diameter)
         (set! pulsing-direction (* pulsing-direction -1))
         (set! circle-diameter circle-max-diameter))))

(define (maybe-change-circle-dir)
   (cond ((<= circle-y 0)
          (set! direction (* direction -1))
          (set! circle-y 0))
         ((>= circle-y doodle-height)
          (set! direction (* direction -1))
          (set! circle-y (- doodle-height circle-diameter)))))

(define acc 0)

(world-changes
 (lambda (events dt quit)
   (for-each
    (lambda (e)
      (match
       e
       (('key 'pressed #\esc)
        (quit #t))
       (else (void))))
    events)

   (clear-screen)

   (maybe-change-circle-dir)
   (maybe-change-pulse-dir)

   (set! circle-y (+ circle-y (* direction (* speed (/ dt 1000)))))

   (inc! acc (inexact->exact (ceiling dt)))
   (when (zero? (modulo acc 60))
     (set! acc 0)
     (set! circle-diameter (+ circle-diameter (* pulsing-speed pulsing-direction))))



   (text 10 15 "Press ESC to quit")
   (text 10 35 (sprintf "circle size ~a , ~a dt sdl-ticks: ~a" circle-diameter dt (sdl-get-ticks)))

   (filled-circle circle-x circle-y circle-diameter '(1 0 0 1))
   (filled-circle 50 100 circle-diameter '(1 1 0 1))

   (show!)))

(run-event-loop)
