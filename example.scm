(use matchable doodle)

(define *paint* #f)

(define red '(1 0 0 0.3))

(world-inits
 (lambda ()
   (clear-screen)
   (set-font! "Vollkorn" 18 red)
   (text (/ doodle-width 2)
         (/ doodle-height 2) '("Welcome to doodle!"
                               "Click the left mouse button to draw circles"
                               "Press ESC to leave")
         align: #:center)))

(world-changes
 (lambda (events dt exit)
   (for-each
    (lambda (e)
      (match e
       (('mouse 'pressed x y 1)
        (set! *paint* #t)
        (filled-circle x y 10 red))
       (('mouse 'released x y 1)
        (set! *paint* #f))
       (('mouse 'moved x y)
        (when *paint*
          (filled-circle x y 10 red)))
       (('key 'pressed #\esc)
        (exit #t))
       (else (void))))
    events)))

(new-doodle title: "Doodle paint" background: solid-white)
(run-event-loop)
