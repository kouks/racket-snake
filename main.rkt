#lang racket/gui

(define grid '(25 15))
(define cell 30)
(define rate 10)
(define score 0)
(define playing #t)

(define (Game board timer)
  (let ([dir (send board get-direction)] [snake (send board get-snake)])
    (send board set-snake (get-new-snake snake dir))
  )
)

(define (get-new-snake snake dir)
  (cond
    [(equal? dir 'up) (move-snake snake (list (caar snake) (if (= 0 (cadar snake)) 14 (- (cadar snake) 1))))]
    [(equal? dir 'down) (move-snake snake (list (caar snake) (if (= 14 (cadar snake)) 0 (+ (cadar snake) 1))))]
    [(equal? dir 'left) (move-snake snake (list (if (= 0 (caar snake)) 24 (- (caar snake) 1)) (cadar snake)))]
    [(equal? dir 'right) (move-snake snake (list (if (= 24 (caar snake)) 0 (+ (caar snake) 1)) (cadar snake)))]
  )
)

(define (move-snake snake piece)
  (if (member piece snake)
    (begin (set! playing #f) (list))
    (if (equal? piece (send board get-food))
      (begin (new-food snake) (append (list piece) snake))
      (append (list piece) (reverse (cdr (reverse snake))))
    )
  )
)

(define (new-food snake)
  (set! score (+ score 1))
  (set! rate (* rate 1.001))
  (send loop start (inexact->exact (floor (/ 1000 rate))))
  (let ([x (random (car grid))] [y (random (cadr grid))])
    (if (member (list x y) snake)
      (new-food snake)
      (send board set-food (list x y))
    )
  )
)

(define board%
  (class canvas%
    (inherit get-width get-height refresh)

    (define direction 'right)
    (define snake (list (list 5 5) (list 5 6)))
    (define food (list 7 7))

    (define/public (set-direction value) (set! direction value))
    (define/public (get-direction) direction)

    (define/public (set-snake value) (set! snake value))
    (define/public (get-snake) snake)

    (define/public (set-food value) (set! food value))
    (define/public (get-food) food)

    (define/override (on-char ke)
      (case (send ke get-key-code)
        [(left right up down)
          (set-direction (send ke get-key-code))
          (refresh)
        ]
      )
    )

    (define/public (step callback)
      (callback)
      (refresh)
    )

    (define/private (paint self dc)
      (if playing
        (begin
          (send dc set-brush "black" 'solid)
          (send dc set-pen "white" 4 'solid)

          (for ([piece snake])
            (let ([x (car piece)] [y (cadr piece)])
              (send dc draw-rectangle (* x cell) (* y cell) cell cell)
            )
          )

          (send dc set-brush "red" 'solid)
          (send dc set-pen "white" 8 'solid)
          (send dc draw-rectangle (* (car food) cell) (* (cadr food) cell) cell cell)
        )
        (begin
          (set! score (* 1000 (/ score timer)))
          (let ([text (format "Game Over, Score: ~a" (round score))])
            (send dc draw-text text 5 5)
          )
        )
      )
    )

    (super-new (paint-callback (lambda (c dc) (paint c dc))))
  )
)

(define window (new frame%
  [label "snake"]
  [width (* cell (car grid))]
  [height (* cell (cadr grid))]
))
(define board (new board% (parent window)))

(define timer 0)
(define loop
  (new timer% [interval (floor (/ 1000 rate))] [notify-callback (lambda ()
    (if playing
      (send board step (lambda ()
        (set! timer (+ 1 timer))
        (Game board timer)
      ))
      (send loop stop)
    )
  )])
)

(send window show #t)
