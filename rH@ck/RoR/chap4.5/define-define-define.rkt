#lang racket
(require 2htdp/image)
#|
So far, we have covered the basics of defining variables and functions. But is there more
to it? In this chapter, we will see everything we need to know about defs

NOTE THAT THESE FUNCTIONS BELOW ARE BROKEN AS THEY ARE JUST EXAMPLES FROM THE BOOK AND ITS NOT
A WORKING PROGRAM. SO MOVING ON TO THE NEXT CHAPTER NOW THAT i HAVE THE IDEAS CONVEYED BELOW.
|#

;; module-level-defs
;; the most common type of defs are module-level, meaning that they can be accessed anywhere in
;; the module. There are two kinds of module level defs:
;;;; variable defs
;;;; function defs

;; we'll do this by working on a GUI for our guessing number game
;; Var defs - by convention we use all caps for module level vars, kinda like constants in other
;; langs. But I don't think I'm gonna do that one all the time.
(define WIDTH 100)
(define HEIGHT 200) ; we can impose limits on our constants by putting a function underneath
(unless (> HEIGHT 0)
  (error 'guess-my-number "HEIGHT may not be negative"))

(define X-CENTER (quotient WIDTH 2))
(define Y-CENTER (quotient HEIGHT 2))

;; Function definitions
(define SQR-COLOR "red")
(define SQR-SIZE 10)
(define (draw-square img x y)
  (place-image (square SQR-SIZE "solid" SQR-COLOR)
               x y
               img))


;; Local Definitions
;; We have scope defs in rkt as with all schemes. This means we can put a define anywhere and
;; the definition will only be accessible there.
(struct posn (x y))
(struct rectangle (width height))
(define  (inside-of-rectangle? r p)
  (define x (posn-x p))
  (define y (posn-y p))
  (define width (rectangle-width r))
  (define height (rectangle-height r))
  (and (<= 0 x) (< x width) (<= 0 y) (< y height)))
;; the scopes of these vars are only seen inside 'inside-of-rectangle?'
;; local defs still work inside a cond
(define (random-stars n)
  (cond
    ((zero? n) '())
    (else (define location (random-location 200 300)) ; note this def is only availabe in this branch
          (if (inside-moon? location)
              (random-stars n)
              (cons location (random-stars (sub1 n)))))))

;; here's another example of local definition
(define (winners lst pred)
  (cond
    ((empty? lst) (list pred))
    (else
     (define fst (first lst))
     (if (score> (record-score pred) (record-score fst))
         (list pred)
         (cons pred (winners (rest lst) fst))))))

;; i see some 'wishful thinking in the function above, so we need to make sure we have those
;; things created
(struct record (name score))

(define (winning-players lst)
  (define sorted-lst (sort lst ...))
  (define (winners lst pred)
    (cond
      ((empty? lst) (list pred))
      (else
       (define fst (first lst))
       (if (score> (record-score pred) (record-score fst))
           (list pred)
           (cons pred (winners (rest lst) fst))))))
  ;; start here
  (winners (rest sorted-lst) (first sorted-lst)))

;; this kinda of local function def is a common things in rkt. I've seen the same in Haskell as well
;; since winners shouldn't be applied to arbitrary lists and records, we hide it by defining it
;; inside the scope of some other function that consumes a list of records. This this function sorts
;; the list and applies winners to the appropriate pieces. As you know by now, the function named
;; winners can be accessed only between (define and its closing ()s, and therefore no one can
;; apply this function in the wrong way.


#|
CHAPTER CHECKPOINT

In this chapter we learned:
--- How to write module-level var and func defs
--- The advantages of introducing constant defs
--- How to write local, variable and function defs
--- Where to place local, variable, and function defs
--- The benefits of defining varibles and functions locally.
|#