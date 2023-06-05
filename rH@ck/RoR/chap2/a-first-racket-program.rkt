#lang racket
(provide (all-defined-out))
#|
I've installed DrRacket, learned that programs go into the definitions panel, and experimented
with Racket in the interactions panel. We're ready to write our first real program, a simple
game for guessing numbers. 
|#

;; This first game is the simplest and oldest around. Guess My Number. Our program will figure
;; our the number by repeatedly making guesses and asking the player if her number is bigger
;; or smaller than the current guess.

;; Everything to the immediate right of a ( is normally a function. so here
;; (guess) (bigger) (smaller) we will have 3 functions that we need to define. With these functions
;; the player will be able to interact with the game.

;; What's the strategy?
;;;; Determine or set the upper and lower bounds (limits) of the number
;;;; Guess a number halfway between those two numbers
;;;; If the player says the number is smaller, lower the upper bound
;;;; If the player says the number is bigger, raise the lower bound.

;; Cutting the range of numbers in half on every guess we can quickly find the number. this is
;; called Binary Search in DSA. Its remarkable effective, even if we played the game from 1
;; to 1,000,000, we could get the number in just 10 guesses. This makes binary search a
;; O(logn) complexity. So let's do it:

;; we define functions and vars with (define ...). Here we see to the right of (, the 'keyword'
;; 'define'. Its actually a macro, if this is like common lisp, but function, macro, keyword,
;; they basically all act the same and get applied to their arguments.

;; some notes about racket and scheme syntax etiquette. racket ignores spaces and lines when it
;; reads code just like other lisps. So we could really put it as we want, as long as its between
;; two (). for that reason we need to stick to some pretty basic conventions so that our code
;; continues to be readable to all.

#|
NOTE: pressing the 'tab' key in DrRacket doesn't give you a tab, which I've figured out earlier.
It automatically indents our code to follow common conventions. We can also auto-indent a chunck
of code by highlighting it and then pressing the 'tab' key. We can auto-indent an entire file,
kind like 'go fmt' by using Ctrl-i
|#

;; functions in racket and scheme are defined like...
#|
(define (function-name argument-name ...) ;; a function can have zero to however many args.
  function-body-expression                ;; all functions must have at least 1 body-expression
  function-body-expression
  ...)
NOTE the indentation.
|#


;; The first function is guess.
(define (guess)
  (quotient (+ lower upper) 2)) ;; as we see, dr racket helps calling out any errors such as missing vars or ()s
;; I tried to use /, but it seems to be for 'real' division, and quotient is like div in Haskell
;; for integer division
(define lower 1)
(define upper 100)
#|
In other programming languages before we see the need to write 'return' to cause a value to be
returned. This is not needed in Racket. The final value calculated in the body of the function is
returned automatically, just all other lisps. 
|#

;; our other functions for closing into the the answer:
(define (smaller) ; smaller takes no parameters
  (set! upper (max lower (sub1 (guess)))) ; body consists of 2 expr, one per line.
  (guess))

;; set! is pronounced 'set bang' which sets or changes vars. in sicp this is the 'assignment operator'
;; bigger is created the same way
(define (bigger)
  (set! lower (min upper (add1 (guess))))
  (guess))

;; so reading from inside out on both smaller and bigger, we are adding or subtracting 1 from
;; guess and then changing either the lower or upper bound to that number and then we run guess
;; again, but this time with the new upper or lower bound which will give us the middle of
;; the next range and so on ...


;; We can also have one main function, that starts and restarts our application. Putting it at the
;; top of the definitions panel helps readers understand the purpose of our program and we would
;; usually do so, but here it is:
(define (start n m)
  (set! lower (min n m)) ; setting at the min and max of n m, means we don't have to worry about
  (set! upper (max n m)) ; which arg somes first. 
  (guess))

;; CHAPTER CHECKPOINT
;; In this chapter, we learned some basic Racket forms. Along the way, you learned how to do the
;; the following.
;;;; Use define to define a variable or function
;;;; Use set! to change the value of a variable
;;;; use the interactions panel for experimentation.
;;;; Copy and paste successful experiments to the definitions panel.


