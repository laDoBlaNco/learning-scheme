#lang racket
(require 2htdp/universe 2htdp/image)
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
;; (define (guess)
;;   (quotient (+ lower upper) 2)) ;; as we see, dr racket helps calling out any errors such as missing vars or ()s
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
;; (define (smaller) ; smaller takes no parameters
;;   (set! upper (max lower (sub1 (guess)))) ; body consists of 2 expr, one per line.
;;   (guess))

;; set! is pronounced 'set bang' which sets or changes vars. in sicp this is the 'assignment operator'
;; bigger is created the same way
;; (define (bigger)
;;   (set! lower (min upper (add1 (guess))))
;;   (guess))

;; so reading from inside out on both smaller and bigger, we are adding or subtracting 1 from
;; guess and then changing either the lower or upper bound to that number and then we run guess
;; again, but this time with the new upper or lower bound which will give us the middle of
;; the next range and so on ...


;; We can also have one main function, that starts and restarts our application. Putting it at the
;; top of the definitions panel helps readers understand the purpose of our program and we would
;; usually do so, but here it is:
;; (define (start n m)
;;   (set! lower (min n m)) ; setting at the min and max of n m, means we don't have to worry about
;;   (set! upper (max n m)) ; which arg somes first. 
;;   (guess))

;; CHAPTER CHECKPOINT
;; In this chapter, we learned some basic Racket forms. Along the way, you learned how to do the
;; the following.
;;;; Use define to define a variable or function
;;;; Use set! to change the value of a variable
;;;; use the interactions panel for experimentation.
;;;; Copy and paste successful experiments to the definitions panel.

;; Coming back to this program we are now going to add the gui. and trying this one in Micro 
;; instead of drr. We are going to create a pretty simply gui. We'll display the program's guesses
;; and respond to the player's key presses. The player will use the up and down arrow keys to 
;; signal 'bigger' and 'smaller', if the number is guess we press '=' or we can press 'q' to quit.

;; Let's start with the data. The data we use to represent out state makes all the difference,
;; because it almost always dictates how the code will be organized.

;; we start with a struct of two fields
(struct interval (small big) #:transparent)

;; now some constants
(define TEXT-SIZE 11)
(define HELP-TEXT 
  (text "↑ for larger numbers, ↓ for smaller ones" 
        TEXT-SIZE 
        "blue"))
(define HELP-TEXT2 
  (text "Press = when your number is guessed; q to quit." 
        TEXT-SIZE 
        "blue"))
(define WIDTH (+ (image-width HELP-TEXT2) 10))
(define HEIGHT 150)
(define COLOR "red")
(define SIZE 72)
(define TEXT-X 3)
(define TEXT-UPPER-Y 10)
(define TEXT-LOWER-Y 135)
(define MT-SC 
  (place-image/align 
   HELP-TEXT TEXT-X TEXT-UPPER-Y 
   "left" "top" 
   (place-image/align 
    HELP-TEXT2 
    TEXT-X TEXT-LOWER-Y "left" "bottom"
    (empty-scene WIDTH HEIGHT))))


;; Now let's redo our smaller and bigger functions, pretty much the same except instead of using 
;; set! to change bounds, we'll create new interval structures
(define (smaller w)
  (interval (interval-small w)
            (max (interval-small w) (sub1 (guess w)))))

(define (bigger w)
  (interval (min (interval-big w) (add1 (guess w))) ;; NOTE this is put in different order
            (interval-big w)))
;; as you can see we are now using the struct fields to calculate the next guesses
(define (guess w)
  (quotient (+ (interval-small w) (interval-big w)) 2))

;; Rendering - here we define the handler for the rendering for the 'to-draw' clause, drawing a 
;; scene everytime big-bang is updated
(define (render w)
  (overlay (text (number->string (guess w)) SIZE COLOR) MT-SC)) ; place the guess into bg scene

;; we also need our final scene on game-end
(define (render-last-scene w)
  (overlay (text "The End" SIZE COLOR) MT-SC))

;; Time to Stop
;; Finally let's take care of the items that stop the game.  
(define (single? w)
  (= (interval-small w) (interval-big w))) ; if the bounds are the same, we've gotten to the number


;; Key events - our key handler takes in a world (state) and a key-event. Then it returns a new
;; world or the world that it has been given, depending on which key has been pressed. we use a 
;; simple 'cond' with all the functions from our game and (stop-with...) from big-bang
(define (deal-with-guess w key)
  (cond ((key=? key "up") (bigger w))
        ((key=? key "down") (smaller w))
        ((key=? key "q") (stop-with w))
        ((key=? key "=") (stop-with w))
        (else w)))

;; start start with our main function and then add the subsequent function on top of it
(define (start lower upper)
  (big-bang (interval lower upper)
    (on-key deal-with-guess)
    (to-draw render)
    (stop-when single? render-last-scene)))
    
#|
CHAPTER CHECKPOINT

In this chapter we described how to create animations and games:

-- The 2htdp/image library is for creating graphics
-- In rkt, graphical shapes are values just like numbers in other languages
-- You can use big-bang from the 2htdp/universe to create animations and games
-- Tick handlers are functions that step a world from one moment to the next.
-- Key-event handlers compute new worlds in response to keyboard events.

There are some challenges with this chapter that I can take a look at.
|#
    
