#lang racket
#|
Step into Realm of Racket, a book that takes you on a unique journey into the land
of computer programming. In the style of Conrad Barski's LAND OF LISP, this book
teaches you how to program in Racket by creating a series of games. Racket is a
a friendly mutation of lisp that's perfect for all, from those wo want to launch
their on education in comuter science to those looking to expand their knowledge
and experience in programming. 
|#

;; after learning a little of the history of lisp, let's take a look at what it looks like:
;; here some valid lisp expression for math:
(* 1 1)
(- 8 (* 2 3))
(sqrt 9)
;; looks like math except for the prefix notation and the ()s ... by why?
(+ 1 2 3 4 5 6 7 8   9 10)  ; becasue its awesome!

;; its also easier. Look at the following valid c++ code:
;; (foo<bar)*g++.baz(!&qux::zip->ding());  ;;  I mean WTF!
;; now lisp:
(sqrt (+ (sqr 3) (sqr 4))) ; deeply nested problem, but the ()s tell you immediately what's happening

;; Now lists
(list 1 2 3 4 5 6 7 8 9 0)
(list(list 1 3 5 7 9) (list 2 4 6 8 0))
(list (list 'hello 'world)
      (list (list 'it 'is) 2063)
      (list 'and 'we 'love 'Racket))

;; an even better way to right the above, ' the whole list
'((hello world)
  ((it is) 2063)
  (and we love Racket 💓)) ; and unicode works

;; one last examle of what we can do with lists
'(sqrt (+ (sqr 3) (sqr 4))) ; we just turned code into data
;; I didn't really realize the importance of how powerful this quoting stuff was. page 5 of RoR
;; The quoted expression creates a piece of data that captures all the structure that is this
;; program expression itself; its nexting structure, its numbers, and its symbols. If you had
;; used string quotes to turn the expression into a piece of data, all you would have is a string.
;; Before you could recover the expression's structure and organization you would need to complete
;; an entire computer science undergraduate major. In Lisp, all it takes is one keystroke on your
;; keyboard, one character on the screen. YOU CAN'T DO ANYTHING LIKE THIS WITHOUT LISP. Now thats
;; cool. And it's powerful.

;; This first chapter talked to us about some historic background about programming and the
;; fabulous lisp family of languages.

;;;; Computers are dumb pieces of hardware
;;;; Programmers use programming languages to turn computers into useful and entertaining gadgets
;;;; One of the oldest high-level languages is lisp, which is more than 50 years old
;;;; There are many related flavors of lisp, and we call lisp a family of languages
;;;; To this day, lisp offers programmers a way to experience programming as poetry
;;;; Racket is our chosen flavor of lisp. Racket is relatively new, and it is especially well
;;;;;; suited for novice programmers who want to ramp up gradually.





