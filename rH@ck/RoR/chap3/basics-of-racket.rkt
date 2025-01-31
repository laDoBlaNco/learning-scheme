#lang racket
#|
so I lost my other notes for Chap 3, so Im going to very quickly summarize and add the code
that I had up to a certain point. Apparently some experimenting that I did with some images
corrupted the file. so I need to be careful about adding images and such into dr racket.

Syntax and Semantics -
Learning any language comes down to syntax (grammar) and semantics (meaning). With programs, and
this was discovered by Church and Turing in the 30's, just like any language, the syntax or
grammar can be very different, and the semantics or meaning be very similar, getting the same
result regardless of the language.

The problems comes into play with certain languages (programming or other) having a very complex
grammar, will mean that a very complex compiler/brain will be needed to understand the meaning.
C++ is an example. lisp is the opposite. with its grammar of just ()s and some words mostly, It
kills the complexity of most things giving us the expressivity that comes with most lisps.

Also some notes on syntax:
    line comments are done with ;
    block comments with #||# and they are nestable
    sexpr comments (commenting out code pieces in ()s with #

    Also the fact that rkt uses [] and {} in addition to (), but its completely convention driven
    rkt sees () all the time, so we I can use my preferred lisp method of () everywhere.
    "If you prefer different conventions, go ahead, adopt them, be happy. But be consistent"
|#

;; Basic building blocks (data types) of racket (lisps)

;; Booleans
'BOOLEANS
#f #t
(zero? 1) (zero? (sub1 1))
(newline)
;; Symbols - common data type in lisp. Standalone word with a ' that evaluates to itself
'SYMBOLS
'this 'are 'all 'symbols
(symbol=? 'foo 'FoO) ; symbols are case-sensitive
(newline)
;; Numbers - rkt supports floating-point, integers, rationals, complex, and many more. It can also
;; handle numbers that other languages would choke on (though I tested with a few othes and they
;; all were able to do the same with ease
'NUMBERS
(expt 53 53)
(sqrt -1)
(* (sqrt -1) (sqrt -1))

(/ 4 6) ; / = real division and 'quotient' = integer division
(/ 4.0 6) ; using a floating-point number converts the result to an approximation rather than a
;; real rational number.
(newline)

;; Strings - no surprises here.
'STRINGS
"tutti frutti"
(string-append "tutti" "frutti")
(string-append "tutti" " " "frutti")
(newline)
;; Lists in rkt (lisps)
;; lists are the main work-horse of all lisps and rkt lisps work just like others. Cons, car, cdr
;; work as expected. There is a symbol in rkt 'empty' that is new to me, and its interchangeable
;; with '(). As I think about it, its the same as nil in common lisp and I think scheme as well.
'LISTS
(cons 1 2)
(define cell (cons 1 2))
(car cell)
(cdr cell)

(cons 'chicken empty)
(cons 'chicken '())
(cons 'pork '(beef chicken))
(cons 'beef (cons 'chicken '()))
(cons 'pork (cons 'beef (cons 'chicken '())))
(list 'pork 'beef 'chicken)

(first (cons 'pork (cons 'beef (cons 'chicken empty))))
(rest (cons 'pork (cons 'beef (cons 'chicken empty))))
(first (rest '(pork beef chicken))) ;; alternative to the cadr in other lisps
(cadr '(pork beef chicken))

(list 'cat (list 'duck 'bat) 'ant)

(first '((peas carrots tomatoes)(pork beef chicken)))
(rest '(peas carrots tomatoes))
(rest (first '((peas carrots tomatoes)(pork beef chicken))))
(cdar '((peas carrots tomatoes)(pork beef chicken))) ; I actually like cadr derivitives better
;; and this helps me to understand that its really just an combination of da or ad down to 4 levels

(cons (cons 'peas (cons 'carrots (cons 'tomatoes '())))
      (cons (cons 'pork (cons 'beef (cons 'chicken '()))) '()))

;; rkt goes a step further with convenience functions for cadr with first,second,third....tenth
(second '((peas carrots tomatoes)(pork beef chicken) duck))
(third '((peas carrots tomatoes)(pork beef chicken) duck))
(second (second'((peas carrots tomatoes)(pork beef chicken) duck))) ; and they can be combined

;; NOTE: to get help inside DrRacket its easy just to put your cursor on a symbol and hit F1
(newline)

;; Structures in Racket
'STRUCTURES
;; structures are another way of packaging data in rkt. They work like other 'structs' in other
;; langs, but of course with ()s all around.
(struct student (name id# dorm)) ; same as Go for example except with all the extra type syntax
student ; created a procedure 'student' which is a structure
;; This isn't necessarily creating a particular student, but a new kind of data, which is distinct
; from all other kinds of data. its like in Go a 'student type'. When we say "creates a new kind
; of data", what we really mean is the struct def provides functions for constructing and taking
; apart student structure values.

;; We can now create an 'instance' of student
(define freshman1 (student 'Joe 1234 'NewHall)) ; 'student' here is the constructor
freshman1

;; with structs we get accessor functions automatically
(student-name freshman1)
(student-id# freshman1)
(student-dorm freshman1) ; these are also called 'field selectors' or 'selectors'

(define mimi (student 'Mimi 1234 'NewHall))
(define nicole (student 'Nicole 5678 'NewHall))
(define rose (student 'Rose 8765 'NewHall))
(define eric (student 'Eric 4321 'NewHall))
;; then we can define a list to hold all the students in:
(define in-class (list mimi nicole rose eric))
;; This structure allows us to pull out data in a very natural thought process
(student-id# (third in-class))
(newline)

;; NESTING STRUCTURES
'NESTING-STRUCTURES
;; similar to embedding structs in Go, Odin, and V, we can nest structures in rkt
(struct student-body (freshmen sophomores juniors seniors))
(define all-students
  (student-body (list freshman1 (student 'Mary 0101 'OldHall))
                (list (student 'Jeff 5678 'OldHall))
                (list (student 'Bob 4321 'Apartment))
                empty))
;; so here we created a student-body struct with fields for four years. then we give the name
;; all-students to one specific instance of student-body. And we create a list to put into
;; each of the four fields of a student-body instance

;; to get them out, we do the same thought process as before:
(student-name (first (student-body-freshmen all-students)))
(student-name (second (student-body-freshmen all-students)))
(student-name (first (student-body-juniors all-students)))
;; NOTE that we simply add '-symbol' to get to the next level of nesting as in 'student-body-juniors'
;; structs and lists are useful for organizing data into meaningful compartments that can
;; be easily accessed. We will be using structs and lists for almost all programs in this book.
(newline)

;; STRUCTURE TRANSPARENCY
'STRUCTURE-TRANSPARENCY

;; by default  rkt creates opaque stuctures, meaning you can see them in the interations window
;; as you've created them. they just print #<example>. To make then 'transparent' we need to
;; add the option #:transparent
(struct example (x y z))
(define ex1 (example 1 2 3))
ex1
(struct example2 (p q r) #:transparent)
(define ex2 (example2 9 8 7))
ex2

;; All the structures in this book are supposed to be transparent, so though we won't see the option
;; in the book, its expected that we are adding it where we need it.

#|
CHAPTER  CHECKPOINT

In this chapter, actually saw most of the building blocks of rkt's syntax and semantics:
    There are many kinds of basic data, like Booleans, symbols, numbers, and strings.
    You can make lists of data.
    You can make your own new kinds of data with structures
    You can mix it all up.

In the next chapter we'll take a look at conditions and decisions.
|#


