#lang racket
(require rackunit) 
#|
We've now seen rkt's simple syntax and a bunch of different kinds of data. But can we write
a program that answers questions about some piece of data? And can we write a program that
chooses different values depending on the answers to those questions? In this chapter, we'll
look at PREDICATES and diffent forms of CONDITIONALS. Among them is an extremely elegant
multi-branch conditional that racketeers (and most lispers) use as their major workhorse
for many of their programming tasks.
|#

;; Typically the behavior of conditionals is dependent on the questions being asked and answered.
;; so we're going to look at how we ask those questions first. We do that with PREDICATES. A
;; predicate is just any function that returns a boolean #t or #f, that's it. Typically these
;; end with a '?' on the name, just like in common lisp which ends all predicates with a 'p'.
;; schemes convention makes more sense.
;; NOTE: something I didn't know about racket is that even though #t and #f are mainly what's used
;; we can also use #true #false and true and false as well.
#t #true true
#f #false false

;; here are several preds we've seen already
zero?
symbol=?
;; student? - but no one told us about this one, but it was there automatically created with our
;; struct

(zero? 42)
(zero? 0)
(symbol=? 'a 'b)

;; now the secret predicates that was automatically created
(struct student (name id# dorm) #:transparent)
(define sophomore3 (student 'David 100234 'PG))
(student-name sophomore3)
;; with this we also get a predicate along with the other accessors
(student? 'a)
(student? sophomore3)
(student? (student 1 2 3))
(student? "I am a student")
;; the struct predicate just checks to see if its value is an instance of the structure or not.

;; These are call 'type predicates' as they deal specifically with differentiating one type of data
;; from another. All built-in forms of data come with a type-predicate
(number? 'a)
(string? "hello world")
(symbol? 'a)
;(image? 10) ; apparently this needs to be imported from another package. 
(boolean? "false")
(list? 'eh)
(cons? '(what is that aboot?))
(empty? 'a)
;; more for numbers
(real? 10)
(real? (sqrt -1))
(rational? 2/3)
(rational? 1.0)
(integer? 1)
(exact-integer? 1.0) 

;; Little  bit more to preds - these below are all equality predicates
(= 1 2)
(= (sqrt -1) 0+1i)
(boolean=? #f #f)
(string=? "hello world" "good bye")
(equal? (student 'David 100234 'PG) sophomore3)
;; NOTE that most basic forms of data come with their own pred, but for those that don't we can
;; use the equal? pred. This compares absolutely everything.
(newline)
'EQUALITY-PREDICATES
(equal? '(1 2 3) '(1 2 3))
(equal? 'a 'b)
(equal? "hello world" 'a)
(equal? 10 10)
(equal? #t 10)

;; so why so many predicates? Because since we don't worry too much about types in rkt we need
;; a way to check them when we need to
;; For example, we could do
(define (add-to-front-of-123 x)
  (cons x '(1 2 3)))

(add-to-front-of-123 'a)
(add-to-front-of-123 0)
(add-to-front-of-123 '(a b c))
;; rkt does that without asking or caring what types we are mixing up here. That's why racketeers
;; tend to use predicates to tell the reader of their programs about the kind of data they are
;; dealing with.
(newline)

;; Now let's get to the conditionals:
;; THE CONDITIONALS: IF AND BEYOND
'IF-AND-BEYOND:
;; oen thing at a time with if
(if (= (+ 1 2) 3)
    'yup
    'nope)
;; as with other lisps, anything that's not #f is considered #t
(if '(1)
    'everything-except-#f-counts-as-#t
    'aw-heck-no)

(if empty
    'everything-except-#f-counts-as-#t
    'aw-heck-no)

(if false
    'everything-except-#f-counts-as-#t
    'aw-heck-no)

;; using if with predicate questions
(if (odd? 5) 'odd-number 'even-number)

;; as with other langs, in if only one of the code snippets is evaluated, unlike what we've seen
;; previously in rkt
(if (odd? 5)
    'odd-number
    (/ 1 0)) ; never  gets here

;; 'if' is a special form like 'define' in that it doesn't follow the normal rules for functions.
(newline)

;; THE SPECIAL FORM THAT DOES IT ALL
'COND:

;; we could make more than one decision using nested ifs, but that gets complex very quickly
(define x 7)
(if (even? x) ; this is the kinda programming I started  with in excel
    'even-number
    (if (= x 7)
        5
        'odd-number))

(cond [(= x 7) 5]
      [(odd? x) 'odd-number]
      [else 'even-number])
;; This is where I'd rather go with the other lisps and just use ()s, but the idea is that each
;; conditional clause is surrounded in []
;; but this still works
(cond ((= x 7) 5)
      ((odd? x) 'odd-number)
      (else 'even-number))
(newline)

;; A FIRST TASTE OF RECURSION
'FIRST-TASTE-OF-RECURSION:
;; know need to fear recursion, in fact thinking recursively will make us better coders
;; Now that we know how to 'ask' questions, we can determine if we are at the end of a list
;; or not.

;; Recursion all just boils down to one point: we can take items from the front of a list and send
;; the rest of the list back to the same function until the list is empty. that process is called
;; a list-eater, and it sure is a good thing that detecting empty lists is easy, becasue so very
;; many functions in rkt end up being list-eaters.
(define (my-length a-list)
  (if (empty? a-list) ; base case, if list is empty return 0
      0
      (add1 (my-length(rest a-list))))) ; recursive case - +1 to each result of the recursion

(my-length '(list with four symbols))
(my-length '(42))

(newline)

;; STEALTH CONDITIONALS AND / OR
'AND-OR
(set! x 5)
(define y 7)
(define z 9)
(and (odd? x)(odd? y)(odd? z))

(define w 4)
(or (odd? w)(odd? y)(odd? z))

;; something we can use and/or for is to code conditional behavior in a more concise manner
(define is-it-even #f)
is-it-even

(or (odd? x) (set! is-it-even #t))
(and (even? x) (set! is-it-even #t))
is-it-even

;; this is due to short-circuit conditionals. and / or stop evaluating once they hit either false
;; or true, as they don't need to continue in order to return a result. If we add this to the fact
;; the last expr is always evaluated, then the last expression doesn't have to be a conditional
;; it can be an action we want to take place, depending on whether or not and/or gets that far.
(and (odd? 5) (even? 5) (/ 1 0)) ; no error cuz we never get to the last expr, rkt just returns
;; the first #f encountered.

;; We can use this type of conditional magic in 3 different ways.
;;;; The normal if and nested ifs
;;;; The short-circuiting and/or
;;;; A combo of both using 'when'. 'when' is just like if without an else branch. The keyword
;;;;; signals that the program will test a condition and then perform an action, but only if #t
;;;; There is also a 'unless' form which does the opposite.
(newline)

;; RETURNING MORE THAN JUST THE TRUTH
'RETURNING-MORE-THAN-JUST-THE-TRUTH

;; since any value other than #f counts as #t, we can use functions in conditions to return
;; more than just the truth. For example, the funtion member can be used to check for a member
;; of a list (this is the same in most lisps)
(if (member 4 (list 3 4 1 5)) '4-is-in 'not-in)

;; this looks normal, but there's something else happening behind the scenes that I studied in
;; LoL and PCL when learning common lisp. 'member' isn't return just #t or #f. Since anything
;; not #f is #t, it takes advantage and returns something else insteasd of #t
(member 1 '(3 1 4 5))
(define tasks '(1 clean 3 homework 4 party))
tasks
(member 3 tasks) ; and of course this result also counts as #t so it can be used in an cond as well
(newline)

;; EQUALITY PREDICATES, ONCE MORE
'MORE-EQUAL-PREDS

(struct point (x y) #:transparent) ; remember this results in the following functions to work with
point
point-x
point-y
point?

;; with these we can create other functions
(define (distance-to-origin p)
  (sqrt (+ (sqr (point-x p)) (sqr (point-y p)))))

(distance-to-origin (point 3 4))
(distance-to-origin (point 12 5))

;; let's give names to our points
(define pt1 (point -1 2))
(define pt2 (point -1 2))
(equal? pt1 pt2) ; these are equal cuz they are both points and their fields are the same value
;; But what if you don't want to compare the pieces, but you want to know if they were created
;; from the same constructor call, for this we have eq?
(eq? pt1 pt2) ;; this is now false cause they aren't the SAME in that they aren't the exact same
;; concrete object and not created at the same time
(eq? pt1 pt1)
(eq? pt2 pt2)

(define pt3 pt1)
(eq? pt1 pt3)

;; it even works on composite data
(define (eq-first-items list1 list2)
  (eq? (first list1)(first list2)))

(eq-first-items (cons pt1 empty) (cons pt3 empty))
(eq-first-items (cons pt1 empty) (cons pt2 empty))

;; This becomes critical when we get to what is similar to pointers in other langs. When we
;; learn how to change fields of a struct, and this in turn means that if certain fields are eq?
;; changing one will in turn change another, but if they are just equal? then they won't
(newline)

;; COMPARING AND TESTING
'COMPARING-AND-TESTING

;; The reason why equality is so important is becasue when you write programs, you should create
;; tests for almost all the functions in your code. Actually you should write the tests before
;; you even write the functions, in some trains of thought. A test contains 2 things:
;;;; a function call
;;;; an expected result

;; rkt comes with several libraries for testing. One of which is 'rackunit which we add as
;; require at the type of the program (hit F1 on 'rackunit' to get the full docs)

;; The most common used function from rackunit is 'check-equal?
(check-equal? (add1 5 ) 7) ; if it passes, nothing happens, but if it fails we get a failure msg
;; with plenty of detail

;; Testing in the real world means that there are messages telling us about the test, etc
;; there's a 3rd argument to 'check-equal?' which is a string
(check-equal? 5 6 "NUMBERS MATTER!")

;; though we don't go through test suites here, we can see some examples in the pkgs/realm folder
;; Here are some other testing functions
(check-not-equal? 5 5)
(check-pred number? "hello")
(check-= 1 10 5) ; is 1 and 10 within a range of 5 from each other?
(check-true (odd? 70))
(check-false (odd? 71))
(check-not-false (member 5 '(1 2 6))) ; slightly different from check-true

#|
CHAPTER CHECKPOINT

In this chapter, we learned about predicates, conditionals, and testing:
-- Racket has predicates, including type and equality predicates
-- There are many conditionals, but if and cond are the most important ones
-- conditionals tend to use predicates to ask questions about values, and the rest of the
   computation depends on the questions' results
-- the rackunit library is for writing tests
-- to test means to compare the outcome of exprs with expected answers
-- All of our programs come with tests, but we dont show them in this book.
|#
