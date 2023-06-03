;; #!/usr/bin/env csi -s

;; Run the CHICKEN REPL in the commandline as follows :
;; $ csi

;; importing modules
(import
	(chicken format)
	(chicken sort)
	(chicken string)) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 0. Syntax
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Single line comments start with a semicolon

#| Block comments
   can span multiple lines and...
   #| can be nested
   |#
|#

;; S-expression comments are used to comment out expressions
#; (display "nothing")    ; discard this expression 

;; CHICKEN has two fundamental pieces of syntax: Atoms and S-expressions
;; an atom is something that evaluates to itself
;; all builtin data types viz. numbers, chars, booleans, strings etc. are atoms
;; Furthermore an atom can be a symbol, an identifier, a keyword, a procedure
;; or the empty list (also called null)
'athing              ;; => athing 
'+                   ;; => + 
+                    ;; => <procedure C_plus>

;; S-expressions (short for symbolic expressions) consists of one or more atoms
(quote +)            ;; => + ; another way of writing '+
(+ 1 2 3)            ;; => 6 ; this S-expression evaluates to a function call
'(+ 1 2 3)           ;; => (+ 1 2 3) ; evaluates to a list 


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 1. Primitive Datatypes and Operators 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Numbers
99999999999999999999 ;; integers
#b1010               ;; binary ; => 10
#o10                 ;; octal  ; => 8
#x8ded               ;; hexadecimal ; => 36333
3.14                 ;; real
6.02e+23
3/4                  ;; rational

;;Characters and Strings
#\A                  ;; A char
"Hello, World!"      ;; strings are fixed-length arrays of characters

;; Booleans
#t                  ;; true
#f                  ;; false

;; Function call is written as (f x y z ...)
;; where f is a function and x,y,z, ... are arguments
(print "Hello, World!")    ;; => Hello, World!
;; formatted output
(printf "Hello, ~a.\n" "World")  ;; => Hello, World.

;; print commandline arguments
;; (map print (command-line-arguments)) 

(list 'foo 'bar 'baz)          ;; => (foo bar baz)
(string-append "pine" "apple") ;; => "pineapple"
(string-ref "tapioca" 3)       ;; => #\i;; character 'i' is at index 3
(string->list "CHICKEN")       ;; => (#\C #\H #\I #\C #\K #\E #\N)
(string-intersperse '("1" "2") ":") ;; => "1:2"
(string-split "1:2:3" ":")     ;; => ("1" "2" "3")


;; Predicates are special functions that return boolean values
(atom? #t)                ;; => #t

(symbol? #t)              ;; => #f

(symbol? '+)              ;; => #t

(procedure? +)            ;; => #t

(pair? '(1 2))            ;; => #t

(pair? '(1 2 . 3))        ;; => #t

(pair? '())               ;; => #f

(list? '())               ;; => #t


;; Some arithmetic operations

(+ 1 1)                   ;; => 2
(- 8 1)                   ;; => 7
(* 10 2)                  ;; => 20
(expt 2 3)                ;; => 8
(remainder 5 2)           ;; => 1
(/ 35 5)                  ;; => 7
(/ 1 3)                   ;; => 0.333333333333333

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 2. Variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; You can create variables with define
;; A variable name can use any character except: ()[]{}",'`;#\ 
(define myvar 5)
myvar        ;; => 5

;; Alias to a procedure
(define ** expt)
(** 2 3)     ;; => 8

;; Accessing an undefined variable raises an exception
;; s            ;; => Error: unbound variable: s

;; Local binding
(let ((me "Bob"))
  (print me))     ;; => Bob

;; (print me)        ;; => Error: unbound variable: me

;; Assign a new value to previously defined variable
(set! myvar 10) 
myvar             ;; => 10


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 3. Collections
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Pairs
;; 'cons' constructs pairs, 
;; 'car' extracts the first element, 'cdr' extracts the rest of the elements
(cons 'subject 'verb)       ;; => '(subject . verb)
(car (cons 'subject 'verb)) ;; => subject
(cdr (cons 'subject 'verb)) ;; => verb

;; Lists
;; cons creates a new list if the second item is a list
(cons 0 '())         ;; => (0)
(cons 1 (cons 2  (cons 3 '())))    ;; => (1 2 3)
;; 'list' is a convenience variadic constructor for lists
(list 1 2 3)    ;; => (1 2 3)


;; Use 'append' to append lists together
(append '(1 2) '(3 4)) ;; => (1 2 3 4)

;; Some basic operations on lists
(map add1 '(1 2 3))    ;; => (2 3 4)
(reverse '(1 3 4 7))   ;; => (7 4 3 1)
(sort '(11 22 33 44) >)   ;; => (44 33 22 11)

(define days '(SUN MON FRI))
(list-ref days 1)      ;; => MON
(set! (list-ref days 1) 'TUE)
days                   ;; => (SUN TUE FRI)


;; Vectors
;; Vectors are heterogeneous structures whose elements are indexed by integers
;; a Vector typically occupies less space than a list of the same length
;; Random access of an element in a vector is faster than in a list
;; so these vectors are more like the lists I'm used to working with while the scheme or
;; lisp list is more of a linked-list structure or chain of conses
(print #(1 2 3))
(print (vector 'a 'b 'c)) 
(print (vector? #(1 2 3)))
(print (vector-length #(1(2) "a"))) 
(print (vector-ref #(1(2)(3 3))2)) 

(define vec #(1 2 3))
(vector-set! vec 2 4)
(print vec) 

;; Vectors can be created from lists and vice versa
(print(vector->list #(1 2 4)))
(print(list->vector '(a b c)))

;; Functions
;; First use lambda to create functions
;; A function always returns the value of its last expression
;; so there is no mystery here with lambda. Its just like like func or function or fn or any
;; of the other keywords used specifically to create anony funcs in other langs
(lambda () "Hello world")
;; we an execute by putting another () around it, just like the () at the end of an anony func
(print((lambda () "Hello world")))

;; a function with an argument 
(print((lambda (x) (* x x )) 3)) 

;; a function with 2 args 
(print((lambda (x y)(* x y))2 3)) 

;; Now let's assign a function to a variable. 
(define sqr (lambda(x)(* x x))) ;; same lambda but using define to put it in square 
(print sqr)
(print (sqr 3))

;; as I learned on scip vids, we have the sytantic sugar of just defining the function directy
;; but the above is still happening under the hood.
(define (sqr2 x)(* x x)) 
(print (sqr 5))

;; we can also redefine exisiting functions if we want to 
(print (foldl cons '() '(1 2 3 4 5))) 
(define (foldl func accu alist)
  (if (null? alist)
    accu
    (foldl func (func (car alist) accu)(cdr alist))))

(print (foldl cons '() '(1 2 3 4 5))) 

;; Equity
;; For numbers  use =
(print (= 3 3.0)) 
(print (= 2 1)) 

;; eq? returns #t if two arguments actually refer to the same object in memory (they are the same)
;; so basically its a pointer comparison
(print(eq? '() '())) ;; there is only one empty list in memory. 
(print(eq? (list 3) (list 3))) ;; creating two different lists 

(print (eq? 'yes 'yes)) 
(print (eq? 3 3)) ;; although this works, its not safe to rely on it
(print (eq? 3 3.0)) ;; better to use = as eq? won't consider 3 and 3.0 the same
(print (eq? "hello" "hello")) 

;; eqv? is the same as eq? all datatypes except numbers and chars
(print (eqv? 3 3.0)) 
(print (eqv? (expt 2 3)(expt 2   3))) 
(print (eqv? 'yes 'yes)) 

;; then equal? will recursively compare contents of pairs, vectors, and strings using eqv?
;; on other objects such as numbers and symbols.
;; A rule of thumb is that objects are generally 'equal?' if they print the same
(print (equal? '(1 2 3) '(1 2 3))) 
(print (equal? #(a b c) #(a b c)))
(print (equal? 'a 'a)) 
(print (equal? "abc" "abc")) 

;; In summary:
;; eq? tests if objects are identical
;; eqv? tests if objects are operationally equivalent
;; equal? tests if objects have same structure and contents

;; and then for strings
(print (string=? "Hello" "Hello")) 


;; Control Flow
;; Conditionals - The next two conditional forms are 'special forms' and they are simply
;; syntactic sugar for each other, depending on the implementation
(if #t
	(print"True")
	(prin "False")) 
	
(if (> 3 2)
	(print "Yes")
	(print "No")) 
	
;; in conditionals, all values that are not '#f' are treated as true, so...
;; 0, '(), #(), "", are all true values
(if 0
	(print "0 is not false")
	(print "0 is false")) 
	
;; a case expression is evaluated as follows:
;; the key is evaluated and compared with each datum in sense of eqv?,
;; The corresponding clause (()) is evaluated and returned as a result
(case (* 2 3)
	((2 3 5 7) (print'prime))
	((1 4 6 8) (print 'composite)))
	
;; case with else clause 
(case (cadr '(c a y))
	((a e i o u) (print 'vowel))
	((w y) (print 'semivowel))
	(else (print 'consonant)))


;; Boolean expressions
;; 'and' returns the first expression that evaluates to #f
;; othewise, it returns the result of the last expression
(print (and #t #f (= 2 2.0))) 
(print (and (< 2 5)(> 2 0) "0 < 2 < 5")) ;; since everything is true the result is the last expr

;; 'or' retuns the first expr that evals to #t, otherwise the result of the last expr
(print (or #f #t #f)) 
(print (or #f #f #f)) 

;; 'when' is like 'if' without the else expression
(print (when (positive? -5) "I'm positive")) ;; NOTE: when we print here, if there is no return result
;; then we get #<unspecified>

;; 'unless' is equivalent to (when (not <test>) <expr>) - I was asking that same thing ;)
(print (unless (null? '(1 2 3)) "not null"))


;; Loops
;; loops can be created with the help of tail-recursions in scheme
(define (loop count)
	(unless (= count 0) ;; base case
		(print "Hello")
		(loop (sub1 count)))) ;; this means that just like eveyrthing I've learned about recursion
		;; no difference here. We have our 'base case' and then the last expression adjust counter
(loop 4)
 
;; we can also use a named let
(let loop ((i 0) (limit 5))
	(when (< i limit)
		(printf "i = ~a\n" i)
		(loop (add1 i) limit)))
		
;; 'do' is another iteration construct
;; It initializes a set of vars and updates the in each iteration
;; A final expression is evaluated after the exit condition is met. 
(do ((x 0 (add1 x)))
	;; each iteration
	((= x 10) (print "done"))
	;;expression
	(print x))
	;; step
	
;; Iteration over lists
(for-each (lambda(a)(print (* a a)))
	'(3 5 7)) 
;; map is like for-each, but returns a list
(print (map (lambda(a)(* a a)) '(3 5 7)))
(print(map add1 '(11 22 33)))


;; Extensions
;; The chicken core is very minimal, but additional features are provided by library exts
;; known as eggs. 
;; you install eggs with 'chicken-install <eggname>'

;;(import utf8) ;; I think this stuff is already built-in now???
;; complex numbers
(print 3+4i) 
(print 1/3) 
(print (expt 9 20))
;; and other extended functions
(print (log 10 (exp 1))) 
(print (numerator 2/3))
(print (denominator 2/3)) 


(print "😀")
(print "\u03BBx:(\u03BC\u0251.\u0251\u2192\u0251).xx")

;; The 'posix' egg provides file i/o, but again i think some of this stuff is already included
;; now
(import (chicken file posix)) 

;; open a file to append, open "write only" and create file if it doesn't exist
(define outfn (file-open "chicken-hen.txt" (+ open/append open/wronly open/creat))) 
;; write some text to the file
(file-write outfn "Did chicken came before hen?\n") 
;; close the file
(file-close outfn)
;; open the file "read only"
(define infn (file-open "chicken-hen.txt" open/rdonly))
;; read some text from the file
(print (file-read infn 30))
(file-close infn)

(import srfi-1) 
;; chicken also supports srfi
(print (filter odd? '(1 2 3 4 5 6 7))) 
(print (count even? '(1 2 3 4 5 6 7)))
(print (take '(12 24 36 48 60)3))
(print (drop '(12 24 36 48 60)2))
;; (print (circular-list 'z 'q)) 

(import srfi-13)
(print (string-reverse "pan")) 
(print (string-index "Turkey" #\k)) 
(print (string-every char-upper-case? "CHICKEN")) 
(print (string-join '("foo" "bar" "baz") ":")) 


;; Macros
;; now we get to macros where can create our own functionality.
;; here we create a for..in iteration like other scripting langs for lists
(define-syntax for
	(syntax-rules (in)
		((for elem in alist body ...)
			(for-each (lambda(elem) body ...) alist)))) 
(for x in '(2 4 8 16)
	(print x))

(for chr in (string->list "PENCHANT")
	(print chr)) 

;; Now lets create a while loop
(define-syntax while
	(syntax-rules ()
		((while cond body ...)
			(let loop ()
				(when cond
					body ...
					(loop))))))
(let ((str "PENCHANT")(i 0))
	(while (< i (string-length str))
		(printf "~a => " i)
		(print (string-ref str i))
		(set! i (add1 i))))
 

;; Modules
;; the test module exports a value name 'hello' and a macro named 'greet'
;; Then we can load it into the interpreter with (load "test.ss") 
(load "test.ss") ;; we had to load it before we could import it
(import test)
(hello)
(greet "little schemers") 



