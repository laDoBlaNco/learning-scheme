;; The test module exports a value name 'hello' and a macro
;; named 'greet'
(module test (hello greet)
	(import scheme)
	
	(define-syntax greet
		(syntax-rules ()
			((_ whom) ; not sure what the _ is for
				(begin
					(display "Hello, ")
					(display whom)
					(display " !\n")))))
	(define (hello)
		(greet "world"))) 

