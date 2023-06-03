(define (fact n)
    (if (< n 2)
        1
        (* n (fact (- n 2)))))
    
(pretty-print (fact 100))

