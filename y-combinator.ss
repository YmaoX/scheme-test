(define fact
  (lambda (n)
    (if (= n 1)
      1
      (* n (fact (- n 1))))))
(fact 6) ; 720
;without using define, try making a lambda with fact as parameter
(lambda (fact)
  (lambda (n)
    (if (= n 1)
      1
      (* n (fact (- n 1))))))
;pass it to itself with the help of (lambda (f) (f f))
((lambda (f) (f f))
 (lambda (fact)
   (lambda (n)
     (if (= n 1)
       1
       (* n (fact (- n 1)))))))
;above will not work because fact is a function which accpets a function 
;as argument (itself)
;now try (fact fact)
((lambda (f) (f f))
 (lambda (fact)
   (lambda (n)
     (if (= n 1)
       1
       (* n ((fact fact) (- n 1)))))))
;test and it works!
(((lambda (f) (f f))
 (lambda (fact)
   (lambda (n)
     (if (= n 1)
       1
       (* n ((fact fact) (- n 1))))))) 6)
;refactoring => take (fact fact) out 
;Scheme is an applicative-order language, namely, that all the arguments to 
;Scheme procedures are evaluated when the procedure is applied. In contrast, 
;normal-order languages delay evaluation of procedure arguments until the actual 
;argument values are needed.
;if we pass (fact fact) as argument, it will be evaluated immediately
;so we wrap it with a lambda
(((lambda (f) (f f))
  (lambda (fact)
    ((lambda (fact2)
      (lambda (n)
        (if (= n 1)
          1
          (* n (fact2 (- n 1)))))) 
     (lambda (x) ((fact fact) x))))) 
 6) ;still works
;note fact2 is the same as the initial lambda fact
;rename
((lambda (f) (f f))
 (lambda (y)
   ((lambda (fact)
      (lambda (n)
        (if (= n 1)
          1
          (* n (fact (- n 1)))))) 
    (lambda (x) ((y y) x))))) 
;take the fact function out as parameter and test
(((lambda (r)
    ((lambda (f) (f f))
     (lambda (y)
       (r (lambda (x) ((y y) x))))))
  (lambda (fact)
    (lambda (n)
      (if (= n 1)
        1
        (* n (fact (- n 1))))))) 
 6) 
;=>
(define Y
  (lambda (r)
    ((lambda (f) (f f))
     (lambda (y)
       (r (lambda (x) ((y y) x)))))))
((Y (lambda (fact)
    (lambda (n)
      (if (= n 1)
        1
        (* n (fact (- n 1))))))) 6)
