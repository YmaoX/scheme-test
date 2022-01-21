;an entry is a pair of lists whose first list is a
;set. Also the two lists must be of equal length.
;a table (environment) is a list of entries
(define first
  (lambda (p) (car p)))
(define second
  (lambda (p) (car (cdr p))))
(define third
  (lambda (p) (car (cdr (cdr p)))))

(define lookup-in-entry-help
  (lambda (name names values entry-f)
    (cond 
      ((null? names) (entry-f name))
      ((equal? name (car names)) (car values))
      (else (lookup-in-entry-help name (cdr names) (cdr values) entry-f)))))
(define lookup-in-entry
  (lambda (name entry entry-f)
    (lookup-in-entry-help name
                          (first entry)
                          (second entry)
                          entry-f)))
(define lookup-in-table
  (lambda (name table table-f)
    (cond 
      ((null? table) (table-f name))
      (else (lookup-in-entry name (car table)
                             (lambda (name)
                               (lookup-in-table name (cdr table) table-f)))))))
;we choose functions (actions) to represent types
(define atom-to-action 
  (lambda (e)
    (cond 
      ((number? e) *const) 
      ((equal? e #t) *const)
      ((equal? e #f) *const)
      ((equal? e (quote cons)) *const) 
      ((equal? e (quote car)) *const) 
      ((equal? e (quote cdr)) *const) 
      ((equal? e (quote null?)) *const) 
      ((equal? e (quote eq?)) *const) 
      ((equal? e (quote atom?)) *const) 
      ((equal? e (quote zero?)) *const) 
      ((equal? e (quote add1)) *const) 
      ((equal? e (quote sub1)) *const) 
      ((equal? e (quote number?)) *const) 
      (else *identifier))))
(define list-to-action 
  (lambda (e) 
    (cond 
      ((atom? (car e)) 
       (cond ((equal? (car e) (quote quote)) *quote) 
             ((equal? (car e) (quote lambda)) *lambda) 
             ((equal? (car e) (quote cond)) *cond) 
             (else *application))) 
      (else *application))))
(define expression-to-action 
  (lambda (e) 
    (cond ((atom? e) (atom-to-action e)) 
          (else (list-to-action e)))))
;use expression-to-action to define "value" and "meaning"
;value is called an interpreter
(define value
  (lambda (e)
    (meaning e (quote ()))))
(define meaning
  (lambda (e table)
    ((expression-to-action e) e table)))
;now we define each action
(define build
  (lambda (s1 s2)
    (cons s1 (cons s2 '()))))
;interpreter needs to 1. bind variable, and 2 evaluate the expression
;so it needs to remember the value in ENV, and pass it to the right scope
;here we add an entry on top of the table, like a stack
(define new-entry build)
(define extend-table cons)

(define text-of second)
(define *quote
  (lambda (e table)
    (text-of e)))

(define initial-table
  (lambda (name)
    (car (quote ()))))
;initial-table shouldn't be called, otherwise it means we are looking for an undefined variable
(define *identifier
  (lambda (e table)
    (lookup-in-table e table initial-table)))

(define *const
  (lambda (e table)
    (cond
      ((number? e) e)
      ((equal? e #t) #t)
      ((equal? e #f) #f)
      (else (build (quote primitive) e)))))
;lambda looks like '(lambda (param) body)
;here (cons table (cdr e)) actually creates a closure 
;closure = (function + the environment the function sees when it's defined) -> lexical scoping
(define *lambda
  (lambda (e table)
    (build (quote non-primitive)
           (cons table (cdr e))))) ; (cdr e) is (param body)
;helper functions
(define table-of first) (define formals-of second) (define body-of third)

(define question-of first) (define answer-of second)
(define else?
  (lambda (x)
    (cond
      ((atom? x) (equal? x (quote else)))
      (else #f))))
(define evcon
  (lambda (lines table)
    (cond
      ((else? (question-of (car lines)))
       (meaning (answer-of (car lines)) table))
      ((meaning  (question-of (car lines)) table)
       (meaning (answer-of (car lines)) table))
      (else (evcon (cdr lines) table)))))
(define cond-lines-of cdr)
(define *cond
  (lambda (e table)
    (evcon (cond-lines-of e) table)))
;evaluate all the arguments, return a list of values
(define evlis
  (lambda (args table)
    (cond
      ((null? args) (quote ()))
      (else (cons (meaning (car args) table) (evlis (cdr args) table))))))
(define function-of car) (define arguments-of cdr)
(define primitive?
  (lambda (l)
    (equal? (first l) (quote primitive))))
(define non-primitive?
  (lambda (l)
    (equal? (first l) (quote non-primitive))))
;fun starts with 'primitive or 'non-primitive
;check the definition of *const and *lambda
(define :atom?
  (lambda (x)
    (cond 
      ((atom? x) #t)
      ((null? x) #f)
      ((equal? (car x) (quote primitive))
       #t)
      ((equal? (car x) (quote non-primitive))
       #t)
      (else #f))))
(define apply-primitive
  (lambda (name vals)
    (cond
      ((equal? name (quote cons))
       (cons (first vals) (second vals)))
      ((equal? name (quote car))
       (car (first vals)))
      ((equal? name (quote cdr))
       (cdr (first vals)))
      ((equal? name (quote null?))
       (null? (first vals)))
      ((equal? name (quote eq?))
       (equal? (first vals) (second vals)))
      ((equal? name (quote atom?))
       (:atom? (first vals)))
      ((equal? name (quote zero?))
       (zero? (first vals)))
      ((equal? name (quote add1))
       (add1 (first vals)))
      ((equal? name (quote sub1))
       (sub1 (first vals)))
      ((equal? name (quote number?)))
      (number? (first vals)))))
(define apply-closure
  (lambda (closure vals) ;closure is (table "function formal" "function body")
    (meaning (body-of closure)
             (extend-table
               (new-entry (formals-of closure) vals)
               (table-of closure))))) ;extend the table we see when the function is defined
(define apply
  (lambda (fun vals)
    (cond
      ((primitive? fun) 
       (apply-primitive (second fun) vals)) 
      ((non-primitive? fun)
       (apply-closure (second fun) vals)))))
(define *application
  (lambda (e table)
    (apply
      (meaning (function-of e) table)
      (evlis (arguments-of e) table))))
;test
(value (quote ((lambda (x) (sub1 x)) 3)))

