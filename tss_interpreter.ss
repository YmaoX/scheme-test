(define first (lambda (p) (car p)))
(define second
  (lambda (p) (car (cdr p))))
(define third
  (lambda (p) (car (cdr (cdr p)))))
(define text-of second) 
(define formals-of second)
(define body-of
  (lambda (x)
    (cdr (cdr x))))
(define ccbody-of body-of)
(define name-of
  (lambda (e)
    (car (cdr e))))
(define right-side-of
  (lambda (e)
    (cond
      ((null? (body-of e)) 0)
      (else (third e)))))

(define abort #f)
(define the-empty-table
  (lambda (name)
    (abort
      (cons (quote no-answer) (cons name (quote ()))))))
(define global-table the-empty-table)

;(box it) return a function which accepts a selector to access the values
(define box
  (lambda (it)
    (lambda (sel)
      (sel it (lambda (new)
                (set! it new))))))
(define setbox
  (lambda (box new)
    (box (lambda (it set) (set new)))))
(define unbox
  (lambda (box)
    (box (lambda (it set) it))))
;(define abox (box 17))
;(unbox abox) ;17
;(setbox abox 32)
;(unbox abox) ;32

;need to define what an empty table is
;use function to make tables
(define lookup
  (lambda (table name)
    (table name)))
;extend returns a similar function as table
(define extend
  (lambda (name1 value table)
    (lambda (name2)
      (cond 
        ((equal? name2 name1) value)
        (else (table name2))))))
;test
;(set! global-table (extend "a" (box 10) global-table))
;(unbox (lookup global-table "a"))

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
       (cond
         ((equal? (car e) (quote quote)) *quote)
         ((equal? (car e) (quote lambda)) *lambda)
         ((equal? (car e) (quote letcc)) *letcc)
         ((equal? (car e) (quote set!)) *set)
         ((equal? (car e) (quote cond)) *cond)
         (else *application)
         ))
      (else *application))))
(define expression-to-action
  (lambda (e)
    (cond
      ((atom? e) (atom-to-action e))
      (else (list-to-action e)))))

(define meaning
  (lambda (e table)
    ((expression-to-action e) e table)))
(define lookup-in-global-table
  (lambda (name)
    (lookup global-table name)))
(define the-meaning
  (lambda (e)
    (meaning e lookup-in-global-table)))
;define changes the global-table
(define define?
  (lambda (e)
    (cond
      ((atom? e) #f)
      ((atom? (car e))
       (equal? (car e) (quote define)))
      (else #f))))
;extend global table with this defined name and it's value
(define *define
  (lambda (e)
    (set! global-table
      (extend
        (name-of e)
        (box
          (the-meaning
            (right-side-of e)))
        global-table))))
(define value
  (lambda (e)
    (call/cc
      (lambda (the-end)
        (set! abort the-end)
        (cond 
          ((define? e) (*define e))
          (else (the-meaning e)))))))
(define *quote
  (lambda (e table)
    (text-of e)))
(define *identifier
  (lambda (e table)
    (unbox (lookup table e))))
(define *set
  (lambda (e table)
    (setbox (lookup table (name-of e)) 
            (meaning (right-side-of e) table))))

;determines the values of a list of expressions, one at a
;time, and returns the value of the last one
(define beglis
  (lambda (es table)
    (cond
      ((null? (cdr es))
       (meaning (car es) table))
      ;first determine (meaning (car es) table) and then continue the rest
      (else ((lambda (val)
               (beglis (cdr es) table))
             (meaning (car es) table))))))
(define box-all
  (lambda (vals)
    (cond
      ((null? vals) (quote ()))
      (else (cons (box (car vals))
                  (box-all (cdr vals)))))))
(define multi-extend
  (lambda (names values table)
    (cond
      ((null? names) table)
      (else (extend (car names) (car values)
              (multi-extend (cdr names) (cdr values) table))))))
;*lambda is different from what we had in little schemer
;it returns a function which you can call with the arguments
(define *lambda
  (lambda (e table)
    (lambda (args)
      (beglis (body-of e)
              (multi-extend
                (formals-of e)
                (box-all args)
                table)))))
(define evlis
  (lambda (args table)
    (cond
      ((null? args) (quote ()))
      (else 
        ((lambda (val)
           (cons val (evlis (cdr args) table)))
         (meaning (car args) table))))))

(define function-of car) (define arguments-of cdr)
;basically all functions with arguments
(define *application
  (lambda (e table)
    ((meaning (function-of e) table)
     (evlis (arguments-of e) table))))
;for (meaning (quote car) table), we need to have
;(define :car
  ;(lambda (args-in-a-list)
    ;(car (car args-in-a-list))))
;above is the same for all 1 argument functions
(define a-prim
  (lambda (p)
    (lambda (args-in-a-list)
      (p (car args-in-a-list)))))
(define b-prim
  (lambda (p)
    (lambda (args-in-a-list)
      (p (car args-in-a-list)
         (car (cdr args-in-a-list))))))

;odd? and even? as recursive functions
(define odd?
  (lambda (n)
    (cond
      ((zero? n) #f) 
      (else (even? (sub1 n))))))
(define even?
  (lambda (n)
    (cond
      ((zero? n) #t)
      (else (odd? (sub1 n))))))
;below equals:
 ;(define *const
   ;(let ((:cons (b-prim cons)))
     ;...
     ;(lambda (e table)
       ;(cond
         ;((eq? e (quote cons)) :cons)
         ;...
         ;))))
;the idea is to evaluate onl once each (x-prim x)
(define *const
  ((lambda (:cons :car :cdr :null? :eq? :atom? :zero? :add1 :sub1 :number?)
     (lambda (e table)
       (cond
         ((number? e) e)
         ((equal? e #t) #t)
         ((equal? e #f) #f)
         ((equal? e (quote cons)) :cons)
         ((equal? e (quote car)) :car)
         ((equal? e (quote cdr)) :cdr)
         ((equal? e (quote null?)) :null?)
         ((equal? e (quote eq?)) :eq?)
         ((equal? e (quote atom?)) :atom?)
         ((equal? e (quote zero?)) :zero?)
         ((equal? e (quote add1)) :add1)
         ((equal? e (quote sub1)) :sub1)
         ((equal? e (quote number?)) :number?))))
   ;below are arguments, this is a function application
   (b-prim cons)
   (a-prim car)
   (a-prim cdr)
   (b-prim equal?)
   (a-prim atom?)
   (a-prim null?)
   (a-prim zero?)
   (a-prim add1)
   (a-prim sub1)
   (a-prim number?)))

(define *letcc
  (lambda (e table)
    (call/cc
      (lambda (skip)
        (beglis (ccbody-of e)
                (extend
                  (name-of e)
                  (box (a-prim skip))
                  table))))))

;test
(value (quote (define tt
                (lambda (x)
                  (add1 x)))))
(value (quote (tt 3))) ;4

(value (quote (define cc-test
                (lambda (x)
                  (letcc cont
                         (+ 2 (cont x))))))) 
(value (quote (cc-test 3))) ;3

(value (quote (value 1))) ;(no-answwer value) because we don't know value (it's not in global-table)

