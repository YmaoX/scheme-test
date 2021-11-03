(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))
;all atoms and lists are S-expressions
; () is a list but not an atom
;cons adds an atom to the front of a list

(null? '()) ;return #t, we cannot ask null? of an atom
(eq? x x) ;argument must be non-numeric atoms, = is used to compare numbers

(define lat?
  (lambda (l)
    (cond
      ((null? l) #t)
      ((atom? (car l)) (lat? (cdr l)))
      (else #f))))
(lat? '("toto" "titi"))
(lat? '("toto" '("titi")))

(define member?
  (lambda (a lat)
    (cond
      ((null? lat) #f)
      ((equal? a (car lat)) #t)
      (else (member? a (cdr lat))))))
(member? "toto" '("titi" "aa" "toto"))
(equal? "toto" "toto") ;#t
(eq? "toto" "toto") ;#f
(eq? 3 3) ;#t

(define set?
  (lambda (lat)
    (cond 
      ((null? lat) #t)
      ((member? (car lat) (cdr lat)) #f)
      (else (set? (cdr lat))))))
(set? '("apples" "peaches" 5 "pears" 5 "plums"))
(set? '("apples" "peaches"  "pears" 5 "plums"))

(define rember
  (lambda (a lat)
    (cond
      ((null? lat) '())
      ((equal? a (car lat)) (cdr lat))
      (else (cons (car lat) (rember a (cdr lat)))))))
(rember "toto" '("titi" "aa" "toto" "hello" "world" "toto"))

(define firsts
  (lambda (l)
    (cond
      ((null? l) '())
      (else (cons (car (car l)) (firsts (cdr l)))))))
(firsts '(("hello" "toto") ("world" "aa") ("!" "hello" "world" "toto")))

(define insertR
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((equal? old (car lat)) (cons old (cons new (cdr lat))))
      (else (cons (car lat) (insertR new old (cdr lat)))))))
;(trace insertR)
(insertR "e" "d" '("a" "b" "c" "d" "f" "g" "d"))

(define insertL
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((equal? old (car lat)) (cons new lat))
      (else (cons (car lat) (insertL new old (cdr lat)))))))
(insertL "e" "f" '("a" "b" "c" "d" "f" "g" "d"))

(define subst
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((equal? old (car lat)) (cons new (cdr lat)))
      (else (cons (car lat) (subst new old (cdr lat)))))))
(subst "e" "i" '("a" "b" "c" "d" "i" "f" "g" "d"))

(define multirember
  (lambda (a lat)
    (cond
      ((null? lat) '())
      ((equal? a (car lat)) (multirember a (cdr lat)))
      (else (cons (car lat) (multirember a (cdr lat)))))))
(multirember "d" '("a" "b" "c" "d" "i" "f" "g" "d"))

(define multiinsertR
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((equal? old (car lat)) (cons old (cons new (multiinsertR new old (cdr lat)))))
      (else (cons (car lat) (multiinsertR new old (cdr lat)))))))
(multiinsertR "e" "d" '("a" "b" "c" "d" "i" "f" "g" "d"))

(define multisubst
  (lambda (new old lat)
    (cond
      ((null? lat) '())
      ((equal? old (car lat)) (cons new (multisubst new old (cdr lat))))
      (else (cons (car lat) (multisubst new old (cdr lat)))))))
(multisubst "e" "d" '("a" "b" "c" "d" "i" "f" "g" "d"))

;we have add1, sub1, zero?, tup
(define +
  (lambda (a b)
    (cond
      ((zero? b) a)
      (else (+ (add1 a) (sub1 b))))))
(+ 10 24)

(define -
  (lambda (a b)
    (cond 
      ((zero? b) a)
      (else (- (sub1 a) (sub1 b))))))
(- 12 3)

(define addtup
  (lambda (tup)
    (cond
      ((null? tup) 0)
      (else (+ (car tup) (addtup (cdr tup)))))))
(addtup '(2 3 4 5))

(define x
  (lambda (a b)
    (cond
      ((zero? b) 0)
      (else (+ a (x a (sub1 b)))))))
(x 3 4)

(define tup+
  (lambda (tup1 tup2)
    (cond
      ((null? tup1) tup2)
      ((null? tup2) tup1)
      (else (cons (+ (car tup1) (car tup2)) (tup+ (cdr tup1) (cdr tup2)))))))
(tup+ '(2 3 5) '(3 4 6 9))

(define ↑
  (lambda (n m)
    (cond
      ((zero? m) 1)
      (else (* n (↑ n (sub1 m)))))))
(↑ 2 3)

(define length
  (lambda (lat)
    (cond 
      ((null? lat) 0)
      (else (add1 (length (cdr lat)))))))
(length '(2 3 4 5))

(define rempick
  (lambda (n lat)
    (cond
      ((zero? n) '())
      (else (cons (car lat) (rempick (sub1 n) (cdr lat)))))))
(rempick 3 '(3 4 5 6 7))

(define no-nums
  (lambda (lat)
    (cond
      ((null? lat) '())
      ((number? (car lat)) (no-nums (cdr lat)))
      (else (cons (car lat) (no-nums (cdr lat)))))))
(no-nums '(2 "toto" 5 7 "hello" 9 " " "world"))

(define eqan?
  (lambda (a1 a2)
    (cond
      ((and (number? a1) (number? a2)) (= a1 a2))
      ((or (number? a1) (number? a2) #f))
      (else (equal? a1 a2)))))

(define rember*
  (lambda (a l)
    (cond
      ((null? l) '())
      ((not (atom? (car l))) (cons (rember* a (car l)) (rember* a (cdr l))))
      ((equal? a (car l)) (rember* a (cdr l)))
      (else (cons (car l) (rember* a (cdr l)))))))
;(trace rember*)
;(rember* "cup" '(("coffee") "cup" (("tea") "cup") ("and" ("hick")) "cup"))
(rember* "sauce" '((("tomato" "sauce")) (("bean") "sauce") ("and" (("flying")) "sauce"))) 

(define insertR*
  (lambda (new old l)
    (cond
      ((null? l) '())
      ((not (atom? (car l))) (cons (insertR* new old (car l)) (insertR* new old (cdr l))))
      ((equal? old (car l)) (cons (car l) (cons new (insertR* new old (cdr l)))))
      (else (cons (car l) (insertR* new old (cdr l)))))))
(insertR* "roast" "chuck" '((("how" "much" ("wood"))) "could" (("a" ("wood") "chuck")) ((("chuck"))) ("if" ("a" (("wood" "chuck")))) "could" "chuck" "wood"))

(define insertL*
  (lambda (new old l)
    (cond
      ((null? l) '())
      ((not (atom? (car l))) (cons (insertL* new old (car l)) (insertL* new old (cdr l))))
      ((equal? old (car l)) (cons new (cons old (insertL* new old (cdr l)))))
      (else (cons (car l) (insertL* new old (cdr l)))))))
(insertL* "pecker" "chuck" '((("how" "much" ("wood"))) "could" (("a" ("wood") "chuck")) ((("chuck"))) ("if" ("a" (("wood" "chuck")))) "could" "chuck" "wood"))

(define occur* 
  (lambda (a l)
    (cond
      ((null? l) 0)
      ((not (atom? (car l))) (+ (occur* a (car l)) (occur* a (cdr l))))
      ((equal? a (car l)) (add1 (occur* a (cdr l))))
      (else (occur* a (cdr l))))))
(occur* "banana" '((("banana")) ("split" (((("banana" "ice"))) ("cream" ("banana")) "sherbet")) ("banana") ("bread") ("banana" "brandy")))

(define subst*
  (lambda (new old l)
    (cond
      ((null? l) '())
      ((not (atom? (car l))) (cons (subst* new old (car l)) (subst* new old (cdr l))))
      ((equal? old (car l)) (cons new (subst* new old (cdr l))))
      (else (cons (car l) (subst* new old (cdr l)))))))
(subst* "orange" "banana" '((("banana")) ("split" (((("banana" "ice"))) ("cream" ("banana")) "sherbet")) ("banana") ("bread") ("banana" "brandy")))

(define member*
  (lambda (a l)
    (cond
      ((null? l) #f)
      ((not (atom? (car l))) (or (member* a (car l)) (member* a (cdr l))))
      ((equal? a (car l)) #t)
      (else (member* a (cdr l))))))
(member* "chips" '(("potato") ("chips" (("with") "fish") ("chips"))))

(define leftmost
  (lambda (l)
    (cond
      ((not (atom? (car l))) (leftmost (car l)))
      (else (car l)))))
(leftmost '(("potato") ("chips" (("with") "fish") ("chips"))))

;primitives we need for numbers: number?, zero?, add1, sub1
;rel = relation = a list of pairs 
;fun = function = (firsts rel) is a set
(define fun?
  (lambda (rel)
    (set? (firsts rel))))

(define first
  (lambda (p) (car p)))
(define second
  (lambda (p) (car (cdr p))))
(define build
  (lambda (s1 s2)
    (cons s1 (cons s2 '()))))

(define revrel 
  (lambda (rel)
    (cond
      ((null? rel) '())
      (else (cons (build 
                    (second (car rel)) (first (car rel))) 
                  (revrel (cdr rel)))))))
(revrel '((8 "a") ("pumpkin" "pie") ("got" "sick")))

(define seconds
  (lambda (l)
    (cond
      ((null? l) '())
      (else (cons (second (car l)) (seconds (cdr l)))))))
(seconds '(("hello" "toto") ("world" "aa") ("!" "hello" "world" "toto")))

;fullfun is one-to-one
(define fullful?
  (lambda (fun)
    (set? (seconds fun))))

(define rember-f
  (lambda (test? a l)
    (cond
      ((null? l) '())
      ((test? a (car l)) (cdr l))
      (else (cons (car l) (rember-f test? a (cdr l)))))))
(rember-f equal? '("pop" "corn") '("lemonande" ("pop" "corn") "and" ("cake")))

;a colector is somtimtes called a "continuation"
;col is a func which takes 2 list as arguments
;below function collects non-a from lat in the first list, and a in the second list
;build functions to collect more than one value at a time
(define multirember&co
  (lambda (a lat col)
    (cond
      ((null? lat)
       (col '() '()))
      ((eq? (car lat) a)
       (multirember&co a (cdr lat)
                       (lambda (newlat seen)
                         (col newlat (cons (car lat) seen))))
       (else
         (multirember&co a (cdr lat)
                         (lambda (newlat seen)
                           (col (cons (car lat) newlat) seen))))))))

(define evens-only*
  (lambda (l)
    (cond
      ((null? l) '())
      ((atom? (car l))
       (cond
         ((even? (car l))
          (cons (car l) (evens-only* (cdr l))))
         (else 
          (evens-only* (cdr l)))))
      (else (cons (evens-only* (car l)) (evens-only* (cdr l)))))))
(evens-only* '((9 1 2 8) 3 10 ((9 9) 7 6) 2))

;collector function and original function have the same return value
;f2 (x) = f1 (t1(x)), f3 (x) = f2 (t2(x)) = f1 (t1(t2(x)))
;check named let in scheme, continuation-passing style
(define evens-only*&co
  (lambda (l col)
    (cond 
      ((null? l) (col '() 1 0))
      ((atom? (car l))
       (cond 
         ((even? (car l))
          (evens-only*&co (cdr l)
                          (lambda (newl p s)
                            (col (cons (car l) newl) (* (car l) p) s))))
         (else (evens-only*&co (cdr l)
                               (lambda (newl p s)
                                 (col newl p (+ s (car l))))))))
      (else (evens-only*&co (car l)
                            (lambda (al ap as) 
                              (evens-only*&co (cdr l)
                                      (lambda (dl dp ds)
                                                (col (cons al dl) (* ap dp) (+ as ds))))))))))
(define the-last-friend
  (lambda (newl product sum)
    (cons sum (cons product newl))))
(trace evens-only*&co)
(evens-only*&co '((1 2) 3) the-last-friend)
(evens-only*&co '((9 1 2 8) 3 10 ((9 9) 7 6) 2) the-last-friend)

(define pick
  (lambda (n lat)
    (cond
      ((zero? (sub1 n)) (car lat))
      (else (pick (sub1 n) (cdr lat))))))
(define keep-looking
  (lambda (a sorn lat)
    (cond
      ((number? sorn) (keep-looking a (pick sorn lat) lat))
      (else (equal? a sorn)))))
;this is a partial function
(define looking
  (lambda (a lat)
    (keep-looking a (pick 1 lat) lat)))
(looking "caviar" '(6 2 4 "caviar" 5 7 3))
(looking "caviar" '(6 2 "grits" "caviar" 5 7 3))

(define length
  (lambda (l)
    (cond 
      ((null? l) 0) 
      (else (+ 1 (length (cdr l)))))))
;length has a name, so we can call it in the body, what if a function doesn't have a name
(define eternity
  (lambda (l)
    (eternity l)))
;function application works only for empty list
;if we want it to work for size 1, then we have to call itself in else
((lambda (l)
   (cond
     ((null? l) 0)
     (else (+ 1 (eternity (cdr l)))))) '())
;eternity is the argument of length, the whole expression return a function which accepts a list as argument
((lambda (length) 
   (lambda (l)
     (cond
       ((null? l) 0)
       (else (add1 (length (cdr l)))))))
 eternity)
;define a function-f and then apply another function-g
((lambda (f) 
   (lambda (l)
     (cond
       ((null? l) 0)
       (else (add1 (f (cdr l)))))))
 ((lambda (g) ;this is the argument of f
    (lambda (l)
      (cond
        ((null? l) 0)
        (else (add1 (g (cdr l)))))))
  eternity))
;there are still repititions
;length <= 0
((lambda (mk-length) 
   (mk-length eternity))
 (lambda (length) 
   (lambda (l)
     (cond
       ((null? l) 0)
       (else (add1 (length (cdr l))))))))
;length <= 1
((lambda (mk-length) 
   (mk-length (mk-length eternity))) 
 (lambda (length)
   (lambda (l) 
     (cond
       ((null? l) 0)
       (else (add1 (length (cdr l))))))))
;-> replace eternity with mk-length for recursion
;this is still length-0
((lambda (mk-length) 
   (mk-length mk-length))
 (lambda (mk-length) 
   (lambda (l)
     (cond
       ((null? l) 0) 
       (else (add1 (mk-length (cdr l))))))))
;this is length
((lambda (mk-length) 
   (mk-length mk-length))
 (lambda (mk-length) 
   (lambda (l)
     (cond
       ((null? l) 0) 
       (else (add1 ((mk-length mk-length) (cdr l))))))))
;-> take (mk-length mk-length) out, replace it by a function application 
((lambda (mk-length) 
   (mk-length mk-length))
 (lambda (mk-length) 
   ((lambda (length)
      (lambda (l) 
        (cond
          ((null? l) 0)
          (else (add1 (length (cdr l))))))) 
    (mk-length mk-length))))

;if f is a function of 1 argument, 
;then (lambda (x) (f x)) is a function of 1 argument
(define f
  (lambda (a)
    (+ 1 a)))
(f 2)
((lambda (x)
  (f x)) 2)
;(mk-length mk-length) returns a function of 1 argument
;(lambda (x) ((mk-length mk-length) x)) is a function
((lambda (mk-length) 
   (mk-length mk-length))
 (lambda (mk-length) 
   (lambda (l)
     (cond
       ((null? l) 0) 
       (else (add1 
               ((lambda (x) ((mk-length mk-length) x))
                (cdr l))))))))
;->
((lambda (mk-length)
   (mk-length mk-length))
 (lambda (mk-length)
   ((lambda (length)
      (lambda (l)
        (cond
          ((null? l) 0)
          (else (add1 (length (cdr l)))))))
    (lambda (x)
      ((mk-length mk-length) x)))))
;-> take out the (lambda (length)) part
 ((lambda (le)
    ((lambda (mk-length)
       (mk-length mk-length))
     (lambda (mk-length)
       (le (lambda (x)
             ((mk-length mk-length) x))))))
  (lambda (length) ;-> this is the original length function
    (lambda (l)
      (cond
        ((null? l) 0)
        (else (add1 (length (cdr l))))))))
;the first part is called applicative-order Y combinator
(define Y
  (lambda (le)
    ((lambda (f) (f f))
     (lambda (f)
       (le (lambda (x) ((f f) x)))))))
;if we define with a name
(define addTo
  (lambda (x)
    (cond
      ((zero? x) 0)
      (else (add1 (addTo (- x 1)))))))
(addTo 5)
;without name, but using Y combinator
((Y (lambda (y)
  (lambda (x)
    (cond
      ((zero? x) 0)
      (else (add1 (y (- x 1)))))))) 4)

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
      ((eq? name (car names)) (car values))
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
      ((eq? e #t) *const)
      ((eq? e #f) *const)
      ((eq? e (quote cons)) *const) 
      ((eq? e (quote car)) *const) 
      ((eq? e (quote cdr)) *const) 
      ((eq? e (quote null?)) *const) 
      ((eq? e (quote eq?)) *const) 
      ((eq? e (quote atom?)) *const) 
      ((eq? e (quote zero?)) *const) 
      ((eq? e (quote add1)) *const) 
      ((eq? e (quote sub1)) *const) 
      ((eq? e (quote number?)) *const) 
      (else *identifier))))
(define list-to-action 
  (lambda (e) 
    (cond 
      ((atom? (car e)) 
       (cond ((eq? (car e) (quote quote)) *quote) 
             ((eq? (car e) (quote lambda)) *lambda) 
             ((eq? (car e) (quote cond)) *cond) 
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
      ((eq? e #t) #t)
      ((eq? e #f) #f)
      (else (build (quote primitive) e)))))
;lambda looks like '(lambda (param) body)
;here (con table (cdr e)) actually creates a closure (function + environment) -> lexical scoping
(define *lambda
  (lambda (e table)
    (build (quote non-primitive)
           (cons table (cdr e)))))
;helper functions
(define table-of first) (define formals-of second) (define body-of third)

(define question-of first) (define answer-of second)
(define else?
  (lambda (x)
    (cond
      ((atom? x) (eq? x (quote else)))
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
    (eq? (first l) (quote primitive))))
(define non-primitive?
  (lambda (l)
    (eq? (first l) (quote non-primitive))))
;fun starts with 'primitive or 'non-primitive
;check the definition of *const and *lambda
(define :atom?
  (lambda (x)
    (cond 
      ((atom? x) #t)
      ((null? x) #f)
      ((eq? (car x) (quote primitive))
       #t)
      ((eq? (car x) (quote non-primitive))
       #t)
      (else #f))))
(define apply-primitive
  (lambda (name vals)
    (cond
      ((eq? name (quote cons))
       (cons (first vals) (second vals)))
      ((eq? name (quote car))
       (car (first vals)))
      ((eq? name (quote cdr))
       (cdr (first vals)))
      ((eq? name (quote null?))
       (null? (first vals)))
      ((eq? name (quote eq?))
       (eq? (first vals) (second vals)))
      ((eq? name (quote atom?))
       (:atom? (first vals)))
      ((eq? name (quote zero?))
       (zero? (first vals)))
      ((eq? name (quote add1))
       (add1 (first vals)))
      ((eq? name (quote sub1))
       (sub1 (first vals)))
      ((eq? name (quote number?)))
      (number? (first vals)))))
(define apply-closure
  (lambda (closure vals)
    (meaning (body-of closure)
             (extend-table
               (new-entry (formals-of closure) vals)
               (table-of closure)))))
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
