(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))
;in this book, empty list is not an atom
;atom and list are s-expressions
(atom? (quote ()))

(define two-in-a-row-b?
  (lambda (a lat)
    (cond
      ((null? lat) #f)
      (else (or (eq? a (car lat)) 
                (two-in-a-row-b? (car lat) (cdr lat)))))))
(define two-in-a-row?
  (lambda (lat)
    (cond
      ((null? lat) #f)
      (else (two-in-a-row-b? (car lat) (cdr lat))))))
 (two-in-a-row? '(2 3 4 6 6 8 9))

(define sum-of-prefixes-b
  (lambda (pre-sum tup)
    (cond
      ((null? tup) (quote ()))
      (else (cons (+ pre-sum (car tup)) 
                  (sum-of-prefixes-b (+ pre-sum (car tup)) (cdr tup)))))))
(define sum-of-prefixes
  (lambda (tup)
    (sum-of-prefixes-b 0 tup)))
(sum-of-prefixes '(2 1 9 17 0))

(define pick
  (lambda (n lat)
    (cond
      ((eq? 1 n) (car lat))
      (else (pick (sub1 n) (cdr lat))))))
(define scramble-b
  (lambda (tup rev-pre)
    (cond
      ((null? tup) '())
      (else (cons (pick (car tup)
                        (cons (car tup) rev-pre))
                  (scramble-b (cdr tup) (cons (car tup) rev-pre)))))))
(define scramble
  (lambda (tup)
    (scramble-b tup '())))
;count backwards from current position n elements (1 1 1 1 1 4 1 1 1 9)
;2 is the 6th element, back 1 is 2, back 2 is 4
(scramble '(1 1 1 3 4 2 1 1 9 2))

;new version of multirember, in recursion, we don't repeat the param a
; (letrec ((mr ...)) mr) defines and returns a recursive function
(define multirember
  (lambda (a lat)
    ((letrec
       ((mr (lambda (lat)
              (cond
                ((null? lat) (quote ()))
                ((eq? a (car lat))
                 (mr (cdr lat)))
                (else 
                  (cons (car lat) (mr (cdr lat))))))))
       mr) lat)))
;above can be
; (define multirember (lambda (a lat) (lecrec ((mr ...)) (mr lat)))) 
; the first part of letrec is naming, the second part is value
; use (letrec ...) to remove arguments that do not change for recursive applications

(define multirember-f
  (lambda (test?)
    (letrec 
      ((m-f (lambda (a lat)
              (cond
                ((null? lat) (quote ()))
                ((test? (car lat) a)
                 (m-f a (cdr lat)))
                (else (cons (car lat) (m-f a (cdr lat))))))))
      m-f)))

(define t equal?)
((multirember-f t) "a" '("a" "b" "c" "d" "a" "e"))

; (letrec ...) can define more than one functions 
;we could also make letrec a lambda
(define scramble
  (lambda (tup)
    (letrec
      ((P (lambda (n lat)
            (cond
              ((eq? 1 n) (car lat))
              (else (P (sub1 n) (cdr lat))))))
       (S (lambda (tup rev-pre)
            (cond 
              ((null? tup) '())
              (else (cons (P (car tup) (cons (car tup) rev-pre))
                          (S (cdr tup) (cons (car tup) rev-pre))))))))
      (S tup '()))))
(scramble '(1 1 1 3 4 2 1 1 9 2))

;if we want to break in some cases
;means "forget what we had remembered to do after leaving
;behind (letcc hop and before encountering (hop M) And then
;act as if we were to determine the value of (letcc hop M)
;whatever M is"
(define intersectall
  (lambda (lset)
    (letcc hop ;call/cc
       (letrec
         ((A (lambda (lset)
               (cond
                 ((null? (car lset)) ;if any of the sets is (), then the result will be ()
                  (hop (quote ())))
                 ((null? (cdr lset))
                  (car lset))
                 (else I (car lset) (A (cdr lset))))))
          (I (lambda (s1 s2)
               (letrec
                 ((J (lambda (s1)
                       (cond
                         ((null? s1) s2)
                         ((member? (car s1) s2)
                          (J (cdr s1)))
                         (else (cons (car s1) (J (cdr s1))))))))
                 (cond
                   ((null? s2) (hop (quote ())))
                   (else (J s1)))))))
         (cond
           ((null? lset) (quote ()))
           (else (A lset)))))))
; = 
(define intersectall
  (lambda (lset)
    (call/cc
      (lambda (hop)
        (letrec
          ((A (lambda (lset)
                (cond
                  ((null? (car lset))
                   (hop (quote ())))
                  ((null? (cdr lset))
                   (car lset))
                  (else intersect (car lset) (A (cdr lset)))))))
          (cond
            ((null? lset) (quote ()))
            (else (A lset))))))))
;works for ((3 mangoes and) () (3 diet hamburgers)) 
;but not for ((3 steaks and) (no food and) (three baked potatoes))
;where the value of sub recursion is (), we need to change intersect 
;so if set2 is empty, it returns directly.

(define rember
  (lambda (a lat)
    (letrec 
      ((R (lambda (lat)
            (cond
              ((null? lat) (quote ()))
              ((eq? a (car lat)) (cdr lat))
              (else (cons (car lat) (R (cdt lat))))))))
      (R lat))))

(define rember-upto-last
  (lambda (a lat)
    (call/cc
      (lambda (skip)
        (letrec
          ((R (lambda (lat)
                (cond
                  ((null? lat) (quote ()))
                  ((eq? a (car lat)) (skip (R (cdr lat))))
                  (else (cons (car lat) (R (cdr lat))))))))
          (R lat))))))
(rember-upto-last 3 '(2 4 3 5 6 7 3 9 8))

;let is like letrec
(define leftmost
  (lambda (l)
    (cond
      ((null? l) (quote ()))
      ((atom? (car l)) (car l))
      (else (let ((a (leftmost (car l)))) 
              (cond
                ((atom? a) a) 
                (else (leftmost (cdr l)))))))))
;better version with cwcc
;let here has 2 values, if the first one has value (not skip),
;then it's ignored, and we go and check the second one 
(define leftmost
  (lambda (l)
    (call/cc
      (lambda (skip)
        (letrec 
          ((L (lambda (l)
                (cond
                  ((null? l) (quote ()))
                  ((atom? (car l)) (skip (car l)))
                  (else (let ()
                          (L (car l)) 
                          (L (cdr l))))))))
        (L l))))))
(leftmost '(((1)) 2 (3)))

(define depth*
  (lambda (l)
    (cond
      ((null? l) 1)
      ((atom? (car l))
       (depth* (cdr l)))
      (else
        (let
          ((a (add1 (depth* (car l))))
           (d (depth* (cdr l))))
          (max a d)))))) ;actually no need to use let anymore 
(depth* '(1 (2 (3 4) 5) 6))

; (try x a b), x is flag, a if success, else b

;set! looks like define, name + expression
(define gourmet
  (lambda (food)
    (set! x (quote skins))
    (cons food (cons x (quote ())))))
(gourmet 'onion)
(display x)
;use (set! ...) only with names defined in (let ...)
(define omnivore
  (let ((x (quote minestrone)))
    (lambda (food)
      (set! x food)
      (cons food (cons x (quote ()))))))
(omnivore 'seed)
;is different from
(define nibbleer
  (lambda (food)
    (let ((x (quote donut))) ;here let doesn't help
      (set! x food)
      (cons food (cons x (quote ()))))))
(nibbleer 'cheerio)
; Use (set! x ...) for (let ((x ...))...) only if there is at least
; one (lambda ...) between it and the (let ...), or if the new value
; for x is a function that refers to x

(define last (quote ()))
(define sweet-toothR
  (lambda (food)
    (set! last (cons food last))
    (cons food (cons (quote cake) (quote ())))))
(display last) (display "\n")
(sweet-toothR 'fruit)
(sweet-toothR 'chocolate)
(display last)

(define find
  (lambda (n Ns Rs)
    (letrec 
      ((A (lambda (ns rs)
            (cond
              ((null? ns) #f)
              ((= (car ns) n) (car rs))
              (else (A (cdr ns) (cdr rs)))))))
      (A Ns Rs))))
;deep call deepM back, so for n, we can cache all x < n results
(define deep
  (lambda (n)
    (cond
      ((zero? n) (quote pizza))
      (else (cons (deepM (sub1 n)) (quote ()))))))
(define deepM
  ;not accessible to outside of this function
  (let ((Rs (quote ()))
        (Ns (quote ())))
    (lambda (n)
      (let ((r (find n Ns Rs)))
      (if (atom? r)
        (let ((result (deep n)))
          (set! Rs (cons result Rs))
          (set! Ns (cons n Ns))
          result)
        r)))))
(deepM 4)

(define length
  (lambda (l)
    (cond
      ((null? l) 0)
      (else (add1 (length (cdr l)))))))
(length '(1 2 3))
;=> original define to a lambda wrapped by a new define
(define L
  (lambda (length)
    (lambda (l)
      (cond
        ((null? l) 0)
        (else (add1 (length (cdr l))))))))
;(L (lambda (arg) (h arg))) is 
;(lambda (l)
;  (cond
;    ((null? l) 0)
;    (else (add1 (h (cdr l))))))
(define h
  (L (lambda (arg) (h arg))))
(h '(1 2 3))
; a (letrec ...) is an abbreviation for an expression consisting
; of (let ..;) and (set! ...)
(define Y!
  (lambda (f)
    (letrec
      ((h (f (lambda (arg) (h arg)))))
      h)))
((Y! L) '(1 2 3)) ; (define length (Y! L)
;Y! is the applicative-order omperative Y combinator
;we go from a recursive function definition to a function f 
;such that (Y! f) builds the corresponding recursive function
;without (define ...)
;f is like the recursive function except that the name of the 
;recursive function is replaced by the name recfun and the whole
;expression is wrapped in (lambda (recfun) ...)

;with let, you can't reference other bindings which appear in the 
;same let expression
;applying a (lambda ...) immediately to an argument is equivalent 
;to (let ...)

;count how many times we see the argument, but we don't have access to N
;so we use a counter as getter (and we also need a setter) 
(define consC
  (let ((N 0))
    (set! counter
      (lambda () N))
    (set! set-counter
      (lambda (n)
        (set! N n)))
    (lambda (a b)
      (set! N (add1 N))
      (cons a b))))
(define deep
  (lambda (m)
    (if (zero? m)
      (quote pizza)
      (consC (deep (sub1 m))
             (quote ())))))
(deep 7)
(define supercounter
  (lambda (f)
    (letrec
      ((S (lambda (n)
            (if (zero? n)
              (f n)
              (let ()
                (f n)
                (S (sub1 n)))))))
      (set-counter 0)
      (S 1000)
      (counter))))
(supercounter deep)

(define lots
  (lambda (n)
    (cond
      ((zero? n) (quote ()))
      (else (cons "egg" (lots (sub1 n)))))))
(define x '(1))
(set-cdr! x '(2))
(display x)

;if we define cons like this
;kar and kdr are both selectors
(define kons
  (lambda (kar kdr)
    (lambda (selector)
      (selector kar kdr))))
(define kar
  (lambda (c)
    (c (lambda (a d) a))))
(define kdr
  (lambda (c)
    (c (lambda (a d) d))))
(kar (kons 1 '(2))) ; => 1
(kdr (kons 1 '(2))) ; => (2)
;another example
(define bons
  (lambda (kar)
    (let ((kdr (quote ())))
      (lambda (selector)
        (selector
          (lambda (x) (set! kdr x))
          kar
          kdr)))))
(define kar
  (lambda (c)
    (c (lambda (s a d) a))))
(define kdr
  (lambda (c)
    (c (lambda (s a d) d))))
(kar (bons 1)) ; => 1
(kdr (bons 1)) ; => ()
(define set-kdr
  (lambda (c x)
    ((c (lambda (s a d) s)) x)))
;now use bons and set-kdr to define kons
(define kons
  (lambda (kar kdr)
    (let ((c (bons kar)))
      (set-kdr c kdr)
      c)))
(kdr (kons 1 '(2))) ; => (2)

;https://stackoverflow.com/a/16531153/2741395
;by invoking the continuation we skip anything after the invocation and continue
;right at the call/cc point => check function intersectall
;here we save the continuation in toppings, now we can use it as we "jump back" to
;whatever computation followed the call/cc point
;call/cc returns either 1. the return value of lambda, or 2. the argument provided 
; when the continucation is invoked

;https://www.cs.rpi.edu/academics/courses/fall00/ai/scheme/reference/schintro-v14/schintro_141.html
;call/cc is a procedure of one argument, wich is another procedure (abortable procedure) to execute
;after the current continuation has been captured. The current continuation will
;be passed to that procedure, which can use it (or not) as it pleases.
;The captured continuation(jump below, called escape procedure) is itself packaged up as a procedure, also of one argument.

;why abortable procedure? I think it's because if we use the captured continuation in that procedure, it terminates and forgets the rest

;call/cc does the following:
;1. creates an escape procedure that captures the current continuation. If called, this procedure 
;will restore the continuation at the point of call to call/cc
;2. calls the procedure passed s its argument, handling it the escape procedure as its argument
(define deepB
  (lambda (m)
    (cond
      ((zero? m)
       (call/cc ; = call-with-current-continuation
         (lambda (jump)
              (set! toppings jump)
              (quote pizza))))
      (else (cons (deepB (sub1 m))
                  (quote ()))))))
(deepB 2) ; ((pizza))
(toppings (quote oh)) ; ((oh))
;when thinking about a value created with (letcc ...)
;write down the function that is equivalent but does not 
;forget. Then, when you use it, remember to forget.
(define deep&co
  (lambda (m k)
    (cond
      ((zero? m) (k (quote pizza)))
      (else
        (deep&co (sub1 m)
                 ;we call current function in new function
                 ;so the first function will be called the first
                 (lambda (x)
                   (k (cons x (quote ())))))))))
(deep&co 3 (lambda (x) x))
(define deep&coB
  (lambda (m k)
    (cond
      ((zero? m)
       (let ()
         (set! toppings k)
         (k (quote pizza))))
      (else
        (deep&coB (sub1 m)
                  (lambda (x)
                    (k (cons x (quote ())))))))))
(deep&coB 2 (lambda (x) x))
(toppings "tea");it's like toppings defined by letcc, but different

(define leave '())
(define walk
  (lambda (l)
    (cond
      ((null? l) (quote ()))
      ((atom? (car l))
       (leave (car l)))
      (else
        ;when you need to use several form where only 1 is allowed, 
        ;this will evaluate f1, f2...fn, and return fn
        (begin ;equal to (let () ...)
          (walk (car l))
          (walk (cdr l)))))))
(define start-it
  (lambda (l)
    (call/cc
      (lambda (here)
        (set! leave here) ;leave does whatever is left to do after the value of (start-it l) is determined
        (walk l)))))
(start-it '(("potato") ("chips" ("chips" ("with"))) "fish")) ;potato
;call/cc 
(define fill '())
(define waddle
  (lambda (l)
    (cond
      ((null? l) (quote ())) 
      ((atom? (car l))
       (begin
         (call/cc
           (lambda (rest)
             (set! fill rest) 
             (leave (car l))))
         ;(display (format "after leave: ~s " (cdr l)))
         (waddle (cdr l)) ; continuation
         )) 
      (else
        (begin
          (waddle (car l))
          ;(display (format "in else: ~s " (cdr l)))
          (waddle (cdr l))
          )))))
(define get-first
  (lambda (l)
    (call/cc
      (lambda (here)
        (set! leave here)
        ;(display "get-first: ")
        (waddle l)
        ;(display "after-waddle: ")
        (leave (quote ()))
        ))))
;(get-first '(1)) ;get-first: 1 | leave directly
;(fill '("go")) ;after leave: () after-waddle: () | called with continuation, return normally to get-first
;(get-first '(() (1))) ;get-first: in else: ((1)) 1 | 
;(fill '("go")) ;after leave: () in else: () after-waddle: ()
(get-first '(("donuts") ("cheerios" ("cheerios" ("spaghettios"))) "donuts")) ;donuts
(define get-next
  (lambda (x)
    (call/cc
      (lambda (here-again)
        (set! leave here-again)
        (fill (quote go))))))
;Q: why we need to set a new leave in get-next
;A: it's also the reason why we add (leave (quote ()) in get-first
;because we want to make get-next/fill return to get-next, rather than get-first
(get-next (quote go)) ;cheerios, again, the argument is ignored
(get-next (quote go)) ;cheerios
(get-next (quote go)) ;spaghettios
(get-next (quote go)) ;donuts
(get-next (quote go)) ;()
(define two-in-a-row*?
  (lambda (l)
    (let ((fst (get-first l)))
      (if (atom? fst) ;in this version '() is not atom
        (two-in-a-row-b*? fst)
        #f))))
(define two-in-a-row-b*?
  (lambda (a)
    (let ((n (get-next (quote go))))
      (if (atom? n)
        (or (eq? n a)
            (two-in-a-row-b*? n))
        #f))))

