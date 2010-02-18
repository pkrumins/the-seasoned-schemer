;
; Chapter 16 of The Seasoned Schemer:
; Ready, Set, Bang!
;
; Code examples assemled by Peteris Krumins (peter@catonmat.net).
; His blog is at http://www.catonmat.net  --  good coders code, great reuse.
;
; Get yourself this wonderful book at Amazon: http://bit.ly/8cyjgw
;

; sub1 primitive
;
(define sub1
  (lambda (n)
    (- n 1)))

; add1 primitive
;
(define add1
  (lambda (n)
    (+ n 1)))

; atom? primitive
;
(define atom?
  (lambda (x)
   (and (not (pair? x)) (not (null? x)))))  

; member? helper function
;
(define member?
  (lambda (a l)
    (letrec
      ((yes? (lambda (l)
               (cond
                 ((null? l) #f)
                 ((eq? (car l) a) #t)
                 (else (yes? (cdr l)))))))
      (yes? l))))


; Code examples start here
;
(define sweet-tooth
  (lambda (food)
    (cons food (cons 'cake '()))))

(define last 'angelfood)

(sweet-tooth 'fruit)        ; '(fruit cake)
last                        ; 'angelfood

; The sweet-toothL function saves the last food
;
(define sweet-toothL
  (lambda (food)
    (set! last food)
    (cons food (cons 'cake '()))))

(sweet-toothL 'chocolate)   ; '(chocolate cake)
last                        ; 'chocolate

(sweet-toothL 'fruit)       ; '(fruit cake)
last                        ; 'fruit

(define ingredients '())

; The sweet-toothR function builds a list of foods
;
(define sweet-toothR
  (lambda (food)
    (set! ingredients
      (cons food ingredients))
    (cons food (cons 'cake '()))))

(sweet-toothR 'chocolate)   ; '(chocolate cake)
ingredients                 ; '(chocolate)

(sweet-toothR 'fruit)       ; '(fruit cake)
ingredients                 ; '(fruit chocolate)

(sweet-toothR 'cheese)      ; '(cheese cake)
ingredients                 ; '(cheese fruit chocolate)

(sweet-toothR 'carrot)      ; '(carrot cake)
ingredients                 ; '(carrot cheese fruit chocolate)

; The deep function wraps pizza in n parenthesis
;
(define deep
  (lambda (m)
    (cond
      ((zero? m) 'pizza)
      (else
        (cons (deep (sub1 m)) '())))))

; Example of deep
;
(deep 3)                    ; '(((pizza)))
(deep 0)                    ; 'pizza

; The deepR1 function remembers the numbers deep has seen so far
;
(define Ns1 '())
(define deepR1
  (lambda (n)
    (set! Ns1 (cons n Ns1))
    (deep n)))

; Examples of deepR1
;
(deepR1 3)                  ; '(((pizza)))
Ns1                         ; (3)
(deepR1 0)                  ; 'pizza
Ns1                         ; (0 3)

; The deepR function remembers the numbers and the results
;
(define Ns '())
(define Rs '())
(define deepR
  (lambda (n)
    (let ((result (deep n)))
      (set! Ns (cons n Ns))
      (set! Rs (cons result Rs))
      result)))

; Examples of deepR
;
(deepR 3)                   ; '(((pizza)))
Ns                          ; '(3)
Rs                          ; '((((pizza))))
(deepR 5)                   ; '(((((pizza)))))
Ns                          ; '(5 3)
Rs                          ; '((((((pizza))))) (((pizza))))
(deepR 3)                   ; '(((pizza)))
Ns                          ; '(3 5 3)
Rs                          ; '((((pizza))) (((((pizza))))) (((pizza))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                            ;
; The nineteenth commandment                                                 ;
;                                                                            ;
; Use (set! ...) to remember valuable things between two distinct uses of a  ;
; function.                                                                  ;
;                                                                            ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; The find function finds pizza in Rs
;
(define find
  (lambda (n Ns Rs)
    (letrec
      ((A (lambda (ns rs)
            (cond
              ((= (car ns) n) (car rs))
              (else
                (A (cdr ns) (cdr rs)))))))
      (A Ns Rs))))

; Examples of find
;
(find 3 Ns Rs)              ; '(((pizza)))
(find 5 Ns Rs)              ; '(((((pizza)))))
;(find 7 Ns Rs)             ; not applicable at this time

; The deepM function either uses find or computes pizza (temporary version)
;
(define deepM-tmp
  (lambda (n)
    (if (member? n Ns)
      (find n Ns Rs)
      (deepR n))))

Ns                          ; '(3 5 3)
Rs                          ; '((((pizza))) (((((pizza))))) (((pizza))))

(set! Ns (cdr Ns))
(set! Rs (cdr Rs))

Ns                          ; '(5 3)
Rs                          ; '((((((pizza))))) (((pizza))))

; The final deepM version
;
(define deepM
  (lambda (n)
    (if (member? n Ns)
      (find n Ns Rs)
      (let ((result (deep n)))
        (set! Rs (cons result Rs))
        (set! Ns (cons n Ns))
        result))))

; Examples of deepM
(deepM 3)                   ; '(((pizza)))
(deepM 6)                   ; '((((((pizza))))))

; Redefining deep
;
(define deep
  (lambda (m)
    (cond
      ((zero? m) 'pizza)
      (else (cons (deepM (sub1 m)) '())))))

(deepM 9)                   ; '(((((((((pizza)))))))))

Ns                          ; '(9 8 7 6 5 3)

; Redefining deepM to folow 16th commandment
;
(define deepM
  (let ((Rs '())
        (Ns '()))
    (lambda (n)
      (if (member? n Ns)
        (find n Ns Rs)
        (let ((result (deep n)))
          (set! Rs (cons result Rs))
          (set! Ns (cons n Ns))
          result)))))

; Tests of the new deepM
;
(deepM 10)                  ; '((((((((((pizza))))))))))
(deepM 16)                  ; '((((((((((((((((pizza))))))))))))))))

; Better answer for find on empty lists
;
(define find
  (lambda (n Ns Rs)
    (letrec
      ((A (lambda (ns rs)
            (cond
              ((null? ns) #f)
              ((= (car ns) n) (car rs))
              (else
                (A (cdr ns) (cdr rs)))))))
      (A Ns Rs))))

; And a better deepM
;
(define deepM
  (let ((Rs '())
        (Ns '()))
    (lambda (n)
      (let ((exists (find n Ns Rs)))
        (if (atom? exists)
          (let ((result (deep n)))
            (set! Rs (cons result Rs))
            (set! Ns (cons n Ns))
            result)
          exists)))))

; Example of the new deepM
;
(deepM 10)                  ; '((((((((((pizza))))))))))
(deepM 16)                  ; '((((((((((((((((pizza))))))))))))))))
(deepM 0)                   ; 'pizza


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                            ;
;              Take a deep breath or a deep pizza, now.                      ;
;                                                                            ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Our good, old friend length
;
(define lengthz
  (lambda (l)
    (cond
      ((null? l) 0)
      (else (add1 (lengthz (cdr l)))))))

; Test length
;
(lengthz '())        ; 0
(lengthz '(a b x))   ; 3

; length via set!
;
(define lengthz
  (lambda (l) 0))

(set! lengthz
  (lambda (l)
    (cond
      ((null? l) 0)
      (else (add1 (lengthz (cdr l)))))))

; Test length
;
(lengthz '())        ; 0
(lengthz '(a b x))   ; 3

; length via set! again
;
(define lengthz
  (let ((h (lambda (l) 0)))
    (set! h
      (lambda (l)
        (cond
          ((null? l) 0)
          (else (add1 (h (cdr l)))))))
    h))

; Test length
;
(lengthz '())        ; 0
(lengthz '(a b x))   ; 3

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                            ;
; The seventeenth commandment (final version)                                ;
;                                                                            ;
; Use (set! x ...) for (let ((x ...)) ...) only if there is at least one     ;
; (lambda ... between it and the (let ...), or if the new value for x is a   ;
; function that refers to x                                                  ;
;                                                                            ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Another way to write length
;
(define h1              ; h1 is actually an anonymous name
  (lambda (l) 0))

(define lengthz
  (let ()
    (set! h1
      (lambda (l)
        (cond
          ((null? l) 0)
          (else (add1 (h1 (cdr l)))))))
    h1))

; Test length
;
(lengthz '())        ; 0
(lengthz '(a b x))   ; 3

; Another way
;
(define h2              ; h2 is actually an anonymous name
  (lambda (l)
    (cond
      ((null? l) 0)
      (else (add1 (h2 (cdr l)))))))

(define lengthz
  (let () h2))

; Test length
;
(lengthz '())        ; 0
(lengthz '(a b x))   ; 3

; length again
;
(define lengthz
  (let ((h (lambda (l) 0)))
    (set! h
      (lambda (l)
        (cond
          ((null? l) 0)
          (else (add1 (h (cdr l)))))))
    h))

; Test length
;
(lengthz '())        ; 0
(lengthz '(a b x))   ; 3

; Let's eliminate parts that are specific to length
;
(define L
  (lambda (lengthz)
    (lambda (l)
      (cond
        ((null? l) 0)
        (else (add1 (lengthz (cdr l))))))))

(define lengthz
  (let ((h (lambda (l) 0)))
    (set! h
      (L (lambda (arg) (h arg))))
    h))

; Test length
;
(lengthz '())        ; 0
(lengthz '(a b x))   ; 3

; Y-bang - the applicative-order imperative y-combinator
; (discovered by Peter Landin)
;
(define Y-bang
  (lambda (f)
    (letrec
      ((h (f (lambda (arg) (h arg)))))
      h)))

(define lengthz (Y-bang L))

; Test length
;
(lengthz '())        ; 0
(lengthz '(a b x))   ; 3

; depth* via Y-bang
;
(define D
  (lambda (depth*)
    (lambda (s)
      (cond
        ((null? s) 1)
        ((atom? (car s)) (depth* (cdr s)))
        (else
          (max (add1 (depth* (car s))) (depth* (cdr s))))))))

(define depth* (Y-bang D))

; Test depth*
;
(depth* '())                ; 1
(depth* '(((pizza)) ()))    ; 3

; The bizarre function
;
(define biz
  (let ((x 0))
    (lambda (f)
      (set! x (add1 x))
      (lambda (a)
        (if (= a x)
          0
          (f a))))))

; Another way to write bizarre
;
(define x1 0)               ; anonymous var
(define biz
  (lambda (f)
    (set! x1 (add1 x1))
    (lambda (a)
      (if (= a x1)
        0
        (f a)))))

; The Y-Combinator
;
(define Y
  (lambda (le)
    ((lambda (f) (f f))
     (lambda (f)
       (le (lambda (x) ((f f) x)))))))

((Y biz) 5)

; ((Y-bang biz) 5)          ; doesn't compute... why?

;
; Go get yourself this wonderful book and have fun with the Scheme language!
;
; Shortened URL to the book at Amazon.com: http://bit.ly/8cyjgw
;
; Sincerely,
; Peteris Krumins
; http://www.catonmat.net
;

