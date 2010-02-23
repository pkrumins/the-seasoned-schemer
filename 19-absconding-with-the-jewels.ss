;
; Chapter 19 of The Seasoned Schemer:
; Absconding with the Jewels
;
; Code examples assemled by Peteris Krumins (peter@catonmat.net).
; His blog is at http://www.catonmat.net  --  good coders code, great reuse.
;
; Get yourself this wonderful book at Amazon: http://bit.ly/8cyjgw
;

; The atom? primitive
;
(define atom?
  (lambda (x)
   (and (not (pair? x)) (not (null? x)))))  

; The sub1 primitive
;
(define sub1
  (lambda (n)
    (- n 1)))

; Our friend deep
;
(define deep
  (lambda (m)
    (cond
      ((zero? m) 'pizza)
      (else (cons (deep (sub1 m)) '())))))

; Example of deep
;
(deep 6)                    ; '((((((pizza))))))

; Six layers creates six layered pizza
;
(define six-layers
  (lambda (p)
    (cons (cons (cons
    (cons (cons (cons p '()) '()) '()) '()) '()) '())))

; Example of six-layers
;
(six-layers 'pizza)         ; '((((((pizza))))))

; Four layers makes a four layered pizza
;
(define four-layers
  (lambda (p)
    (cons (cons (cons
    (cons p '()) '()) '()) '())))

; Example of four-layers
;
(four-layers 'pizza)        ; '((((pizza))))

; The deepB function does layering
;
(define toppings 0)
(define deepB
  (lambda (m)
    (cond
      ((zero? m)
       (call-with-current-continuation
         (lambda (jump)
           (set! toppings jump)
           'pizza)))
      (else
        (cons (deepB (sub1 m)) '())))))

; Example of deepB
;
(deepB 6)                   ; '((((((pizza)))))), but what does jump do?

; Six layers again
;
(six-layers 'mozzarella)    ; '((((((mozarella))))))

; Now the toppings
;
(toppings 'mozzarella)      ; '((((((mozarella))))))
(toppings 'cake)            ; '((((((cake))))))
(toppings 'pizza)           ; '((((((pizza))))))

; What about his
;
(cons (toppings 'cake) '()) ; it's still '((((((cake)))))) and not '(((((((cake)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                            ;
; The twentieth commandment                                                  ;
;                                                                            ;
; When thinking about a value created with (letcc ...), write down the       ;
; function that is equivalent but does not forget. Then, when you use it,    ;
; remember to forget.                                                        ;
;                                                                            ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Deep&co
;
(define deep&co
  (lambda (m k)
    (cond
      ((zero? m) (k 'pizza))
      (else
        (deep&co (sub1 m)
         (lambda (x) (k (cons x '()))))))))

; Examples of deep&co
;
(deep&co 0 (lambda (x) x))          ; 'pizza
(deep&co 6 (lambda (x) x))          ; '((((((pizza))))))
(deep&co 2 (lambda (x) x))          ; '((pizza))

; Deep&coB
;
(define deep&coB
  (lambda (m k)
    (cond
      ((zero? m)
       (let ()
         (set! toppings k)
         (k 'pizza)))
    (else
      (deep&coB (sub1 m)
        (lambda (x)
          (k (cons x '()))))))))

; Examples of deep&coB
;
(deep&coB 6 (lambda (x) x))         ; '((((((pizza))))))
(deep&coB 4 (lambda (x) x))         ; '((((pizza))))

; toppings is now four-layers

(toppings 'cake)                    ; '((((cake))))
(cons
  (toppings 'cake)
  (toppings 'cake))                 ; '(((((cake)))) (((cake))))

; Remember two-in-a-row?
;
(define two-in-a-row?
  (letrec
    ((W (lambda (a lat)
          (cond
            ((null? lat) #f)
            (else
              (let ((nxt (car lat)))
                (or
                  (eq? nxt a)
                  (W nxt (cdr lat)))))))))
    (lambda (lat)
      (cond
        ((null? lat) #f)
        (else (W (car lat) (cdr lat)))))))

; Examples of two-in-a-row?
;
(two-in-a-row? '(mozzarella cake mozzarella))      ; #f
(two-in-a-row? '(mozzarella mozzarella cake))      ; #t

; walk
;
(define leave '0)
(define walk
  (lambda (l)
    (cond
      ((null? l) '())
      ((atom? (car l))
       (leave (car l)))
      (else
        (begin
          (walk (car l))
          (walk (cdr l)))))))

; we need leave
(define start-it
  (lambda (l)
    (call-with-current-continuation
      (lambda (here)
        (set! leave here)
        (walk l)))))

; Example of start-it
;
(start-it '((potato) (chips (chips (with))) fish))  ; 'potato

; waddle, just like walk but remembers where it left
;
(define fill 0)
(define waddle
  (lambda (l)
    (cond
      ((null? l) '())
      ((atom? (car l))
       (begin
         (call-with-current-continuation
           (lambda (rest)
             (set! fill rest)
             (leave (car l))))
         (waddle (cdr l))))
      (else
        (begin
          (waddle (car l))
          (waddle (cdr l)))))))

; A new start
;
(define start-it2
  (lambda (l)
    (call-with-current-continuation
      (lambda (here)
        (set! leave here)
        (waddle l)))))

; Example of start-it2
;
(start-it2 '((donuts) (cheerios (cheerios (spaghettios))) donuts))
; ===> 'donuts

; get-next gets the next atom
;
(define get-next
  (lambda (x)
    (call-with-current-continuation
      (lambda (here-again)
        (set! leave here-again)
        (fill 'go)))))

; Examples of get-next
;
(get-next 'go)      ; 'cheerios
(get-next 'go)      ; 'cheerios
(get-next 'go)      ; 'paghettios
(get-next 'go)      ; 'donuts
(get-next 'go)      ; '()

; get-first
;
(define get-first
  (lambda (l)
    (call-with-current-continuation
      (lambda (here)
        (set! leave here)
        (waddle l)
        (leave '())))))

; Examples of get-first and get-next
;
(get-first '(fish (chips)))     ; 'fish
(get-next 'go)                  ; 'chips
(get-next 'go)                  ; '()

; And now the two-in-a-row*?
;
(define two-in-a-row*?
  (letrec
    ((T? (lambda (a)
           (let ((n (get-next 0)))
             (if (atom? n)
               (or (eq? n a) (T? n))
               #f))))
     (get-next
       (lambda (x)
         (call-with-current-continuation
           (lambda (here-again)
             (set! leave here-again)
             (fill 'go)))))
     (fill (lambda (x) x))
     (waddle
       (lambda (l)
         (cond
           ((null? l) '())
           ((atom? (car l))
            (begin              ; or (let() ...
              (call-with-current-continuation
                (lambda (rest)
                  (set! fill rest)
                  (leave (car l))))
              (waddle (cdr l))))
           (else
             (begin
               (waddle (car l))
               (waddle (cdr l)))))))
     (leave (lambda (x) x)))
    (lambda (l)
      (let ((fst (call-with-current-continuation
                   (lambda (here)
                     (set! leave here)
                     (waddle l)
                     (leave '())))))
        (if (atom? fst) (T? fst) #f)))))

; Examples of two-in-a-row*?
;
(two-in-a-row*? '(((food) ()) (((food)))))      ; #t
(two-in-a-row*? '(a (b (c (d))) () () (d)))     ; #t
(two-in-a-row*? '())                            ; #f
(two-in-a-row*? '(((((a))))))                   ; #f

;
; Go get yourself this wonderful book and have fun with the Scheme language!
;
; Shortened URL to the book at Amazon.com: http://bit.ly/8cyjgw
;
; Sincerely,
; Peteris Krumins
; http://www.catonmat.net
;

