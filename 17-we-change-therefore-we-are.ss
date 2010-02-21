;
; Chapter 17 of The Seasoned Schemer:
; We Change, Therefore We Are!
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

; The add1 primitive
;
(define add1
  (lambda (n)
    (+ n 1)))

; The deep function wraps pizza in m parenthesis
;
(define deep
  (lambda (m)
    (if (zero? m)
      'pizza
      (cons (deep (sub1 m)) '()))))

; Examples of deep
;
(deep 3)                ; '(((pizza)))
(deep 0)                ; 'pizza

; The deepM function remembers calls to deep
;
; The deepM function uses find function to find n in Ns and return 
; the correct value from Rs
;
(define find
  (lambda (n Ns Rs)
    (letrec
      ((A (lambda (ns rs)
            (cond
              ((null? ns) #f)
              ((= (car Ns) n) (car Rs))
              (else
                (A (cdr ns) (cdr rs)))))))
      (A ns rs))))

(define deepM
  (let ((Rs '())
        (Ns '()))
    (letrec
      ((D (lambda (m)
            (if (zero? m)
              'pizza
              (cons (deepM (sub1 m)) '())))))
      (lambda (n)
        (let ((exists (find n Ns Rs)))
          (if (atom? exists)
            (let ((result (D n)))
              (set! Rs (cons result Rs))
              (set! Ns (cons n Ns))
              result)
            exists))))))

; Examples of deepM
;
(deepM 3)               ; '(((pizza)))
(deepM 0)               ; 'pizza

; No need to use letrec in deepM
;
(define deepM-letrec
  (let ((Rs '())
        (Ns '())
        (D (lambda (m)
              (if (zero? m)
                'pizza
                (cons (deepM-letrec (sub1 m)) '())))))
    (lambda (n)
      (let ((exists (find n Ns Rs)))
        (if (atom? exists)
          (let ((result (D n)))
            (set! Rs (cons result Rs))
            (set! Ns (cons n Ns))
            result)
          exists)))))

; Test of the new deepM
;
(deepM-letrec 3)               ; '(((pizza)))
(deepM-letrec 0)               ; 'pizza

; No need for D in deepM
;
(define deepM-D
  (let ((Rs '())
        (Ns '()))
    (lambda (n)
      (let ((exists (find n Ns Rs)))
        (if (atom? exists)
          (let ((result ((lambda (m)
                           (if (zero? m)
                             'pizza
                             (cons (deepM-D (sub1 m)) '()))) n)))
            (set! Rs (cons result Rs))
            (set! Ns (cons n Ns))
            result)
          exists)))))

; Test of the new deepM
;
(deepM-D 3)               ; '(((pizza)))
(deepM-D 0)               ; 'pizza

; No need for the 2nd lambda 
;
(define deepM-2nd
  (let ((Rs '())
        (Ns '()))
    (lambda (n)
      (let ((exists (find n Ns Rs)))
        (if (atom? exists)
          (let ((result (if (zero? n)
                            'pizza
                            (cons (deepM-2nd (sub1 n)) '()))))
            (set! Rs (cons result Rs))
            (set! Ns (cons n Ns))
            result)
          exists)))))

; Test of the new deepM
;
(deepM-2nd 3)               ; '(((pizza)))
(deepM-2nd 0)               ; 'pizza

; The consC function counts number of conses needed to build
; n-deep pizza
;
(define consC
  (let ((N 0))
    (lambda (x y)
      (set! N (add1 N))
      (cons x y))))

; No way to get N out of this consC, so we need the counter
; function that will hold the count
;
(define counter 0)

; And a modification of consC
;
(define consC
  (let ((N 0))
    (set! counter (lambda() N))
    (lambda (x y)
      (set! N (add1 N))
      (cons x y))))
        
; And we need to modify deep
;
(define deep
  (lambda (m)
    (if (zero? m)
      'pizza
      (consC (deep (sub1 m)) '()))))

(deep 5)            ; '(((((pizza)))))
(counter)           ; 5

(deep 7)            ; '(((pizza))
(counter)           ; 12                ;;; (5 + 7) 

; Let's determine how many cons'es are necessary to find
; values of (deep 0) ... (deep 1000).
;
(define supercounter
  (lambda (f)
    (letrec
      ((S (lambda (n)
            (if (zero? n)
              (f n)
              (let ()
                (f n)
                (S (sub1 n)))))))
      (S 1000)
      (counter))))

; Try out supercounter
;
(supercounter deep)         ; 500512    ;;; not 500500 as expected

; Need to wipe out counter before using it again
;
(define set-counter 0)

(define consC
  (let ((N 0))
    (set! counter (lambda() N))
    (set! set-counter
      (lambda (x) (set! N x)))
    (lambda (x y)
      (set! N (add1 N))
      (cons x y))))

(set-counter 0)

; Try out supercounter again
;
(supercounter deep)

; How many conses are used by (deepM 5)?
; Need to modify deepM to use consC first.
;
(define deepM-consC
  (let ((Rs '())
        (Ns '()))
    (lambda (n)
      (let ((exists (find n Ns Rs)))
        (if (atom? exists)
          (let ((result
                  (if (zero? n)
                    'pizza
                    (consC (deepM-consC (sub1 n)) '()))))
            (set! Rs (cons result Rs))
            (set! Ns (cons n Ns))
            result)
          exists)))))

; How many conses are used by (deepM 5)?
;
(deepM 5)           ; '(((((pizza)))))
(counter)           ; 500505    ;;; because we forgot to reset counter

(set-counter 0)
(deepM-consC 5)     ; '(((((pizza)))))
(counter)           ; 5

(deep 7)            ;
(counter)           ; ??? the book says it should be 0, I get 12, why?

(supercounter deepM-consC)  ; ??? how did this work? it didn't take an argument?

; Book has also talks about conses in rember* function but I am not interested
; in it at the moment.

;
; Go get yourself this wonderful book and have fun with the Scheme language!
;
; Shortened URL to the book at Amazon.com: http://bit.ly/8cyjgw
;
; Sincerely,
; Peteris Krumins
; http://www.catonmat.net
;

