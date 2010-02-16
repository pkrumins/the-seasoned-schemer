;
; Chapter 13 of The Seasoned Schemer:
; Hop Skip and Jump
;
; Code examples assemled by Peteris Krumins (peter@catonmat.net).
; His blog is at http://www.catonmat.net  --  good coders code, great reuse.
;
; Get yourself this wonderful book at Amazon: http://bit.ly/8cyjgw
;

; The intersect function finds the intersect of two sets
;

(define intersect
  (lambda (set1 set2)
    (cond
      ((null? set1) '())  ; don't forget the 1st commandment
      ((member? (car set1) set2)
       (cons (car set1) (intersect (cdr set1) set2)))
      (else
        (intersect (cdr set1) set2)))))

; It needs member? helper function
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

; Examples of intersect
;
(intersect '(a b x c d) '(q w e x r t y a))     ; '(a x)
(intersect '(a b x c d) '())                    ; '()
(intersect '() '())                             ; '()
(intersect '() '(a b x c d))                    ; '()
(intersect '(a b x c d) '(a b x c d))           ; '(a b x c d)

; We forgot the 12th commandment - use letrec to remove arguments
; that do not change for recursive applications
;
(define intersect-letrec
  (lambda (set1 set2)
    (letrec
      ((I (lambda (set)
            (cond
              ((null? set) '())
              ((member? (car set) set2)
               (cons (car set) (I (cdr set))))
              (else
                (I (cdr set)))))))
      (I set1))))

; Test of intersect-letrec
;
(intersect-letrec '(a b x c d) '(q w e x r t y a))     ; '(a x)
(intersect-letrec '(a b x c d) '())                    ; '()
(intersect-letrec '() '())                             ; '()
(intersect-letrec '() '(a b x c d))                    ; '()
(intersect-letrec '(a b x c d) '(a b x c d))           ; '(a b x c d)

; The intersectall function finds intersect of a bunch of sets
;
(define intersectall
  (lambda (lset)
    (cond
      ((null? lset) '())
      ((null? (cdr lset)) (car lset))
      (else
        (intersect (car lset)
                   (intersectall (cdr lset)))))))

; Examples of intersectall
;
(intersectall '((a) (a) (a)))                   ; '(a)
(intersectall '((a) () (a)))                    ; '()
(intersectall '())                              ; '()
(intersectall '((a b c d) (b c d e) (c d e f))) ; '(c d)

; Obeying the 12th commandment
;
(define intersectall-letrec
  (lambda (lset)
    (letrec
      ((A (lambda (lset)
            (cond
              ((null? (cdr lset)) (car lset))
              (else
                (intersect (car lset)
                           (A (cdr lset))))))))
      (cond
        ((null? lset) '())
        (else (A lset))))))

; Tests of intersectall-letrec
;
(intersectall-letrec '((a) (a) (a)))                   ; '(a)
(intersectall-letrec '((a) () (a)))                    ; '()
(intersectall-letrec '())                              ; '()
(intersectall-letrec '((a b c d) (b c d e) (c d e f))) ; '(c d)

; Introducing letcc
;
(define intersectall-letcc
  (lambda (lset)
    (call-with-current-continuation
      (lambda (hop)
        (letrec
          ((A (lambda (lset)
                (cond
                  ((null? (car lset)) (hop '()))
                  ((null? (cdr lset)) (car lset))
                  (else
                    (intersect (car lset)
                               (A (cdr lset))))))))
          (cond
            ((null? lset) '())
            (else (A lset))))))))

; Tests of intersectall-letcc
;
(intersectall-letcc '((a) (a) (a)))                   ; '(a)
(intersectall-letcc '((a) () (a)))                    ; '()
(intersectall-letcc '())                              ; '()
(intersectall-letcc '((a b c d) (b c d e) (c d e f))) ; '(c d)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                            ;
; The fourteenth commandment                                                 ;
;                                                                            ;
; Use (letcc ...) to return values abruptly and prompty.                     ;
;                                                                            ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; intersectall that returns abruptly and promptly
;
(define intersectall-ap
  (lambda (lset)
    (call-with-current-continuation
      (lambda (hop)
        (letrec
          ((A (lambda (lset)
                (cond
                  ((null? (car lset)) (hop '()))
                  ((null? (cdr lset)) (car lset))
                  (else
                    (I (car lset)
                       (A (cdr lset)))))))
           (I (lambda (s1 s2)
                (letrec
                  ((J (lambda (s1)
                        (cond
                          ((null? s1) '())
                          ((member? (car s1) s2)
                           (cons (car s1) (J (cdr s1))))
                          (else
                            (J (cdr s1)))))))
                  (cond
                    ((null? s2) (hop '()))
                    (else (J s1)))))))
          (cond
            ((null? lset) '())
            (else (A lset))))))))

; Tests of intersectall-ap
;
(intersectall-ap '((a) (a) (a)))                   ; '(a)
(intersectall-ap '((a) () (a)))                    ; '()
(intersectall-ap '())                              ; '()
(intersectall-ap '((a b c d) (b c d e) (c d e f))) ; '(c d)

; rember function via letrec
;
(define rember
  (lambda (a lat)
    (letrec
      ((R (lambda (lat)
            (cond
              ((null? lat) '())
              ((eq? (car lat) a) (cdr lat))
              (else
                (cons (car lat) (R (cdr lat))))))))
      (R lat))))

; Examples of rember
;
(rember 'x '(a x b c))          ; '(a b c)
(rember 'x '())                 ; '()
          
; The rember-beyond-first function rembers everything beyond first match
;
(define rember-beyond-first
  (lambda (a lat)
    (letrec
      ((R (lambda (lat)
            (cond
              ((null? lat) '())
              ((eq? (car lat) a) '())
              (else
                (cons (car lat) (R (cdr lat))))))))
      (R lat))))

; Examples of rember-beyond-first
;
(rember-beyond-first
  'roots
  '(noodles spaghetti spatzle bean-thread roots potatoes yam others rice))
; ==> '(noodles spaghetti spaghetti bean-thread)

(rember-beyond-first
  'others
  '(noodles spaghetti spatzle bean-thread roots potatoes yam others rice))
; ==> '(noodles spaghetti spaghetti bean-thread roots potatoes yam)

(rember-beyond-first
  'sweetthing
  '(noodles spaghetti spatzle bean-thread roots potatoes yam others rice))
; ==> '(noodles spaghetti spaghetti bean-thread roots potatoes yam others rice)

; The rember-upto-last function rembers everything before first match
;
(define rember-upto-last
  (lambda (a lat)
    (call-with-current-continuation
      (lambda (skip)
        (letrec
          ((R (lambda (lat)
                (cond
                  ((null? lat) '())
                  ((eq? (car lat) a) (skip (R (cdr lat))))
                  (else
                    (cons (car lat) (R (cdr lat))))))))
          (R lat))))))

; Examples of rember-upto-last
;
(rember-upto-last
  'roots
  '(noodles spaghetti spatzle bean-thread roots potatoes yam others rice))
; ==> '(potatoes yam others rice)

(rember-upto-last
  'sweetthing
  '(noodles spaghetti spatzle bean-thread roots potatoes yam others rice))
; ==> '(noodles spaghetti spaghetti bean-thread roots potatoes yam others rice)

(rember-upto-last
  'cookies
  '(cookies noodles spaghetti spatzle bean-thread roots potatoes yam cookies others rice))
; ==> '(others rice)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                            ;
;                      Have you taken a tea break yet?                       ;
;                          We are taking ours now.                           ;
;                                                                            ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;
; Go get yourself this wonderful book and have fun with the Scheme language!
;
; Shortened URL to the book at Amazon.com: http://bit.ly/8cyjgw
;
; Sincerely,
; Peteris Krumins
; http://www.catonmat.net
;

