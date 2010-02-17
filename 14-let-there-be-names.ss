;
; Chapter 14 of The Seasoned Schemer:
; Let There Be Names
;
; Code examples assemled by Peteris Krumins (peter@catonmat.net).
; His blog is at http://www.catonmat.net  --  good coders code, great reuse.
;
; Get yourself this wonderful book at Amazon: http://bit.ly/8cyjgw
;

; atom? primitive
;
(define atom?
  (lambda (x)
   (and (not (pair? x)) (not (null? x)))))  

; add1 primitive
;
(define add1
  (lambda (n)
    (+ n 1)))

; The leftmost function finds the leftmost atom in a non-empty list of
; s-expressions that does not contain the empty list.
;
(define leftmost
  (lambda (l)
    (cond
      ((atom? (car l)) (car l))
      (else (leftmost (car l))))))

; Example of leftmost
;
(leftmost '(((a) b) (c d)))         ; 'a

; Example of not-applicable leftmost
;
; (leftmost '((() a) ()))           ; not applicable because leftmost item is an empty list

; Let's fix this.
;
(define leftmost-fixed
  (lambda (l)
    (cond
      ((null? l) '())
      ((atom? (car l)) (car l))
      (else
        (cond
          ((atom? (leftmost-fixed (car l)))
           (leftmost-fixed (car l)))
          (else (leftmost-fixed (cdr l))))))))

; Examples of leftmost-fixed
;
(leftmost-fixed '(((x) b) (c d)))   ; 'x
(leftmost-fixed '(((x) ()) () (e))) ; 'x
(leftmost-fixed '(((() x) ())))     ; 'x
(leftmost-fixed '(((()) ())))       ; '()

(define leftmost-let
  (lambda (l)
    (cond
      ((null? l) '())
      ((atom? (car l)) (car l))
      (else
        (let ((a (leftmost-let (car l))))
          (cond
            ((atom? a) a)
            (else (leftmost-let (cdr l)))))))))

; Tests of leftmost-let
;
(leftmost-let '(((y) b) (c d)))   ; 'y
(leftmost-let '(((y) ()) () (e))) ; 'y
(leftmost-let '(((() y) ())))     ; 'y
(leftmost-let '(((()) ())))       ; '()

; The rember1* function removes the leftmost occurrence of a in l
;
(define rember1*
  (lambda (a l)
    (cond
      ((null? l) '())
      ((atom? (car l))
       (cond
         ((eq? (car l) a) (cdr l))
         (else
           (cons (car l) (rember1* a (cdr l))))))
      (else
        (cond
          ((equal? (rember1* a (car l)) (car l)) ; if the list with 'a' removed doesn't change
           (cons (car l) (rember1* a (cdr l))))  ; then recurse
          (else
            (cons (rember1* a (car l)) (cdr l)))))))) ; otherwise remove 'a'

; Examples of rember1*
;
(rember1*
  'salad
  '((Swedish rye) (French (mustard salad turkey)) salad))
; ==> '((Swedish rye) (French (mustard turkey)) salad)

(rember1*
  'meat
  '((pasta meat) pasta (noodles meat sauce) meat tomatoes))
; ==> '((pasta) pasta (noodles meat sauce) meat tomatoes)

; rember1* rewritten using the 12th commandment
;
(define rember1*-letrec
  (lambda (a l)
    (letrec
      ((R (lambda (l)
            (cond
              ((null? l) '())
              ((atom? (car l))
               (cond
                 ((eq? (car l) a) (cdr l))
                 (else
                   (cons (car l) (R (cdr l))))))
              (else
                (cond
                  ((equal? (R (car l)) (car l)) ; if the list with 'a' removed doesn't change
                   (cons (car l) (R (cdr l))))  ; then recurse
                  (else
                    (cons (R (car l)) (cdr l))))))))) ; otherwise remove 'a'
      (R l))))

; Tests of rember1*-letrec
;
(rember1*-letrec
  'salad
  '((Swedish rye) (French (mustard salad turkey)) salad))
; ==> '((Swedish rye) (French (mustard turkey)) salad)

(rember1*-letrec
  'meat
  '((pasta meat) pasta (noodles meat sauce) meat tomatoes))
; ==> '((pasta) pasta (noodles meat sauce) meat tomatoes)

; rember* rewritten using the 12th commandment and let
;
(define rember1*-letrec-let
  (lambda (a l)
    (letrec
      ((R (lambda (l)
            (cond
              ((null? l) '())
              ((atom? (car l))
               (cond
                 ((eq? (car l) a) (cdr l))
                 (else
                   (cons (car l) (R (cdr l))))))
              (else
                (let ((av (R (car l))))
                  (cond
                    ((equal? (car l) av)         ; if the list with 'a' removed didn't change
                     (cons (car l) (R (cdr l)))) ; then recurse
                    (else
                      (cons av (cdr l))))))))))  ; otherwise remove 'a'
      (R l))))

; Tests of rember1*-letrec-let
;
(rember1*-letrec-let
  'salad
  '((Swedish rye) (French (mustard salad turkey)) salad))
; ==> '((Swedish rye) (French (mustard turkey)) salad)

(rember1*-letrec-let
  'meat
  '((pasta meat) pasta (noodles meat sauce) meat tomatoes))
; ==> '((pasta) pasta (noodles meat sauce) meat tomatoes)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                            ;
; The fifteenth commandment (preliminary version)                            ;
;                                                                            ;
; Use (let ...) to name the values of repeated expressions.                  ;
;                                                                            ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; The depth* function finds the max depth of an s-expression
;
(define depth*
  (lambda (l)
    (cond
      ((null? l) 1)
      ((atom? (car l)) (depth* (cdr l)))
      (else
        (cond
          ((> (depth* (cdr l))
              (add1 (depth* (car l))))
           (depth* (cdr l)))
          (else
            (add1 (depth* (car l)))))))))

; Examples of depth*
;
(depth* '((pickled) peppers (peppers pickled)))                          ; 2
(depth* '(margarine ((bitter butter) (makes) (batter (bitter))) butter)) ; 4
(depth* '(c (b (a b) a) a))                                              ; 3

; depth* rewritten using let
;
(define depth*-let
  (lambda (l)
    (cond
      ((null? l) 1)
      ((atom? (car l)) (depth*-let (cdr l)))
      (else 
        (let ((a (add1 (depth*-let (car l))))
              (d (depth*-let (cdr l))))
          (cond
            ((> d a) d)
            (else a)))))))

; Tests of depth*-let
;
(depth*-let '((pickled) peppers (peppers pickled)))                          ; 2
(depth*-let '(margarine ((bitter butter) (makes) (batter (bitter))) butter)) ; 4
(depth*-let '(c (b (a b) a) a))                                              ; 3

; Another version of depth*
;
(define depth*-let-2
  (lambda (l)
    (cond
      ((null? l) 1)
      (else
        (let ((d (depth*-let-2 (cdr l))))
          (cond
            ((atom? (car l)) d)
            (else
              (let ((a (add1 (depth*-let-2 (car l)))))
                (cond
                  ((> d a) d)
                  (else a))))))))))

; Tests of depth*-let
;
(depth*-let-2 '((pickled) peppers (peppers pickled)))                          ; 2
(depth*-let-2 '(margarine ((bitter butter) (makes) (batter (bitter))) butter)) ; 4
(depth*-let-2 '(c (b (a b) a) a))                                              ; 3

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                            ;
; The fifteenth commandment (revised version)                                ;
;                                                                            ;
; Use (let ...) to name the values of repeated expressions in a function     ;
; definition if they may be evaluated twice for one and the same use of the  ;
; function.                                                                  ;
;                                                                            ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; depth* via if
;
(define depth*-if
  (lambda (l)
    (cond
      ((null? l) 1)
      ((atom? (car l))
       (depth*-if (cdr l)))
      (else
        (let ((a (add1 (depth* (car l))))
              (d (depth* (cdr l))))
          (if (> d a) d a))))))

; Tests of depth*-if
;
(depth*-if '((pickled) peppers (peppers pickled)))                          ; 2
(depth*-if '(margarine ((bitter butter) (makes) (batter (bitter))) butter)) ; 4
(depth*-if '(c (b (a b) a) a))                                              ; 3

; depth* via max
;
(define depth*-max
  (lambda (l)
    (cond
      ((null? l) 1)
      ((atom? (car l))
       (depth*-max (cdr l)))
      (else
        (max
          (add1 (depth*-max (car l)))
          (depth*-max (cdr l)))))))

; Tests of depth*-max
;
(depth*-max '((pickled) peppers (peppers pickled)))                          ; 2
(depth*-max '(margarine ((bitter butter) (makes) (batter (bitter))) butter)) ; 4
(depth*-max '(c (b (a b) a) a))                                              ; 3

; Not interested in scramble problem, skipping it.

; leftmost via letcc
;
(define leftmost-letcc
  (lambda (l)
    (call-with-current-continuation
      (lambda (skip)
        (lm l skip)))))

; lm is leftmost-letcc's helper function
;
(define lm
  (lambda (l out)
    (cond
      ((null? l) '())
      ((atom? (car l)) (out (car l)))
      (else
        (let () ; can also use 'begin'
          (lm (car l) out)
          (lm (cdr l) out))))))

(leftmost-letcc '(((x) b) (c d)))   ; 'x
(leftmost-letcc '(((x) ()) () (e))) ; 'x
(leftmost-letcc '(((() x) ())))     ; 'x
(leftmost-letcc '(((()) ())))       ; '()

; letfmost following 13th and 14th commandments
;
(define leftmost-1314
  (letrec
    ((lm (lambda (l out)
           (cond
             ((null? l) '())
             ((atom? (car l)) (out (car l)))
             (else
               (begin
                 (lm (car l) out)
                 (lm (cdr l) out)))))))
    (lambda (l)
      (call-with-current-continuation
        (lambda (skip)
          (lm l skip))))))

(leftmost-1314 '(((x) b) (c d)))   ; 'x
(leftmost-1314 '(((x) ()) () (e))) ; 'x
(leftmost-1314 '(((() x) ())))     ; 'x
(leftmost-1314 '(((()) ())))       ; '()

; another way to follow 13th and 14th commandments
;
(define leftmost-13142
  (lambda (l)
      (letrec
        ((lm (lambda (l out)
               (cond
                 ((null? l) '())
                 ((atom? (car l)) (out (car l)))
                 (else
                   (begin
                     (lm (car l) out)
                     (lm (cdr l) out)))))))
        (call-with-current-continuation
          (lambda (skip)
            (lm l skip))))))

(leftmost-13142 '(((x) b) (c d)))   ; 'x
(leftmost-13142 '(((x) ()) () (e))) ; 'x
(leftmost-13142 '(((() x) ())))     ; 'x
(leftmost-13142 '(((()) ())))       ; '()

; yet another way
;
(define leftmost-131422
  (lambda (l)
    (call-with-current-continuation
      (lambda (skip)
        (letrec
          ((lm (lambda (l)
                 (cond
                   ((null? l) '())
                   ((atom? (car l)) (skip (car l)))
                   (else
                     (begin
                       (lm (car l))
                       (lm (cdr l))))))))
          (lm l))))))

(leftmost-131422 '(((x) b) (c d)))   ; 'x
(leftmost-131422 '(((x) ()) () (e))) ; 'x
(leftmost-131422 '(((() x) ())))     ; 'x
(leftmost-131422 '(((()) ())))       ; '()

; rember1* via letcc
;
(define rember1*-letcc
  (lambda (a l)
    (letrec
      ((rm (lambda (a l oh)
             (cond
               ((null? l) (oh 'no))
               ((atom? (car l))
                (if (eq? (car l) a)
                  (cdr l)
                  (cons (car l) (rm a (cdr l) oh))))
               (else
                 (let ((new-car
                         (call-with-current-continuation
                           (lambda (oh)
                             (rm a (car l) oh)))))
                   (if (atom? new-car)
                     (cons (car l) (rm a (cdr l) oh))
                     (cons new-car (cdr l)))))))))
      (let ((new-l
              (call-with-current-continuation
                (lambda (oh)
                  (rm a l oh)))))
        (if (atom? new-l)
          l
          new-l)))))

; Tests of rember1*-letcc
;
(rember1*-letcc
  'salad
  '((Swedish rye) (French (mustard salad turkey)) salad))
; ==> '((Swedish rye) (French (mustard turkey)) salad)

(rember1*-letcc
  'meat
  '((pasta meat) pasta (noodles meat sauce) meat tomatoes))
; ==> '((pasta) pasta (noodles meat sauce) meat tomatoes)

(rember1*-letcc
  'a
  '((foo bar) baz))
; ==> '((foo bar) baz)

;
; Go get yourself this wonderful book and have fun with the Scheme language!
;
; Shortened URL to the book at Amazon.com: http://bit.ly/8cyjgw
;
; Sincerely,
; Peteris Krumins
; http://www.catonmat.net
;

