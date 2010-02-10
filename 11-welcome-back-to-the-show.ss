;
; Chapter 11 of The Seasoned Schemer:
; Welcome Back to the Show
;
; Code examples assemled by Peteris Krumins (peter@catonmat.net).
; His blog is at http://www.catonmat.net  --  good coders code, great reuse.
;
; Get yourself this wonderful book at Amazon: http://bit.ly/8cyjgw
;

; Remember member? from The Little Schemer? (http://bit.ly/4GjWdP)
; It finds if an element 'a' is in a list of atoms 'lat'.
;
(define member?
  (lambda (a lat)
    (cond
      ((null? lat) #f)
      (else (or (eq? a (car lat))
                (member? a (cdr lat)))))))

; is-first? function finds out whether the next element in lat, if there is
; one, is identical to this element.
;
(define is-first?
  (lambda (a lat)
    (cond
      ((null? lat) #f)
      (else (eq? (car lat) a)))))

; two-in-a-row? function determines whether any atom occurs twice in a row.
;
(define two-in-a-row?
  (lambda (lat)
    (cond
      ((null? lat) #f)
      (else
        (or (is-first? (car lat) (cdr lat))
            (two-in-a-row? (cdr lat)))))))

; Examples of two-in-a-row?
;
(two-in-a-row? '(Italian sardines spaghetti parsley))           ; false
(two-in-a-row? '(Italian sardines sardines spaghetti parsley))  ; true
(two-in-a-row? '(Italian sardines more sardines spaghetti))     ; false

; Another version of two-in-a-row? that leaves decision of what to do to
; is-first-b?
;
(define two-in-a-row-2?
  (lambda (lat)
    (cond
      ((null? lat) #f)
      (else
       (is-first-b? (car lat) (cdr lat))))))

; is-first-b? function for two-in-a-row-2?
;
(define is-first-b?
  (lambda (a lat)
    (cond
      ((null? lat) #f)
      (else (or (eq? (car lat) a)
                (two-in-a-row-2? lat))))))

; Examples of two-in-a-row-2?
;
(two-in-a-row-2? '(Italian sardines spaghetti parsley))           ; false
(two-in-a-row-2? '(Italian sardines sardines spaghetti parsley))  ; true
(two-in-a-row-2? '(Italian sardines more sardines spaghetti))     ; false

; Another version of two-in-a-row? that recurs itself instead of using
; is-first?
;
(define two-in-a-row-b?
  (lambda (preceding lat)
    (cond
      ((null? lat) #f)
      (else (or (eq? (car lat) preceding)
                (two-in-a-row-b? (car lat) (cdr lat)))))))

; The final version of two-in-a-row?
;
(define two-in-a-row-final?
  (lambda (lat)
    (cond
      ((null? lat) #f)
      (else (two-in-a-row-b? (car lat) (cdr lat))))))

(two-in-a-row-final? '(Italian sardines spaghetti parsley))           ; false
(two-in-a-row-final? '(Italian sardines sardines spaghetti parsley))  ; true
(two-in-a-row-final? '(Italian sardines more sardines spaghetti))     ; false

; Helper function for upcoming sum-of-prefixes function
;
(define sum-of-prefixes-b
  (lambda (sonssf tup)     ; sonssf stands for 'sum of numbers seen so far'
    (cond
      ((null? tup) '())
      (else (cons (+ sonssf (car tup))
                  (sum-of-prefixes-b
                   (+ sonssf (car tup))
                   (cdr tup)))))))

; sum-of-prefixes function finds the running sum of a list of numbers
;
(define sum-of-prefixes
  (lambda (tup)
    (sum-of-prefixes-b 0 tup)))

; Examples of sum-of-prefixes
;
(sum-of-prefixes '(2 1 9 17 0))   ; '(2 3 12 29 29)
(sum-of-prefixes '(1 1 1 1 1))    ; '(1 2 3 4 5)
(sum-of-prefixes '(1 1 1))        ; '(1 2 3)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                            ;
; The eleventh commandment                                                   ;
;                                                                            ;
; Use additional arguments when a function needs to know what the other      ;
; arguments to the function have been like so far.                           ;
;                                                                            ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Remember the pick function from chapter 4 of The Little Schemer?
;
(define pick
  (lambda (n lat)
    (cond
      ((one? n) (car lat))
      (else (pick (sub1 n) (cdr lat))))))

; It uses one? and sub1 helper functions
;
(define one?
  (lambda (n) (= n 1)))

(define sub1
  (lambda (n) (- n 1)))

; scramble-b is a helper function for scramble
;
(define scramble-b
  (lambda (tup rev-pre)
    (cond
      ((null? tup) '())
      (else
       (cons (pick (car tup) (cons (car tup) rev-pre))
             (scramble-b (cdr tup)
                         (cons (car tup) rev-pre)))))))

; scramble
(define scramble
  (lambda (tup)
    (scramble-b tup '())))

; Examples of scramble
;
(scramble '(1 1 1 3 4 2 1 1 9 2))       ; '(1 1 1 1 1 4 1 1 1 9)
(scramble '(1 2 3 4 5 6 7 8 9))         ; '(1 1 1 1 1 1 1 1 1)
(scramble '(1 2 3 1 2 3 4 1 8 2 10))    ; '(1 1 1 1 1 1 1 1 2 8 2)

;
; Go get yourself this wonderful book and have fun with the Scheme language!
;
; Shortened URL to the book at Amazon.com: http://bit.ly/8cyjgw
;
; Sincerely,
; Peteris Krumins
; http://www.catonmat.net
;

