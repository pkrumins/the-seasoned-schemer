;
; Chapter 18 of The Seasoned Schemer:
; We Change, Therefore We Are the Same!
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

; The lots function creates lots of eggs
;
(define lots
  (lambda (n)
    (cond
      ((zero? n) '())
      (else
        (cons 'egg (lots (sub1 n)))))))

; The lenkth function counts the eggs
;
(define lenkth
  (lambda (l)
    (cond
      ((null? l) 0)
      (else
        (add1 (lenkth (cdr l)))))))

; Examples of lots and lenkth
;
(lots 3)                ; '(egg egg egg)
(lots 5)                ; '(egg egg egg egg egg)
(lots 12)               ; '(egg egg egg egg egg egg egg egg egg egg egg egg)
(lenkth (lots 3))       ; 3
(lenkth (lots 5))       ; 5
(lenkth (lots 15))      ; 15

; Create 4 eggs from 3 eggs
;
(cons 'egg (lots 3))    ; '(egg egg egg egg)

; consC, counter, set-counter from chapter 17
;
(define counter (lambda() 0))
(define set-counter (lambda () 0))
(define consC
  (let ((N 0))
    (set! counter (lambda() N))
    (set! set-counter
      (lambda (x) (set! N x)))
    (lambda (x y)
      (set! N (add1 N))
      (cons x y))))

; Add an egg at the end
;
(define add-at-end
  (lambda (l)
    (cond
      ((null? (cdr l))
       (consC (car l) (cons 'egg-end '())))
      (else
        (consC (car l) (add-at-end (cdr l)))))))

; Example of add-at-end
;
(add-at-end (lots 3))   ; '(egg egg egg egg-end)

(counter)               ; 3

; Add an egg at the end without making any new conses except for the last one
;
(define add-at-end-too
  (lambda (l)
    (letrec
      ((A (lambda (ls)
            (cond
              ((null? (cdr ls))
               (set-cdr! ls (cons 'egg-end2 '())))
              (else
                (A (cdr ls)))))))
      (A l)
      l)))

; Example of add-at-end-too
;
(set-counter 0)
(add-at-end-too (lots 3))   ; '(egg egg egg egg-end2)
(counter)                   ; 0

; kons the magnificent
;
(define kons
  (lambda (kar kdr)
    (lambda (selector)          ; returns lambda (selector)
      (selector kar kdr))))     ; calls selector with kar and kdr arguments

; kar
;
(define kar
  (lambda (c)                   ; applies selector on (a d) and returns 'a (car)
    (c (lambda (a d) a))))

; kdr
;
(define kdr
  (lambda (c)                   ; applies selector on (a d) and returns d (cdr)
    (c (lambda (a d) d))))

; Examples of kons kar kdr
;
(kar (kons 'a '()))                 ; 'a
(kdr (kons 'a '()))                 ; '()
(kar (kdr (kons 'a (kons 'b '())))) ; 'b
(kar (kons 'a (kons 'b '())))       ; 'a

; Another cons
;
(define bons
  (lambda (kar)
    (let ((kdr '()))
      (lambda (selector)
        (selector
          (lambda (x) (set! kdr x))
          kar
          kdr)))))

; Another kar
;
(define bar
  (lambda (c)
    (c (lambda (s a d) a))))

; Another kdr
;
(define bdr
  (lambda (c)
    (c (lambda (s a d) d))))

; set-kdr
;
(define set-kdr
  (lambda (c x)
    ((c (lambda (s a d) s)) x)))

; create kons using set-kdr and bons
;
(define kons2
  (lambda (a d)
    (let ((c (bons a)))
      (set-kdr c d)
      c)))

; Example of kons2 bar and bdr
;
(bar (kons2 'a '(1 2 3)))       ; 'a
(bdr (kons2 'a '(1 2 3)))       ; '(1 2 3)

; Now eggs again
;
(define dozen (lots 12))

dozen           ; '(egg egg egg egg egg egg egg egg egg egg egg egg)
                ; used 12 conses

(define bakers-dozen (add-at-end dozen))

bakers-dozen    ; '(egg egg egg egg egg egg egg egg egg egg egg egg egg-end)
                ; used 13 conses (25 total now)

(define bakers-dozen-too (add-at-end-too dozen))

bakers-dozen-too    ; '(egg egg egg egg egg egg egg egg egg egg egg egg egg-end egg-end2)
                    ; used 1 cons (26 total)

(define bakers-dozen-again (add-at-end dozen))

bakers-dozen-again  ; '(egg egg egg egg egg egg egg egg egg egg egg egg egg-end egg-end2 egg-end)
                    ; used 14 conses (40 total)

(define same?
  (lambda (c1 c2)
    (cond
      ((and (null? c1) (null? c2)) #t)
      ((or (null? c1) (null? c2)) #f)
      (else
       (let ((t1 (cdr c1))
             (t2 (cdr c2)))
         (set-cdr! c1 1)
         (set-cdr! c2 2)
         (let ((v (= (cdr c1) (cdr c2))))
           (set-cdr! c1 t1)
           (set-cdr! c2 t2)
           v))))))

(same? dozen dozen)                     ; #t
(same? dozen bakers-dozen)              ; #f
(same? dozen bakers-dozen-too)          ; #t
(same? dozen bakers-dozen-again)        ; #f
(same? bakers-dozen bakers-dozen-too)   ; #f   ;;; the book says #t???

; The last-kons function returns the last cons in a non-empty kons-list
;
(define last-kons
  (lambda (ls)
    (cond
      ((null? (cdr ls)) ls)
      (else (last-kons (cdr ls))))))

(define long (lots 12))                 ; '(egg egg egg egg egg egg egg egg egg egg egg egg)

(set-cdr! (last-kons long) long)        ; #0 = '(egg egg egg egg egg egg egg egg egg egg egg . #0#) 

; The finite-lenkth function returns length of a list
; or #f if it's an infinite list
;
(define finite-lenkth
  (lambda (p)
    (call-with-current-continuation
      (lambda (infinite)
        (letrec
          ((C (lambda (p q)
                (cond
                  ((same? p q) (infinite #f))
                  ((null? q) 0)
                  ((null? (cdr q)) 1)
                  (else
                    (+ (C (sl p) (qk q)) 2)))))
           (qk (lambda (x) (cdr (cdr x))))
           (sl (lambda (x) (cdr x))))
          (cond
            ((null? p) 0)
            (else
              (add1 (C p (cdr p))))))))))

; Examples of finite-lenkth
;
(define not-so-long (lots 5))         ; '(egg egg egg egg egg)
(finite-lenkth not-so-long)           ; 5
(finite-lenkth long)                  ; #f

; Guy's Favorite Pie
;
(define mongo
  (cons 'pie
   (cons 'a
    (cons 'la
     (cons 'mode '())))))
(set-cdr! (cdr (cdr (cdr mongo))) (cdr mongo))

; mongo

;
; Go get yourself this wonderful book and have fun with the Scheme language!
;
; Shortened URL to the book at Amazon.com: http://bit.ly/8cyjgw
;
; Sincerely,
; Peteris Krumins
; http://www.catonmat.net
;

