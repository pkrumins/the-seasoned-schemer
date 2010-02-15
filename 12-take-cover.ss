;
; Chapter 12 of The Seasoned Schemer:
; Take Cover
;
; Code examples assemled by Peteris Krumins (peter@catonmat.net).
; His blog is at http://www.catonmat.net  --  good coders code, great reuse.
;
; Get yourself this wonderful book at Amazon: http://bit.ly/8cyjgw
;

; add1 primitive
;
(define add1
  (lambda (x) (+ x 1)))

; The Y-combinator
;
(define Y
  (lambda (le)
    ((lambda (f) (f f))
     (lambda (f)
       (le (lambda (x) ((f f) x)))))))

; length function written via Y-combinator and applied to '(a b c)
;
((Y (lambda (length)
     (lambda (l)
       (cond
         ((null? l) 0)
         (else (add1 (length (cdr l)))))))) '(a b c))  ; produces output 3

; No need to pass 'a' around in multirember
; Use Y-combinator not to pass it around
;
(define multirember
  (lambda (a lat)
    ((Y (lambda (mr)
          (lambda (lat)
            (cond
              ((null? lat) '())
              ((eq? a (car lat)) (mr (cdr lat)))
              (else
               (cons (car lat) (mr (cdr lat))))))))
     lat)))

; Example of multirember
;
(multirember 'a '(a b c a a a x))             ; '(b c x)

; multirember via letrec
;
(define multirember-letrec
  (lambda (a lat)
    ((letrec
       ((mr (lambda (lat)
              (cond
                ((null? lat) '())
                ((eq? a (car lat)) (mr (cdr lat)))
                (else
                  (cons (car lat) (mr (cdr lat))))))))
       mr)
     lat)))

; Example of multirember-letrec
;
(multirember-letrec 'a '(a b c a a a x))      ; '(b c x) 

; Structure of letrec
;
; ((letrec ((mr ...)) mr) values)

; Another way to write multirember via letrec
;
(define multirember-letrec-2
  (lambda (a lat)
    (letrec
      ((mr (lambda (lat)
             (cond
               ((null? lat) '())
               ((eq? a (car lat)) (mr (cdr lat)))
               (else
                 (cons (car lat) (mr (cdr lat))))))))
       (mr lat))))

; Another test of applying multirember-letrec-2
;
(multirember-letrec-2 'a '(a b c a a a x))      ; '(b c x) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                            ;
; The twelfth commandment                                                    ;
;                                                                            ;
; Use (letrec ...) to remove arguments that do not change for recursive      ;
; applications.                                                              ;
;                                                                            ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Yet another way to write multirember via letrec
;
(define multirember-letrec-3
  (letrec
    ((mr (lambda (a lat)
           (cond
             ((null? lat) '())
             ((eq? (car lat) a) (mr a (cdr lat)))
             (else
               (cons (car lat) (mr a (cdr lat))))))))
    mr))

; Test multirember-letrec-3
;
(multirember-letrec-3 'a '(a b c a a a x))      ; '(b c x) 

; The member? function determines if the given element is in the list
;
(define member?
  (lambda (a lat)
    (cond
      ((null? lat) #f)
      ((eq? (car lat) a) #t)
      (else (member? a (cdr lat))))))

; Test member?
;
(member? 'x '(a b c x d e f))   ; #t
(member? 'x '(a b c d e f))     ; #f

; member? via letrec
;
(define member-letrec?
  (lambda (a l)
    ((letrec
       ((yes? (lambda (l)
                (cond
                  ((null? l) #f)
                  ((eq? (car l) a) #t)
                  (else (yes? (cdr l)))))))
     yes?)
    l)))

; Test member-letrec?
;
(member-letrec? 'x '(a b c x d e f))   ; #t
(member-letrec? 'x '(a b c d e f))     ; #f

; Another member? via letrec
;
(define member-letrec-2?
  (lambda (a l)
    (letrec
      ((yes? (lambda (l)
               (cond
                 ((null? l) #f)
                 ((eq? (car l) a) #t)
                 (else (yes? (cdr l)))))))
      (yes? l))))

; Test member-letrec-2?
;
(member-letrec-2? 'x '(a b c x d e f))   ; #t
(member-letrec-2? 'x '(a b c d e f))     ; #f

; The union function takes two sets and merges them
;
(define union
  (lambda (set1 set2)
    (cond
      ((null? set1) set2)
      ((member? (car set1) set2) 
       (union (cdr set1) set2))
      (else
        (cons (car set1) (union (cdr set1) set2))))))

; Example of union
;
(union
  '(tomatoes and macaroni casserole)
  '(macaroni and cheese)) ; '(tomatoes and macaroni casserole cheese)

; union via letrec
;
(define union-letrec
  (lambda (set1 set2)
    (letrec
      ((U (lambda (set)
            (cond
              ((null? set) set2)
              ((member? (car set) set2)
               (U (cdr set)))
              (else
                (cons (car set) (U (cdr set))))))))
      (U set1))))

; Test of union-letrec
;
(union-letrec
  '(tomatoes and macaroni casserole)
  '(macaroni and cheese)) ; '(tomatoes and macaroni casserole cheese)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                            ;
; The thirteenth commandment                                                 ;
;                                                                            ;
; Use (letrec ...) to hide and to protect functions.                         ;
;                                                                            ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define union-letrec-protected
  (lambda (set1 set2)
    (letrec
      ((U (lambda (set)
            (cond
              ((null? set) set2)
              ((M? (car set) set2)
               (U (cdr set)))
              (else
                (cons (car set) (U (cdr set)))))))
       (M?
         (lambda (a lat)
           (cond
             ((null? lat) #f)
             ((eq? (car lat) a) #t)
             (else
               (M? a (cdr lat)))))))
      (U set1))))

; Test of union-letrec-protected
;
(union-letrec-protected
  '(tomatoes and macaroni casserole)
  '(macaroni and cheese)) ; '(tomatoes and macaroni casserole cheese)

; Fixing M? to follow 12th commandment
;
(define union-letrec-protected-12
  (lambda (set1 set2)
    (letrec
      ((U (lambda (set)
            (cond
              ((null? set) set2)
              ((M? (car set) set2)
               (U (cdr set)))
              (else
                (cons (car set) (U (cdr set)))))))
       (M?
         (lambda (a lat)
           (letrec
             ((N? (lambda (lat)
                    (cond
                      ((null? lat) #f)
                      ((eq? (car lat) a) #t)
                      (else
                        (N? (cdr lat)))))))
             (N? lat)))))
      (U set1))))

; Test of union-letrec-protected-12
;
(union-letrec-protected-12
  '(tomatoes and macaroni casserole)
  '(macaroni and cheese)) ; '(tomatoes and macaroni casserole cheese)

; The two-in-a-row? checks if a lat contains two equal elements
;
(define two-in-a-row?
  (lambda (lat)
    (letrec
      ((W (lambda (a lat)
            (cond
              ((null? lat) #f)
              ((eq? a (car lat)) #t)
              (else
                (W (car lat) (cdr lat)))))))
      (cond
        ((null? lat) #f)
        (else (W (car lat) (cdr lat)))))))

; Test two-in-a-row?
; 
(two-in-a-row? '(Italian sardines spaghetti parsley))           ; #f
(two-in-a-row? '(Italian sardines sardines spaghetti parsley))  ; #t
(two-in-a-row? '(Italian sardines more sardines spaghetti))     ; #f

; Another version of two-in-a-row?
;
(define two-in-a-row-2?
  (letrec
    ((W (lambda (a lat)
          (cond
            ((null? lat) #f)
            ((eq? a (car lat)) #t)
            (else
              (W (car lat) (cdr lat)))))))
    (lambda (lat)
      (cond
        ((null? lat) #f)
        (else (W (car lat) (cdr lat)))))))

; Test two-in-a-row-2?
;
(two-in-a-row-2? '(Italian sardines spaghetti parsley))           ; #f
(two-in-a-row-2? '(Italian sardines sardines spaghetti parsley))  ; #t
(two-in-a-row-2? '(Italian sardines more sardines spaghetti))     ; #f

; The sum-of-prefixes finds the running sum of a list of numbers
;
(define sum-of-prefixes
  (lambda (tup)
    (letrec
      ((S (lambda (sss tup)
            (cond
              ((null? tup) '())
              (else
                (cons (+ sss (car tup))
                      (S (+ sss (car tup)) (cdr tup))))))))
      (S 0 tup))))

; Examples of sum-of-prefixes
;
(sum-of-prefixes '(2 1 9 17 0))   ; '(2 3 12 29 29)
(sum-of-prefixes '(1 1 1 1 1))    ; '(1 2 3 4 5)
(sum-of-prefixes '(1 1 1))        ; '(1 2 3)

; TODO: scramble function (i am not interested in it atm)

;
; Go get yourself this wonderful book and have fun with the Scheme language!
;
; Shortened URL to the book at Amazon.com: http://bit.ly/8cyjgw
;
; Sincerely,
; Peteris Krumins
; http://www.catonmat.net
;

