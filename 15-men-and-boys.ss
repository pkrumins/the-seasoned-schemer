;
; Chapter 15 of The Seasoned Schemer:
; The Difference Between Men and Boys...
;
; Code examples assemled by Peteris Krumins (peter@catonmat.net).
; His blog is at http://www.catonmat.net  --  good coders code, great reuse.
;
; Get yourself this wonderful book at Amazon: http://bit.ly/8cyjgw
;

(define x
  (cons 'chicago
        (cons 'pizza '())))

(set! x 'gone)
(set! x 'skins)

(define gourmet
  (lambda (food)
    (cons food
          (cons x '()))))

(gourmet 'onion)    ; '(onion skins)

(set! x 'rings)

(gourmet 'onion)    ; '(onion rings)

(define gourmand
  (lambda (food)
    (set! x food)
    (cons food
          (cons x '()))))

(gourmand 'potato)  ; '(potato potato)

(gourmand 'rice)    ; '(rice rice)

(define dinerR
  (lambda (food)
    (set! x food)
    (cons 'milkshake
          (cons food '()))))

(dinerR 'onion)     ; '(milkshake onion)

(dinerR 'pecanpie)  ; '(milkshake pecanpie)

(define omnivore
  (let ((x 'minestrone))
    (lambda (food)
      (set! x food)
      (cons food (cons x '())))))

(omnivore 'bouillabaisse)   ; '(bouillabaisse bouillabaisse)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                            ;
; The sixteenth commandment                                                  ;
;                                                                            ;
; Use (set! ...) only with names defined in (let ...)s.                      ;
;                                                                            ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define gobbler
  (let ((x 'minestrone))
    (lambda (food)
      (set! x food)
      (cons food (cons x '())))))

(gobbler 'gumbo)            ; '(gumbo gumbo)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                            ;
; The seventeenth commandment (preliminary version)                          ;
;                                                                            ;
; Use (set! ...) for (let ((x ...)) ...) only if there is at least one       ;
; (lambda ... between it and the (let ((x ...)) ...).                        ;
;                                                                            ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                                            ;
; The eighteenth commandment                                                 ;
;                                                                            ;
; Use (set! x ...) only when the value that x refers to is no longer needed. ;
;                                                                            ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define chez-nous
  (lambda ()
    (let ((a food))
      (set! food x)
      (set! x a))))

(define food 'none)

(define glutton
  (lambda (x)
    (set! food x)
    (cons 'more
          (cons x
                (cons 'more
                      (cons x
                            '()))))))
(dinerR 'milk)
(glutton 'cream)

; x is milk
; food is cream

x
food

(chez-nous) ; x is now cream, food is now milk

x
food

(chez-nous) ; x is now milk, food is now cream

x
food

; There is more stuff in the book

;
; Go get yourself this wonderful book and have fun with the Scheme language!
;
; Shortened URL to the book at Amazon.com: http://bit.ly/8cyjgw
;
; Sincerely,
; Peteris Krumins
; http://www.catonmat.net
;

