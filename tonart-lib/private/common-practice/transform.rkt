#lang racket

(require tonart (for-syntax racket/match syntax/parse tonart/liszt))

(define-art-object (transforms [forms]))

(begin-for-syntax
  (struct transform/s [body]))

(define-syntax (define-transform stx)
  (syntax-parse stx
    [(_ name body)
     #'(define-syntax name (transform/s body))]))

(define-transform L
  (λ (chord)
    (match chord
      [(list p a (or 'major 'M)) (append (transpose-by-interval p a 3 'major) (list 'm))]
      [(list p a (or 'minor 'm)) (append (transpose-by-interval p a 6 'minor) (list 'M))])))

(define-transform P
  (λ (chord)
    (match chord
      [(list p a (or 'major 'M)) (list p a 'm)]
      [(list p a (or 'minor 'm)) (list p a 'M)])))

(define-transform R
  (λ (chord)
    (match chord
      [(list p a (or 'major 'M)) (append (transpose-by-interval p a 6 'major) (list 'm))]
      [(list p a (or 'minor 'm)) (append (transpose-by-interval p a 3 'minor) (list 'M))])))

(define-syntax (define-transform-alias stx)
  (syntax-parse stx
    [(_ name [tform:id ...])
     #'(define-syntax name 
         (transform/s 
           (λ (chord) (let* ([chord (run-transform #'tform chord)] ...) chord))))]))

(define-transform-alias N [R L P])
(define-transform-alias S [L P R])
(define-transform-alias H [L P L])

(define-for-syntax (run-transform name chord)
  ((transform/s-body (syntax-local-value name)) chord))

(define-mapping-rewriter (run-transforms [(: tr transforms)]) 
  (λ (stx tr)
    (syntax-parse tr
      [(_ lists ...)
       #:do [(define chords (context-ref*/within (current-ctxt) (get-id-ctxt tr) #'chord))]
       #:with ((result ...) ...)
         (for/list ([ch chords])
           (for/fold ([acc (list ch)] #:result (reverse acc)) 
                     ([l (syntax->list #'(lists ...))])
             (syntax-parse l
               [(name* ...)
                (cons 
                  (for/fold ([ch (car acc)]) 
                            ([name (syntax->list #'(name* ...))])
                    (liszt->chord (run-transform name (chord->liszt ch))))
                  acc)])))
       (qq-art tr (context #,@(map delete-expr chords) (seq (ix-- result ...)) ...))])))

#;(qr (transforms [L P] [P] [N]) (chord c 0 M) (run-transforms))

(provide (all-defined-out) (for-syntax (all-defined-out)))
