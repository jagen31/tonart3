#lang racket

(require art art/timeline tonart/private/rewriter/lib tonart/private/realizer/electronic/lib 
  racket/runtime-path rsound rsound/envelope sf2-parser
  (for-syntax syntax/parse racket/match racket/list racket/string racket/dict) )
(provide (all-defined-out))

;; load fluid by default
(define-runtime-path soundfont-path "../resources/sf2")
(define fluid
  (parse-soundfont
   (open-input-file
    (build-path soundfont-path "FluidR3_GM.sf2"))))

;;;;;;;;;; realizer FAMILIES- use other realizers to stream/make a sound.
;; stream directly to pstream
(define-syntax (define-composite-pstream-realizer stx)
  (syntax-parse stx
    [(_ n:id {subrealizer:id ...})
     #'(begin
         (define-composite-realizer n {subrealizer ...} [(define pstream (make-pstream))] 
           (λ(clauses) 
              #`(let ()
                 #,@(for/list ([expr clauses])
                      #`(let ([expr* #,expr])
                          (pstream-queue pstream (rs-scale 0.25 (cdr expr*)) (car expr*))))))))]))

;; create an rsound
(define-syntax (define-composite-rsound-realizer stx)
  (syntax-parse stx
    [(_ n:id {subrealizer:id ...})
     #'(begin
         (define-composite-realizer n {subrealizer ...} [(define pstream (make-pstream))] 
           (λ(clauses) 
              (define result #`(let ()
                 #,(for/fold ([acc #'(silence 1)])
                              ([expr clauses])
                      #`(let* ([expr* #,expr]
                               [sound (rs-scale 1 (cdr expr*))]
                               [silence+sound (if (zero? (car expr*)) sound (rs-append (silence (car expr*)) sound))])
                          (rs-overlay (rs-scale 0.05 silence+sound) #,acc)))))
         #`(rs-scale 4 #,result))))]))

;; subrealizer for performing tones from a context
(define (get-duration start end tempo)
  (round (* (/ (- end start) (/ tempo 60)) (default-sample-rate))))
(define-subrealizer tone-subrealizer
  (λ(ctxt)
    (for/foldr ([acc '()])
               ([stx ctxt])
      (syntax-parse stx
        [({~literal tone} freq) 

         (define-values (start* end*) (syntax-parse (context-ref (get-id-ctxt stx) #'interval) 
           [({~literal interval} ({~literal start} val:number) ({~literal end} val2:number)) (values (syntax-e #'val) (syntax-e #'val2))]))
         ;; FIXME jagen THIS ASSUMES UNIFORM TEMPO
         (cons #`(let ([duration (get-duration #,start* #,end* 60)]) 
             (cons (round (* #,start* (default-sample-rate)))
                   (rs-scale 2 (rs-mult (sine-window duration (floor (/ duration 4))) (make-tone freq 0.1 duration))))) acc)]
        [_ acc]))))
      
(define preset-memo (make-hash))
(define (load-preset/memo name)
  (if (hash-has-key? preset-memo name)
      (hash-ref preset-memo name)
      (let ([result (load-preset fluid name)])
        (hash-set! preset-memo name result)
        result)))

(define def (default-sample-rate))

(define-subrealizer midi-subrealizer
  (λ(ctxt)
    (println "running midi subrealizer")
    (for/foldr ([acc '()])
               ([stx ctxt])
      (syntax-parse stx
        [({~literal midi} num:number) 
         (define iv (context-ref (get-id-ctxt stx) #'interval))
         (unless iv (raise-syntax-error 'midi-subrealizer 
           (format "this realizer requires beat intervals for all midis, got: ~s" (syntax->datum (un-@ stx))) stx))
         (define-values (start* end*) (syntax-parse iv
           [({~literal interval} ({~literal start} val:number) ({~literal end} val2:number)) (values (syntax-e #'val) (syntax-e #'val2))]))
         (define instrument (context-ref/surrounding ctxt (get-id-ctxt stx) #'instrument))
         (unless instrument (raise-syntax-error 'midi-subrealizer "no instrument in context for midi" stx))
         (syntax-parse instrument
           [({~literal instrument} name:id)
            (cons #`(let ([duration (get-duration #,start* #,end* 60)]) 
                      (cons (round (* #,start* def))
                        (preset-midi->rsound (load-preset/memo (symbol->string (syntax->datum #'name))) 
                                             (syntax-e #'num) duration))) 
              acc)])]
        [_ acc]))))

(define-composite-pstream-realizer music-pstream-realizer {tone-subrealizer midi-subrealizer})
(define-composite-rsound-realizer music-rsound-realizer {tone-subrealizer midi-subrealizer})
