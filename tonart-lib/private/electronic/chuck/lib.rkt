#lang at-exp racket

(require 
  art art/timeline art/coordinate/instant art/coordinate/switch 
  tonart/private/lib tonart/private/electronic/lib 
  racket/runtime-path rsound rsound/envelope sf2-parser
  (for-syntax syntax/parse racket/match racket/list racket/string racket/dict))
(provide (all-defined-out))

;; create a ChucK string containing a program which plays the score
(define-art-realizer music-chuck-realizer
  (λ (stx)
  (syntax-parse stx
    [_

     (define ctxt* (current-ctxt))

     (define smap* (context-ref (lookup-ctxt) #'sound-map))
     (unless smap* (raise-syntax-error 'midi-subrealizer "no sound map in context"))
     (define smap (syntax-parse smap* [(_ map ...) (syntax->datum #'(map ...))]))

     (define header
       (append
         (list "SinOsc _osc => dac;")
         (list "0.5 => _osc.gain;")
         (list "0 => _osc.freq;")
         (for/foldr ([acc '()])
                    ([(name path) (in-dict smap)])
           (append 
             (list (format "SndBuf ~a => dac;" name)
                   (format "\"~a\" => ~a.read;" path name)
                   (format "~a.samples() => ~a.pos;" name name))
             acc))))

     (define ctxt (sort ctxt* < 
       #:key (λ (stx) 
         (define inst (context-ref (get-id-ctxt stx) #'instant))
         (if inst
           (syntax-parse inst [(_ result) (syntax-e #'result)])
           0))))

     (define body
       (for/fold ([acc '()] [t 0] #:result (reverse acc))
                 ([stx ctxt])
         (syntax-parse stx
           [({~literal sound} name:id) 
            (define instant (context-ref (get-id-ctxt stx) #'instant))
            (define switch (context-ref (get-id-ctxt stx) #'switch))
            (unless instant (raise-syntax-error 'midi-subrealizer (format "this realizer requires an instant for all midis, got: ~s" (un-@ stx)) stx))
            (unless switch (raise-syntax-error 'midi-subrealizer (format "this realizer requires a switch for all midis, got: ~s" (un-@ stx)) stx))
            (syntax-parse #`(#,instant #,switch)
              [((_ time) (_ on?))
               (define t* (syntax-e #'time))
               (define skip (if (= t* t) '() (list (format "~a::ms => now;" (inexact->exact (round (* 1000 (- t* t))))))))
               (values (cons (if (syntax-e #'on?) (format "0 => ~a.pos;" (syntax-e #'name)) "<<< \"off\" >>>;") (append skip acc)) t*)])]
           [({~literal tone} freq:number)
            (define instant (context-ref (get-id-ctxt stx) #'instant))
            (define switch (context-ref (get-id-ctxt stx) #'switch))
            (unless instant (raise-syntax-error 'midi-subrealizer (format "this realizer requires an instant for all midis, got: ~s" (un-@ stx)) stx))
            (unless switch (raise-syntax-error 'midi-subrealizer (format "this realizer requires a switch for all midis, got: ~s" (un-@ stx)) stx))
            (syntax-parse #`(#,instant #,switch)
              [((_ time) (_ on?))
               (define t* (syntax-e #'time))
               (define skip (if (= t* t) '() (list (format "~a::ms => now;" (inexact->exact (round (* 1000 (- t* t))))))))
               (values (cons (if (syntax-e #'on?) (format "~a => _osc.freq;" (syntax-e #'freq)) "0 => _osc.freq;") (append skip acc)) t*)])]
           [_ (values acc t)])))

      #`#,(string-join (append header body) "\n")])))



(define-art-embedding (send [items])
  (λ (stx ctxt)
    (syntax-parse stx
      [(head:id expr ...)
       (rewrite (quasisyntax/loc stx (context expr ...)))])))

(define-art-object (advance-time [time]))
