#lang at-exp racket

(require 
  art art/timeline art/coordinate/instant art/coordinate/switch 
  tonart/private/lib tonart/private/electronic/lib 
  racket/runtime-path rsound rsound/envelope sf2-parser
  (for-syntax syntax/parse racket/match racket/list racket/string racket/dict racket/string))
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
         (list "SawOsc _osc1 => dac;")
         (list "SawOsc _osc2 => dac;")
         (list "SawOsc _osc3 => dac;")
         (list "SawOsc _osc4 => dac;")
         (list "SawOsc _osc5 => dac;")
         (list "SawOsc _osc6 => dac;")
         (list "SawOsc _osc7 => dac;")
         (list "SawOsc _osc8 => dac;")
         (list "0 => _osc1.gain;")
         (list "0 => _osc2.gain;")
         (list "0 => _osc3.gain;")
         (list "0 => _osc4.gain;")
         (list "0 => _osc5.gain;")
         (list "0 => _osc6.gain;")
         (list "0 => _osc7.gain;")
         (list "0 => _osc8.gain;")
         (for/foldr ([acc '()])
                    ([(name path) (in-dict smap)])
           (append 
             (list (format "SndBuf ~a => dac;" name)
                   (format "\"~a\" => ~a.read;" path name)
                   (format "~a.samples() => ~a.pos;" name name))
             acc))))

     (define ctxt-
       (sort ctxt* < 
         #:key (λ (stx) 
           (define swi (context-ref (get-id-ctxt stx) #'switch))
           (if swi
             (syntax-parse swi [(_ result) (if (syntax-e #'result) 0 1)])
             1))))
     (define ctxt
       (sort ctxt- < 
         #:key (λ (stx) 
           (define inst (context-ref (get-id-ctxt stx) #'instant))
           (if inst
             (syntax-parse inst [(_ result) (syntax-e #'result)])
             0))))

       ;; TODO jagen31 YUGE hack
       (define get-count
         (let ([count 0] ) 
           (define (get-count) 
             (begin0 count (set! count (modulo (add1 count) 8)))) get-count))
       (define event-map (make-hash))
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
             [({~literal full-tone} freq:number vol:number)
              (define instant (context-ref (get-id-ctxt stx) #'instant))
              (define switch (context-ref (get-id-ctxt stx) #'switch))
              (define art-id (context-ref (get-id-ctxt stx) #'art-id))
              (unless instant (raise-syntax-error 'midi-subrealizer (format "this realizer requires an instant for all midis, got: ~s" (un-@ stx)) stx))
              (unless switch (raise-syntax-error 'midi-subrealizer (format "this realizer requires a switch for all midis, got: ~s" (un-@ stx)) stx))
              (syntax-parse #`(#,instant #,switch)
                [((_ time) (_ on?))
                 (define t* (syntax-e #'time))
                 (define skip (if (= t* t) '() (list (format "~a::ms => now;" (inexact->exact (round (* 1000 (- t* t))))))))
                 (define count (if (syntax-e #'on?) 
                   (let () (define the-count (get-count)) (hash-set! event-map (syntax-e art-id) the-count) the-count)
                   (hash-ref event-map (syntax-e art-id))))
                 (define freq-var (format "_osc~a.freq" (add1 count)))
                 (define gain-var (format "_osc~a.gain" (add1 count)))
                 (values (append 
                           (if (syntax-e #'on?) 
                               (list (format "~a => ~a;" (exact->inexact (/ (syntax-e #'vol) 10)) gain-var) (format "~a => ~a;" (syntax-e #'freq) freq-var))
                               (list (format "0 => ~a;" gain-var)))
                           skip acc)
                         t*)])]
             [_ (values acc t)])))

      #`#,(string-join (append header body) "\n")])))


(define-art-embedding (send [items])
  (λ (stx ctxt)
    (syntax-parse stx
      [(head:id expr ...)
       (rewrite (quasisyntax/loc stx (context expr ...)))])))

(define-art-object (advance-time [time]))


(define (do-send-chuck ck-output chuck-string)
  (displayln (format "{ Go.queueMe();\n~a }" chuck-string) ck-output)
  (flush-output ck-output))

(define (do-advance-time-chuck ck-output t)
  (displayln (format "{ Go.advanceTime(~s::second); }" t) ck-output)
  (flush-output ck-output))

(define-syntax (start-chucker stx)
  (syntax-parse stx
    [(_ name:id)
     #'(begin
         (match-define (list ck-input ck-output p ck-error _) (process "chuck --shell"))
         (println p)
         #;(thread 
           (λ ()
             (let loop () 
               (displayln (read-line ck-input))
               (loop))))
         #;(thread 
           (λ ()
             (let loop () 
               (displayln (read-line ck-error))
               (loop))))
         (displayln "+ chuck-backend/play_queued.ck" ck-output)
         (flush-output ck-output)
         (displayln "+ chuck-backend/go.ck" ck-output)
         (flush-output ck-output)
         (displayln "+ chuck-backend/rec.ck" ck-output)
         (flush-output ck-output)
         (define-art-realizer my-secret-realizer
           (λ (stx)
             (define/syntax-parse (expr (... ...))
               (for/list ([e (current-ctxt)])
                 (syntax-parse e
                   [({~literal send} expr (... ...))
                    (println "Generating SEND")
                    #'(begin 
                        (println "Sending")
                        (println (realize (music-chuck-realizer) expr (... ...)))
                        (do-send-chuck ck-output (realize (music-chuck-realizer) expr (... ...))))]
                   [({~literal advance-time} t:number) 
                    (println "generating advance time")
                    #'(begin
                        (println "Advancing Time")
                        (do-advance-time-chuck ck-output t))])))
             #'(begin expr (... ...))))
         (define-syntax (name stx)
           (syntax-parse stx
             [(_ expr (... ...)) #'(realize (my-secret-realizer) expr (... ...))])))]))
