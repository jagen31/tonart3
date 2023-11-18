#lang racket
(require art tonart/rewriter/common-practice/main
             tonart/realizer/electronic/rsound/main
         (for-syntax racket syntax/parse data/gvector))

(define-for-syntax the-exprs (gvector))
(define the-sound (box (silence 1)))
(define the-loop-length (box 8))
(void (set-output-device! 1))
(define looping (box #f))

(define (start-loop)
  (thread (λ ()
    (println "starting loop...")
    (let loop ()
      #;(set-output-device! (unbox the-device))
      (with-handlers ([(λ(x) #t) (λ(x) (print x))]) (play (unbox the-sound)))
      (sleep (unbox the-loop-length))
      (when looping (loop))))))

;; FIXME jagen31 define this here to declare the intent to optimize this in the future with
;; some static analysis/annotations on the performer and the composition
(define-for-syntax (needs-recompile) #t)

(define-for-syntax (get-recompile-expr)
  (if (needs-recompile)
      #`(begin (println "recompiling!") (set-box! the-sound (perform music-rsound-performer #,@(gvector->list the-exprs))))
      #`(void)))

(define-syntax (#%top-interaction stx)
  (syntax-parse stx

    ;; playing
    [(_ . ({~literal play})) #'(play (unbox the-sound))]
    [(_ . ({~literal start-loop})) #'(begin (set-box! looping #t) (start-loop))]
    [(_ . ({~literal stop-loop})) #'(set-box! looping #f)]
    [(_ . ({~literal set-loop-length} n:number)) #'(set-box! the-loop-length n)]

    ;; write sound/source
    [(_ . ({~literal save} name:string))
     (define file (open-output-file (syntax-e #'name)))
     (for ([expr (in-gvector the-exprs)]) (writeln (syntax->datum expr) file))
     (close-output-port file)
     #'(void)]
    [(_ . ({~literal save-sound} name:string))
     #'(rs-write (unbox the-sound) name)]
    [(_ . ({~literal set-audio-device} dev:number))
     #'(set-output-device! dev)]

    ;; crud
    [(_ . ({~literal add} instr ...))
     ;; add the instruction to the instructions
     (apply gvector-add! the-exprs (syntax->list #'(instr ...)))
     (get-recompile-expr)]
    [(_ . ({~literal remove} ix:number))
     (gvector-remove! the-exprs (syntax-e #'ix))
     (get-recompile-expr)]
    [(_ . ({~literal replace} ix:number expr))
     (gvector-set! the-exprs (syntax-e #'ix) #'expr)
     (get-recompile-expr)]
    [(_ . ({~literal insert} ix:number expr))
     (gvector-insert! the-exprs (syntax-e #'ix) #'expr)
     (get-recompile-expr)]

    ;; show the lines, or the compiled output
    [(_ . ({~literal show}))
     (for ([expr (gvector->list the-exprs)]
           [i (in-naturals)])
       (displayln (format "~a: ~a" i (syntax->datum expr))))
     #'(void)]
    [(_ . ({~literal show-compiled}))
     #`(perform quote-performer #,@(gvector->list the-exprs))]
    [(_ . ({~literal racket} expr))
     #'expr]))
