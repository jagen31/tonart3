#lang racket
(require art tonart/rewriter/common-practice/main
             tonart/realizer/electronic/rsound/main
         (for-syntax racket syntax/parse data/gvector))

(define-for-syntax the-exprs (gvector))
(define-for-syntax the-post-exprs (gvector))
(define the-sound (box (silence 1)))
(define the-length (box 8))
(define the-device (box 2))

(thread (λ()
  (let loop ()
    (set-output-device! (unbox the-device))
    (with-handlers ([(λ(x) #t) (λ(x) (print x))]) (play (unbox the-sound)))
    (sleep (unbox the-length))
    (loop))))

(define-for-syntax (get-recompile-expr)
  #`(set-box! the-sound (perform music-rsound-performer #,@(gvector->list the-exprs) #,@(gvector->list the-post-exprs))))

(define-syntax (#%top-interaction stx)
  (syntax-parse stx
    [(_ . ({~literal set-length} n:number))
     #'(set-box! the-length n)]
    [(_ . ({~literal export-sound} name:string))
     #'(rs-write (unbox the-sound) name)]
    [(_ . ({~literal export-source} name:string))
     (define file (open-output-file (syntax-e #'name)))
     (for ([expr (in-gvector the-exprs)]) (writeln (syntax->datum expr) file))
     (for ([expr (in-gvector the-post-exprs)]) (writeln (syntax->datum expr) file))
     (close-output-port file)]
    [(_ . ({~literal set-audio-device} dev:number))
     #'(set-box! the-device dev)]
    [(_ . ({~literal add} instr ...))
     ;; add the instruction to the instructions
     (apply gvector-add! the-exprs (syntax->list #'(instr ...)))
     ;; perform the instructions and set to the sound
     #`(set-box! the-sound (perform music-rsound-performer 
         #,@(gvector->list the-exprs) #,@(gvector->list the-post-exprs)))]
    [(_ . ({~literal post} instr ...))
     ;; add the instruction to the post instructions
     (apply gvector-add! the-post-exprs (syntax->list #'(instr ...)))
     ;; perform the instructions and set to the sound
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
    [(_ . ({~literal show}))
     (for ([expr (append (gvector->list the-exprs) (gvector->list the-post-exprs))]
           [i (in-naturals)])
       (displayln (format "~a: ~a" i (syntax->datum expr))))
     #'(void)]
    [(_ . ({~literal show} {~literal compiled}))
     #`(perform quote-performer #,@(gvector->list the-exprs) #,@(gvector->list the-post-exprs))]))
