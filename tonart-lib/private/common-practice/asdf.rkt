#lang racket

(require tonart (for-syntax syntax/parse racket/dict))

(define-art-object (subdivisions []))
(define-art-object (set-labels []))
(define-art-object (tuning []))
(define-art-object (note [p a o]))
(define-art-object (semitone [n o]))

(define-art ionian
  (subdivisions 12)
  (set-labels [c 0] [d 2] [e 4] [f 5] [g 7] [a 9] [b 11]))


(define-mapping-rewriter (note->semitone [(: n note)])
  (λ (stx n)
    
    (define/syntax-parse (_ mapping- ...) (require-context (lookup-ctxt) n #'pitch-set-labels))
    (define mapping (syntax->datum #'(mapping- ...)))

    (syntax-parse n
      [(note p a o)
       (define ix- (dict-ref mapping (syntax-e #'p)))
       (define/syntax-parse ix (+ ix- (syntax-e #'a)))
       
       (qq-art n (semitone ix o))])))

(define-mapping-rewriter (semitone->tone [(: st semitone)])
  (λ (stx st)
    
    (define/syntax-parse (_ tuning- ...) (require-context (lookup-ctxt) st #'tuning))
    (define tuning (list->vector (syntax->list #'(tuning- ...))))

    (syntax-parse st
      [(semitone ix octave)
       (define freq- (dict-ref tuning (syntax-e #'ix)))
       (define/syntax-parse freq (* freq- (expt 2 (- (syntax-e #'octave) 4))))
       
       (qq-art st (tone freq))])))

(realize (staff-realizer [800 200] [(soprano treble)])
  ionian
  (i@ [0 4] (voice@ (soprano) (note a 0 4)))
  (tuning 261.626 277.183 293.665 311.127 329.628 349.228
          369.994 391.995 415.305 440.000 466.164 493.883))