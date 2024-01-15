#lang racket

(require art art/sequence tonart/private/lib 
  (for-syntax syntax/parse racket/match racket/list) rsound rsound/envelope sf2-parser)
(provide (all-defined-out))

;;;;;;; TONES - these are pretty easy to have a computer perform.
(define-art-object (tone [freq]))

(define-art-object (volume [n]))

(define-art-rewriter tones
  (λ (stx)
    (syntax-parse stx [(_ the-tone:number ...) (qq-art stx (ix-- (tone the-tone) ...))])))

;; MIDI- an alternative to sine waves
(define-art-object (midi [num]))
(define-art-object (instrument-map [instruments]))
(define-art-object (full-midi [num velocity instrument]))

(define-mapping-rewriter (midi->full-midi [(: m midi)])
  (λ (stx m)
    (syntax-parse m
      [(_ note)
       #:do [(define insts* (context-ref*/surrounding (current-ctxt) (get-id-ctxt m) #'instrument))
             (when (null? insts*) (raise-syntax-error 'midi-full-midi "no instrument in context for midi." m))]
       #:with [(_ inst-name) ...] insts* 
       (qq-art m (full-midi note 80 [inst-name ...]))])))
