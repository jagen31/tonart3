#lang racket

(require art art/sequence tonart/private/lib 
  (for-syntax syntax/parse racket/match racket/list racket/dict) rsound rsound/envelope sf2-parser)
(provide (all-defined-out))

;;;;;;; TONES - these are pretty easy to have a computer perform.
(define-art-object (tone [freq]))
;; context free
(define-art-object (full-tone [freq volume]))

(define-art-object (volume [n]))

(define-art-rewriter tones
  (λ (stx)
    (syntax-parse stx [(_ the-tone:number ...) (qq-art stx (ix-- (tone the-tone) ...))])))

(define-mapping-rewriter (tone->full-tone [(: t tone)])
  (λ (stx t)
    (syntax-parse t
      [(_ freq)
       #:with (_ vol) (require-context (lookup-ctxt) t #'volume)
       (qq-art t (full-tone freq vol))])))

;; MIDI- an alternative to sine waves
(define-art-object (midi [num]))
(define-art-object (instrument-map [instruments]))
;; context-free
(define-art-object (full-midi [num velocity instruments]))

(define-mapping-rewriter (midi->full-midi [(: m midi)])
  (λ (stx m)
    (syntax-parse m
      [(_ note)
       #:do [(define insts* (context-ref*/surrounding (current-ctxt) (get-id-ctxt m) #'instrument))
             (when (null? insts*) (raise-syntax-error 'midi-full-midi "no instrument in context for midi." m))]
       #:with [(_ inst-name) ...] insts* 
       (qq-art m (full-midi note 80 [inst-name ...]))])))

(define-art-object (sound-map [dict]))
(define-art-object (sound [name]))

(define-art-rewriter merge-sound-maps
  (λ (stx)
    (syntax-parse stx
      [_
       (define maps (context-ref*/within (current-ctxt) (get-id-ctxt stx) #'sound-map))

       ;; stmts are the statements to run to both delete the old and insert the renamed sounds
       (define-values (stmts map*)
       (for/fold ([stmts '()] [map* '()] #:result (values (reverse stmts) map*))
                 ([m maps])
         (define m* (syntax-parse m [(_ m ...) (syntax->datum #'(m ...))]))
         (for/fold ([stmts stmts] [map* map*])
                   ([(name path) (in-dict m*)])
           ;; rename until it is a new name (by adding ones to the end :P)
           ;; FIXME crappy
           (define (rename name)
             (if (dict-has-key? map* name)
               (rename (string->symbol (format "~s1" name)))
               (values name (dict-set map* name path))))
           (define-values (name* map**) (rename name))

           ;; rename all affected sounds, if the name has changed
           (if (not (equal? name* name))
             (values
               (for/fold ([acc stmts])
                         ([the-sound (context-ref*/within (current-ctxt) (get-id-ctxt m) #'sound)])
                 (syntax-parse the-sound
                  [(_ name**:id) 
                   (if (equal? (syntax->datum #'name**) name)
                     (cons (qq-art the-sound (sound #,name*)) (cons (delete-expr the-sound) acc))
                     acc)]))
               map**)
             (values stmts map**)))))
       #`(context #,@(map delete-expr maps) #,@stmts 
           #,(qq-art stx (sound-map #,@(for/list ([(k v) (in-dict map*)]) #`[#,k . #,v]))))])))