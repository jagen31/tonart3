#lang racket

(require (except-in art bitmap) art/timeline art/sequence art/coordinate/subset 2htdp/image
         (for-syntax syntax/parse racket/math racket/list racket/match racket/math tonart/liszt racket/set syntax/id-set racket/format))
(provide (all-defined-out) (for-syntax (all-defined-out)))

;; define-coordinate interval (copy-coordinate std:interval) or something
;; define-coordinate voice (copy-coordinate std:subset) or something

(define-art-object (tempo [bpm]))
(define-art-object (instrument [name]))

(define-art-rewriter musi@
  (λ (stx)
    (syntax-parse stx
      [(_ [start*:number end*:number (voice:id ...)] expr ...)
       (qq-art stx (@ [(interval [start* end*])] (voice@ (voice ...)) expr ...))])))

;; FIXME jagen SOMEWHAT SLOW (also somewhat broken)
(define-art-rewriter apply-tempo
  (λ (stx)
    (define-values (tempos* rest-ctxt)
      (partition (λ (x) (and (free-identifier=? (car (syntax->list x)) #'tempo)
                             (context-within? (get-id-ctxt x) (get-id-ctxt stx) (current-ctxt))))
                 (current-ctxt)))
    ;; This sort is because the previous tempos affect the later tempos.  we can avoid this
    ;; by beginning with the later tempos.
    (define tempos 
      (sort tempos* > 
        #:key (λ (x) (syntax-parse (context-ref (get-id-ctxt x) #'interval) 
                       [(_ (_ start:number) _) (syntax-e #'start)]))))
    (define new-ctxt 
    (for/fold ([acc rest-ctxt]) 
              ([tem tempos])
      (define iv (context-ref (get-id-ctxt tem) #'interval))
      ;; FIXME jagen yuck
      (syntax-parse #`(#,tem #,(or iv #'(interval [0 +inf.0])))
        [((_ tem*:number) (_ [s e]))
         (define tem** (syntax-e #'tem*))
         (define s* (syntax-e #'s))
         (define e* (syntax-e #'e))
         (for/list ([expr acc])
           (define iv* (context-ref (get-id-ctxt expr) #'interval))
           (cond [iv*
                  (syntax-parse iv*
                    [(_ [expr-s expr-e])
                     (match-define (list s** e**) 
                       (normalize-tempo-interval (list s* e*) tem** 
                         (list (syntax-e #'expr-s) (syntax-e #'expr-e))))
                     (put-in-id-ctxt expr #`(interval [#,s** #,e**]))])]
                 [else expr]))])))
    #`(@ () #,@(map delete-expr tempos) #,@(map delete-expr rest-ctxt) (context #,@new-ctxt))))

(define-subset-coordinate voice voice@)

(define-art-embedding (music [items])
  (λ (stx ctxt)
    (syntax-parse stx
      [(head:id expr ...)
       (rewrite (quasisyntax/loc stx (@ () expr ...)))])))

(define-for-syntax (do-draw-music-voice ctxt* each-height)

  (define max-end
    (for/fold ([acc 1]) ([e ctxt*])
      (define end (expr-interval-end e))
      (if (infinite? end) acc (max acc end))))

  (define each-width (/ (drawer-width) max-end))
  (define line
    (for/fold ([im #'empty-image])
              ([i (in-range max-end 2)])
      #`(beside #,im 
          (overlay (text #,(~s (add1 i)) 16 'black) 
                   (rectangle #,(* each-width 2) 10 'solid 'transparent)))))


    (for/fold ([im #`(rectangle #,(drawer-width) #,(drawer-height) 'solid 'tan)])
              ([e ctxt*])
        (match-define (cons start end) (expr-interval e))

        (define end* (if (infinite? end) max-end end))
        (define x-start (* start each-width))
        (define x-end (* end* each-width))
        (define width* (- x-end x-start))

        (define sub-pic 
          (parameterize ([drawer-width width*] [drawer-height each-height]) 
            (drawer-recur e)))

        #`(overlay/xy
            (add-line (add-line (add-line
              #,sub-pic 
              0 10 #,width* 10 'purple)
              0 0 0 20 'purple)
              #,width* 0 #,width* 20 'purple)
              #,(- x-start) #,(- (/ (drawer-height) 2)) #,im)))
   
(define-for-syntax (do-draw-music ctxt)
  ;; FIXME jagen library fn

  (define voices (voice-find-all ctxt))
  (define each-height (/ (drawer-height) (add1 (set-count voices))))
  (define no-voice (filter (λ (expr) (null? (expr-voice expr))) ctxt))

  #`(above/align 'left
      empty-image
      #,(if (empty? no-voice)
        #'empty-image
        #`(overlay/align 'left 'top 
            (text "<no-voice>" 16 'black) 
            #,(do-draw-music-voice no-voice each-height)))
      #,@(for/list ([voice-index (in-naturals)] [v voices])
           (define ctxt* (filter (λ (expr) (context-within? (get-id-ctxt expr) (list #`(voice #,v)) ctxt)) ctxt))
           #`(overlay/align 'left 'top
               (text #,(format "~a:" (symbol->string (syntax->datum v))) 16 'black)
               #,(do-draw-music-voice ctxt* each-height)))))
  


(define-drawer draw-music 
  (λ (stx)
    (syntax-parse stx
      [(_ expr ...) (do-draw-music (syntax->list #'(expr ...)))])))

(register-drawer! music draw-music)

(define-mapping-rewriter (rewrite-in-music [(: s music)])
  (λ (stx s)
    (syntax-parse stx
      [(_ expr ...)
       (syntax-parse s
         [(_ expr* ...)
           #:with (result ...) 
             (rewrite-in (syntax->list #'(expr* ...)) #'(context expr ...))
           #`(context #,(qq-art s (music result ...)))])])))

(define-for-syntax (music-start stx)
  (syntax-parse stx
    [({~literal music} expr ...)
     (min (map expr-interval-start (syntax->list #'(expr ...))))]))

(define-for-syntax (music-end stx)
  (syntax-parse stx
    [({~literal music} expr ...)
     #:do [(define exprs (syntax->list #'(expr ...)))]
     (if (null? exprs)
         0
         (apply max (map (compose (λ (e) (if (infinite? e) 0 e)) expr-interval-end) (syntax->list #'(expr ...)))))]))

(define-mapping-rewriter (inline-music-seq [(: s seq)])
  (λ (stx s)
    (define init-start (expr-interval-start s))
    (define init-end (expr-interval-end s))
    (syntax-parse s 
      [(_ expr ...)
        #:do [(define sorted (sort (syntax->list #'(expr ...)) < #:key expr-single-index))]
        #:with (result ...) 
          (for/list ([e sorted]) 
            (syntax-parse e
              [({~literal music} music-expr ...)
               #:with end (music-end e)
               #`[end #,@(for/list ([e (syntax->list #'(music-expr ...))]) (remove-from-id-ctxt e #'index))]]))
        #`(@ () #,(delete-expr s) #,(qq-art s (-- #,init-start result ...)))])))
