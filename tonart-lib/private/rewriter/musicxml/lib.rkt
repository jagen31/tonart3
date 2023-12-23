#lang at-exp racket

(require art art/sequence art/timeline art/sequence/ravel tonart/private/rewriter/lib tonart/common-practice tonart/rsound xml 2htdp/image 2htdp/universe (prefix-in cute: 2htdp/planetcute)
  (for-syntax racket/string syntax/parse racket/list racket/match racket/dict racket/set syntax/to-string fmt racket/math
              (except-in xml attribute) "mxml.rkt" syntax/id-table syntax/id-set))
  
(define-coordinate (duration [d]))
;; FIXME jagen cruft- this should be default
(define-hom-merge-rule duration (λ (l r _ __ ___) (or r l)))
(define-hom-within?-rule duration (λ (l r _ __ ___) #t))

;; should this even me a coord or just an object that goes in id ctxt
(define-coordinate (tie [start]))
(define-hom-merge-rule tie (λ (l r _ __ ___) (or r l)))
(define-hom-within?-rule tie (λ (l r _ __ ___) #t))

(define-art-object (xml []))

(define-art-embedding (measure [items])
  (λ (stx ctxt)
    (syntax-parse stx
      [(head:id expr ...)
       (rewrite (quasisyntax/loc stx (|@| () expr ...)))])))
(define-art-embedding (mxml-measure [items])
  (λ (stx ctxt)
    (syntax-parse stx
      [(head:id expr ...)
       (rewrite (quasisyntax/loc stx (|@| () expr ...)))])))

(define-art-object (mxml-note [p a o]))
(define-art-object (mxml-rest []))


(define-art-rewriter enclose-in-measures
  (λ (stx)
    
    (define objs-to-enclose (syntax-parse stx [(_ [objs:id ...]) (immutable-free-id-set (syntax->list #'(objs ...)))]))
    (define voices (voice-find-all (current-ctxt)))


    (define ctxt*-
      (filter 
        (λ (e) (syntax-parse e [(head:id _ ...) (free-id-set-member? objs-to-enclose #'head)]))
        (current-ctxt)))
    
    ;; make end the same for all voices so we get the same number of measures
    (define end (apply max (cons 0 (map (λ (e) (or (expr-interval-end e) 0)) ctxt*-))))

    (define voice-results 
      (for/list ([v (in-set voices)])

          (define ctxt*
            (filter (λ (e) (context-within? (get-id-ctxt e) (list #`(voice #,v)) (current-ctxt)))
              ctxt*-))

        (define time-sigs 
          (context-ref*/within (current-ctxt) 
            (get-id-ctxt (put-in-id-ctxt (ensure-id-ctxt stx) #`(voice #,v))) #'time-sig))

        (for/fold ([acc '()]) 
                  ([time time-sigs])

          (define/syntax-parse (_ num*:number denom*:number) time)
          (define/syntax-parse (_ (_ start*:number) (_ end*:number)) 
            (or (context-ref (get-id-ctxt time) #'interval) #'(interval (start 0) (end +inf.0))))
          (match-define (list start end- num denom) (map syntax-e (list #'start* #'end* #'num* #'denom*)))

          (define result
            ;; FIXME improve runtime
            (for/list ([s (in-range start end num)] [e (in-range (+ start num) (+ end num) num)])
              (define objs- (context-ref*/interval-intersect ctxt* (list #`(interval (start #,s) (end #,e)))))
              (define objs 
                (for/list ([obj objs-]) 
                  (define iv (car obj))
                  (define iv* (interval-syntax->datum (context-ref (get-id-ctxt (cdr obj)) #'interval)))
                  (define result (put-in-id-ctxt (cdr obj) #`(interval (start #,(car iv)) (end #,(cdr iv)))))
                  (cond
                    [(equal? iv iv*) result]
                    [(= (car iv) (car iv*)) (put-in-id-ctxt result #'(tie start))]
                    [(= (cdr iv) (cdr iv*)) (put-in-id-ctxt result #'(tie stop))]
                    [else (println obj) (println iv*) (error 'enclose-in-measures "not sure yet")])))
              #`(measure #,@objs)))

        #`(voice@ (#,v) #,@(map delete-expr ctxt*) (seq (ix-- #,@result))))))
    #`(context #,@voice-results)))

(define-art-rewriter insert-rests
  (λ (stx)
    (define measures (context-ref*/within (current-ctxt) (get-id-ctxt stx) #'measure))
    (define result 
      (for/list ([i (in-naturals)] [m measures])
        (syntax-parse m
          [(_ expr ...)
           (define/syntax-parse (_ num _) (require-context (current-ctxt) m #'time-sig))
           (define sorted-exprs (sort (syntax->list #'(expr ...)) < #:key expr-interval-start))
           (qq-art m 
             (measure
               #,@(for/fold ([acc '()] #:result (reverse acc))
                            ([e sorted-exprs] 
                             [e2 (cdr (append sorted-exprs
                                         (list (put-in-id-ctxt (ensure-id-ctxt #'(music-rest))
                                                 #`(interval (start #,(* (syntax-e #'num) (add1 i))) (end +inf.0))))))])
                    (define t (expr-interval-end e))
                    (define t* (expr-interval-start e2))
                    (if (= t t*)
                      (cons e acc)
                      (cons e (cons (put-in-id-ctxt (ensure-id-ctxt #'(music-rest)) #`(interval (start #,t) (end #,t*))) acc))))))])))
    #`(context #,@(map delete-expr measures) #,@result)))

(define-mapping-rewriter (note->mxml-note [(: n note)])
  (λ (stx n)
    (println "here")
    (syntax-parse n
      [(_ p a o)
       #:with (_ _ denom) (require-context (current-ctxt) n #'time-sig)
       #:do [
        (println "interval")
        (define the-dur- (- (expr-interval-end n) (expr-interval-start n)))
        (define the-dur (/ the-dur- (syntax-e #'denom)))]
       #:with (dur dot ...)
         (match (inexact->exact the-dur)
           [1/16 #'(sixteenth)]
           [1/8 #'(eighth)] [3/16 #'(eighth dot)]
           [1/4 #'(quarter)] [3/8 #'(quarter dot)]
           [1/2 #'(half)] [3/4 #'(half dot)]
           [1 #'(whole)])
       (qq-art n (|@| [(duration #,the-dur-)] (mxml-note p a o dur dot ...)))])))

(define-mapping-rewriter (rest->mxml-rest [(: r music-rest)])
  (λ (stx r)
    (syntax-parse r
      [(_)
       #:with (_ _ denom) (require-context (current-ctxt) r #'time-sig)
       #:do [
        (define the-dur- (- (expr-interval-end r) (expr-interval-start r)))
        (define the-dur (/ the-dur- (syntax-e #'denom)))]
       #:with (dur dot ...)
         (match (inexact->exact the-dur)
           [1/16 #'(sixteenth)]
           [1/8 #'(eighth)] [3/16 #'(eighth dot)]
           [1/4 #'(quarter)] [3/8 #'(quarter dot)]
           [1/2 #'(half)] [3/4 #'(half dot)]
           [1 #'(whole)])
       (qq-art r (|@| [(duration #,the-dur-)] (mxml-rest dur dot ...)))])))
       

(define-mapping-rewriter (measure->mxml-measure [(: m measure)])
  (λ (stx m)
  (println "HERE")
    (syntax-parse m
      [(_ expr ...)
       (define sig (require-context (current-ctxt) m #'time-sig))
       (define result(qq-art m (mxml-measure 
         (seq (ix-- 
           #,@(run-art-exprs 
            (list #'(note->mxml-note) #'(rest->mxml-rest) (delete-expr sig)) 
            (cons sig (syntax->list #'(expr ...)))))))))
        (println "DONE") result])))

(define-art-rewriter load-musicxml
  (λ (stx)
    (syntax-parse stx
      [(_ file:string [voice ...])
       #:do [
       (define it (syntax:read-xml (open-input-file (syntax-e #'file))))
       (define part-descriptions (xml-path it part-list score-part))
       (define part-hash
         (for/hash ([part part-descriptions] [voice (syntax->list #'(voice ...))])
           (define the-part (syntax-e (xml-attr part id)))
           (values the-part voice)))
       (define parts (xml-path it part))]
       #:with (result ...) 
         (for/list ([part parts])
           (define measures 
             (for/list ([measure (xml-path part measure)])
               (define notes
                 (for/list ([note (xml-path measure note)])
                   (match-define (list p a o dur g)
                     (map (compose syntax-e (λ (x) (if (not (null? x)) (car x) #'#f)))
                       (list (xml-path note pitch step) (xml-path note pitch alter) (xml-path note pitch octave) 
                             (xml-path note duration) (xml-path note type) )))
                   (define t- (xml-path note tie))
                   (define t
                     (cond
                       [(null? t-) '()]
                       [(= (length t-) 2) (list #'(tie continue))]
                       [else (list #`(tie #,(string->symbol (syntax-e (xml-attr (car t-) type)))))]))
                   (quasisyntax/loc note 
                     (|@| [(duration #,(string->number (syntax-e (xml-value dur)))) #,@t]
                       (mxml-note 
                         #,(string->symbol (string-downcase (syntax-e (xml-value p))))
                         #,(string->number (syntax-e (if a (xml-value a) #'"0"))) 
                         #,(string->number (syntax-e (xml-value o)))
                         #,(string->symbol (syntax-e (xml-value g)))
                         #,@(if (null? (xml-path note dot)) '() (list #'dot)))))))
               (define time-sigs 
                 (for/list ([time-sig (xml-path measure attributes time)])
                   (match-define (list n d)
                     (list (xml-value (car (xml-path time-sig beats))) (xml-value (car (xml-path time-sig beat-type)))))
                   (quasisyntax/loc time-sig (time-sig #,(string->number (syntax-e n)) #,(string->number (syntax-e d))))))
               (define divisions
                 (for/list ([division (xml-path measure attributes divisions)])
                   (quasisyntax/loc division (divisions #,(string->number (syntax-e (xml-value division)))))))
             #`(mxml-measure (seq #,@time-sigs #,@divisions (ix-- #,@notes)))))
           #`(voice@ (#,(dict-ref part-hash (syntax-e (xml-attr part id)))) (seq (ix-- #,@measures))))
       #'(|@| () result ...)])))

(define-for-syntax (generate score-parts parts)
  @string-append{
    <?xml version="1.0" encoding="UTF-8"?>
    <!DOCTYPE score-partwise PUBLIC "-//Recordare//DTD MusicXML 4.0 Partwise//EN" "http://www.musicxml.org/dtds/partwise.dtd">
    <score-partwise version="4.0"> 
    <part-list>
      @(string-join score-parts "\n")
    </part-list>
    @(string-join parts "\n")
    </score-partwise>
  })

(define-for-syntax (generate-score-part v)
  @string-append{
    <score-part id="@(symbol->string (syntax-e v))">
      <part-name>@(symbol->string (syntax-e |v|))</part-name>
    </score-part>
  })

(define-for-syntax (generate-part v ms)
  @string-append{
    <part id="@(symbol->string (syntax-e v))">
      @(string-join ms "\n")
    </part>
  })

(define-for-syntax (generate-tie-stmts t)

  ;; Some BS, musicxml stores some things redundantly for philosophy reasons
  (define (tie-abs x ss) 
    (for/list ([s ss]) @string-append{<@x type="@s"/>}))
  (define (tie-abs2 ss) 
    (values
      (string-join (tie-abs "tie" ss) "\n")
      (string-join (tie-abs "tied" ss) "\n")))
    
   (syntax-parse t
    [#f (values "" "")]
    [(_ {~datum start}) (tie-abs2 (list "start"))]
    [(_ {~datum continue}) (tie-abs2 (list "start" "stop"))]
    [(_ {~datum stop}) (tie-abs2 (list "stop"))]))

(define-for-syntax (generate-note n)
  (syntax-parse n
    [({~literal mxml-note} p a o g {~optional {~and the-dot {~datum dot}}})
     #:with (_ dur:number) (require-context (current-ctxt) n #'duration)
     (define tie- (get-context (current-ctxt) n #'tie))
     (define-values (audio-tie visual-tie) (generate-tie-stmts tie-))
      @string-append{
      <note>
        <pitch>
          <step>@(string-upcase (symbol->string (syntax-e #'p)))</step>
          <alter>@(number->string (syntax-e #'a))</alter>
          <octave>@(number->string (syntax-e #'o))</octave>
          </pitch>
        <duration>@(number->string (syntax-e #'dur))</duration>
        @audio-tie
        <voice>1</voice>
        <type>@(symbol->string (syntax-e #'g))</type>
        @(if (attribute the-dot) "<dot/>\n" "")
        <stem>down</stem>
        <notations>
          @visual-tie
        </notations>
        </note>
      }]
      [({~literal mxml-rest} g {~optional {~and the-dot {~datum dot}}})
     #:with (_ dur:number) (require-context (current-ctxt) n #'duration)
      @string-append{
      <note>
        <rest/>
        <duration>@(number->string (syntax-e #'dur))</duration>
        <voice>1</voice>
        <type>@(symbol->string (syntax-e #'g))</type>
        @(if (attribute the-dot) "<dot/>\n" "")
        </note>
      }]))

(define-for-syntax (generate-measure m)
  (syntax-parse m
    [({~literal mxml-measure} ({~literal seq} inner-expr ...))
     #:with (_ ix:number) (require-context (current-ctxt) m #'index)
     (define maybe-div (context-ref (syntax->list #'(inner-expr ...)) #'divisions))
     (define maybe-time-sig (context-ref (syntax->list #'(inner-expr ...)) #'time-sig))
     (define notes 
       (for/list ([n (filter (syntax-parser [(head _ ...) (or (free-identifier=? #'head #'mxml-note) (free-identifier=? #'head #'mxml-rest))]) 
                             (syntax->list #'(inner-expr ...)))]) 
         (generate-note n)))

     (define attributes
       @string-append{
       <attributes>
       @(cond
          [maybe-div
           (define/syntax-parse (_ div) maybe-div)
           @string-append{
             <divisions>@(number->string (syntax-e #'div))</divisions>
           }]
          [else ""])
       @(cond
          [maybe-time-sig
           (define/syntax-parse (_ num denom) maybe-time-sig)
           @string-append{
             <time>
               <beats>@(number->string (syntax-e #'num))</beats>
               <beat-type>@(number->string (syntax-e #'denom))</beat-type>
            </time>
           }]
          [else ""])
         </attributes>
         })

     @string-append{
       <measure number="@(number->string (add1 (syntax-e #'ix)))">
       @attributes
       @(string-join notes "\n")
       </measure>
     }]))

;; unload-musicxml: 
;;  (@ [voice] (seq (@ [index] (mxml-measure (seq (@ [index duration] mxml-note)))))) | k -> unchanged
;; convert sequences of mxml measures with appropriate coords into musicxml
(define-art-realizer unload-musicxml
  (λ (stx)
    (define seqs (context-ref* (current-ctxt) #'seq))
    (define-values (score-parts parts) 
      (for/foldr ([score-parts '()] [parts '()])
                 ([seq seqs])
        (define/syntax-parse (_ v) (require-context (current-ctxt) seq #'voice))
        (syntax-parse seq
          [(_ expr ...)
           (define ms (for/list ([m (context-ref* (syntax->list #'(expr ...)) #'mxml-measure)]) (generate-measure m)))
           (values (cons (generate-score-part #'v) score-parts) 
                   (cons (generate-part #'v ms) parts))])))
     #`(read-xml (open-input-string #,(generate score-parts parts)))))

#;(write-xml
 (realize (unload-musicxml)
  (load-musicxml "bells.musicxml" [melody]))
 (open-output-file "bells2.musicxml" #:exists 'replace))

#;(realize (quote-realizer)
 (load-musicxml "bells.musicxml" [melody]))

#;(write-xml
  (realize (unload-musicxml)
    (time-sig 3 4)
    (voice@ (melody)
      (-- [1 (note a 0 4)] [1 (note b 0 4)] [1 (note c 1 5)] [2 (note d 0 5)]
          [1 (note f 1 4)] [1 (note g 1 4)] [1 (note c 1 5)]))
    (voice@ (bass)
      (-- [4 (note f 1 3)] [2 (note b 0 3)] [2 (note c 1 4)] [1 (note f 1 3)]))
    
    (enclose-in-measures [note])
    (rewrite-in-seq (insert-rests) (measure->mxml-measure) #:capture [time-sig]))
  (open-output-file "bells2.musicxml" #:exists 'replace))

(define-for-syntax (split-n li n) (if (< (length li) n) (list li) (cons (take li n) (split-n (drop li n) n))))

(define-art-realizer draw-quoted
  (λ (stx)
    (syntax-parse stx
      [(_ colo:id)
       #`(text #,(program-format (string-trim (syntax->string #`(#,@(current-ctxt))))) 18 '#,(syntax-e #'colo))])))

#;(define-art-realizer draw-music-realizer
  )

(define (draw-arrow width color)
   (above/align 'right (line 10 5 color) (line width 0 color) (line 10 -5 color)))

(define-for-syntax (do-draw-trace exprs width height ctxt im- embed)
  ;; we'll rewrite step by step and draw each step
  (for/fold ([acc ctxt] [im im-]) 
            ([e exprs])
    (define-values (ctxt* im+)
      (syntax-parse e
        [(head:id expr ...)
         #:do [(define maybe-embed (syntax-local-value #'head))]
         #:when (embed/s? maybe-embed) 
         (define-values (c i) 
           (do-draw-trace (syntax->list #'(expr ...)) width height '() #'empty-image (λ (x) (list #`(head #,@(embed x))))))
         (values c #`(above/align 'left (text #,(format "~a:" (symbol->string (syntax->datum #'head))) 20 'purple) #,i))]
        [_
         (define rewriter-image (realize-art-exprs #'(draw-quoted yellow) (list e)))
         (define rewriter-image*
           #`(above/align 'left  #,rewriter-image (rectangle 10 10 'solid 'transparent) (draw-arrow (image-width #,rewriter-image)'purple)))
         (define rewritten (run-art-expr e acc))
         (define rewritten-image (realize-art-exprs #'(draw-realizer [800 100]) (embed rewritten)))
         (values rewritten #`(above/align 'left #,rewriter-image* #,rewritten-image))]))
    (values ctxt* #`(above/align 'left #,im #,im+))))

(define-art-realizer draw-trace-realizer
  (λ (stx)
    (define-values (width height) 
      (syntax-parse stx [(_ [w:number h:number]) (values (syntax-e #'w) (syntax-e #'h))]))
    (define refs (context-ref* (current-ctxt) #'reflected))
    #`(overlay
        #,(for/fold ([im #'empty-image])
                    ([ref refs])
          (syntax-parse ref
            [(_ expr ...) 
             (define-values (ctxt im*)
               (do-draw-trace (syntax->list #'(expr ...)) width height '() im (λ (x) x)))
             im*]))
        (rectangle #,width #,height 'solid 'transparent))))



(begin-for-syntax 
  (define recursive-drawers (make-free-id-table))
  (struct drawer/s [body])
  (define drawer-width (make-parameter 0))
  (define drawer-height (make-parameter 0)))

(define-syntax (define-drawer stx)
  (syntax-parse stx
    [(_ name:id body)
     #'(define-syntax name (drawer/s body))]))

(define-for-syntax (do-draw-seq ctxt width height)
  (define max-ix
    (for/fold ([acc '()])
              ([expr ctxt])
      (define ix (expr-index expr))
      (if acc (max-index acc ix) ix)))
  (when (> (length max-ix) 2) (raise-syntax-error 'draw-seq "cannot draw indexes greater than 2 yet" #f))

  (define-values (each-width each-height)
    (cond 
      [(= (length max-ix) 2)
       (values (floor (/ width (add1 (cadr max-ix)))) (floor (/ height (add1 (car max-ix)))))]
      [(= (length max-ix) 1)
       (values (floor (/ width (add1 (car max-ix)))) (floor height))]
      [(= (length max-ix) 0)
      (values (floor width) (floor height))]))

  (define get-expr-single-index 
    (cond 
      [(= (length max-ix) 2) expr-index]
      [(= (length max-ix) 1)
       (λ (e) (list 0 (expr-single-index e)))]
      [(= (length max-ix) 0)
       (λ (e) (list 0 0))]))

  (define result
    (for/fold ([acc (hash)]) 
              ([e ctxt])
       (hash-set acc (get-expr-single-index e)
         (syntax-parse e
           [({~literal seq} expr ...)
            (define sub-result (do-draw-seq (syntax->list #'(expr ...)) (- each-width 20) (- each-height 20)))
            ;; draw a box for nested sequences, apl style
            #`(overlay (rectangle #,each-width #,each-height 'outline 'blue) #,sub-result)]
           [_ (drawer-recur e)]))))

  (define result2 
    (for/fold ([im #'empty-image])
              ([(ix expr) (in-hash result)])
      #`(overlay/xy #,im (* #,each-width #,(cadr ix)) (* #,each-height #,(car ix)) #,expr)))
  result2)

(define-drawer draw-seq
  (λ (stx)
    (syntax-parse stx
      [(_ expr ...)
       (do-draw-seq (syntax->list #'(expr ...)) (drawer-width) (drawer-height))])))

(define-for-syntax (do-draw-music-voice ctxt* each-height)

  (define max-end
    (for/fold ([acc 0]) ([e ctxt*])
      (define end (expr-interval-end e))
      (if (infinite? end) acc (max acc end))))

  (define each-width (/ (drawer-width) (add1 max-end)))

  (for/fold ([im #'empty-image])
            ([e ctxt*])
    (match-define (cons start end) (expr-interval e))

    (define end* (if (infinite? end) max-end end))
    (define x-start (* start each-width))
    (define x-end (* (add1 end*) each-width))
    (define width* (- x-end x-start))

    (define sub-pic 
      (parameterize ([drawer-width width*] [drawer-height each-height]) 
        (drawer-recur e)))

    #`(overlay/offset #,sub-pic #,(- x-start) 0 #,im)))
   
(define-for-syntax (do-draw-music ctxt)
  ;; FIXME jagen library fn

  (define voices (voice-find-all ctxt))
  (define each-height (/ (drawer-height) (add1 (set-count voices))))

  #`(above/align 'left
    #,@(for/list ([voice-index (in-naturals)] [v voices])
         (define ctxt* (filter (λ (expr) (context-within? (get-id-ctxt expr) (list #`(voice #,v)) ctxt)) ctxt))

         #`(beside (overlay (text #,(format "~a:" (symbol->string (syntax->datum v))) 16 'black) (rectangle 60 #,each-height 'solid 'transparent))
             #,(do-draw-music-voice ctxt* each-height)))
    empty-image))
  

(define-drawer draw-music 
  (λ (stx)
    (syntax-parse stx
      [(_ expr ...) (do-draw-music (syntax->list #'(expr ...)))])))

(define-syntax (register-drawer! stx)
  (syntax-parse stx
    [(_ head:id r:id) (free-id-table-set! recursive-drawers #'head #'r) #'(void)]))

(define-for-syntax (drawer-recur stx)
  (syntax-parse stx
    [(head:id _ ...)
     (define draw (free-id-table-ref recursive-drawers #'head (λ () #f)))
     (if draw ((drawer/s-body (syntax-local-value draw)) stx) (realize-art-exprs #'(draw-quoted blue) (list stx)))]))

(define-for-syntax (do-draw-note p a o)
  (define p* (string-upcase (symbol->string (syntax-e p))))
  (define a* (match (syntax-e a) [0 ""] [1 "#"] [-1 "b"]))
  (define o* (syntax-e o))
  #`(add-line (overlay (text #,(format "~a~a~s" p* a* o*) 12 'red) (circle 9 'outline 'black) (circle 8 'solid 'blue)) 18 9 18 -20 'black))

(define-drawer draw-note
  (λ (stx)
    (syntax-parse stx
      [({~datum note} p a o) (do-draw-note #'p #'a #'o)])))

(define-drawer draw-mxml-note
  (λ (stx)
    (syntax-parse stx
      [(_ p a o _ ...)
       (define drawn (do-draw-note #'p #'a #'o))
       #`(overlay/align 'left 'top (text "< / >" 12 'black) #,drawn)])))

(define-drawer draw-mxml-rest
  (λ (stx)
    (syntax-parse stx
      [({~literal mxml-rest} _) #`(overlay (above (rectangle 10 5 'solid 'black) (line 14 0 'black))
                                           (rectangle 40 50 'solid 'transparent))])))

(define-for-syntax (measure-spacer)
  #'(overlay (line 0 40 'black) (rectangle 10 40 'solid 'transparent)))

(define-drawer draw-measure
  (λ (stx)
    (syntax-parse stx
      [(_ expr ...)
       (define d (do-draw-music-voice (syntax->list #'(expr ...)) (drawer-height)))
       #`(beside #,(measure-spacer) #,d #,(measure-spacer))])))

(define-drawer draw-mxml-measure
  (λ (stx)
    (syntax-parse stx
      [(_ expr ...)
       (define d (do-draw-music-voice (syntax->list #'(expr ...)) (drawer-height)))
       #`(beside #,(measure-spacer) #,d #,(measure-spacer))])))

(define-art-realizer draw-realizer 
  (λ (stx) 
    (syntax-parse stx
      [(_ [width:number height:number])
       #`(above
         #,@(for/list ([e (current-ctxt)]) 
              (parameterize ([drawer-width (syntax-e #'width)] 
                             [drawer-height (syntax-e #'height)]) 
                (drawer-recur e)))
         empty-image)])))

(register-drawer! note draw-note)
(register-drawer! mxml-rest draw-mxml-rest)
(register-drawer! mxml-note draw-mxml-note)
(register-drawer! music draw-music)
(register-drawer! seq draw-seq)
(register-drawer! measure draw-measure)
(register-drawer! mxml-measure draw-mxml-measure)

#;(realize (draw-realizer [800 100]) 
  (music (voice@ (melody) 
           (-- [2 (note a 0 4)] [1 (note b 0 4)] [0.5 (note c 1 5)] [0.5 (note d 0 5)] [1 (note d -1 5)]))
         (voice@ (accomp) 
           (-- [1 (note a 0 3)] [2 (note e 0 3)] [1 (note a 0 3)] [1 (note f 1 3)] [1 (note a 1 3)]))
         (time-sig 4 4)
         (note->mxml-note)))


(define-interpretation rudolph)
(interpretation+ rudolph
  [rudolph-the-red (context (rhythm 0.5 1 0.5 1 1 1 3) (seq (^s 5 6 5 3 8 6 5)))]
  [had-a-very-shiny (context (rhythm 0.5 0.5 0.5 0.5 1 1 4) (seq (^s 5 6 5 6 5 8 7)))]
  [and-if-you-ever (context (rhythm 0.5 1 0.5 1 1 1 3) (seq (^s 4 5 4 2 7 6 5)))]
  [you-would-even (context (rhythm 0.5 0.5 0.5 0.5 1 1 3) (seq (^s 5 6 5 6 5 6 3)))])

(rs-write
 (realize (music-rsound-realizer)
  (voice@ (melody) (-- [8 (rudolph-the-red)] [8 (had-a-very-shiny)] [8 (and-if-you-ever)] [8 (you-would-even)]))

  (interpret rudolph)
  (apply-rhythm)

  (voice@ (accomp)
    (-- [12 (loop 4 (seq (^s 1 5 -2 5)))] [16 (loop 4 (seq (^s 2 5 -2 5)))] [4 (seq (^s 1 5 -2 5))]))
  (voice@ (accomp) (i@ [0 32] (loop 4 (uniform-rhythm 1))))
  (expand-loop)
  (apply-rhythm)

  (key b -1 major)
  (voice@ (melody) (octave 4))
  (voice@ (accomp) (octave 3))
  (^->note)
    
  (tempo 120)
  (apply-tempo)
    
  (voice@ (melody) (instrument Clarinet))
  (voice@ (accomp) (instrument |Yamaha Grand Piano|))
  (note->midi))
 "rudolph.wav")

  (realize (draw-trace-realizer [1200 800])
    (reflected
      (music
       (voice@ (melody) (-- [8 (rudolph-the-red)] [8 (had-a-very-shiny)] [8 (and-if-you-ever)] [8 (you-would-even)]))

    (interpret rudolph)
    (apply-rhythm)

    (voice@ (accomp)
      (-- [12 (loop 4 (seq (^s 1 5 -2 5)))] [16 (loop 4 (seq (^s 2 5 -2 5)))] [4 (seq (^s 1 5 -2 5))]))
    (voice@ (accomp) (i@ [0 32] (loop 4 (uniform-rhythm 1))))
    (expand-loop)
    (apply-rhythm)

    (key b -1 major)
    (voice@ (melody) (octave 4))
    (voice@ (accomp) (octave 3))
    (^->note)

       
    (time-sig 4 4)
    (enclose-in-measures [note])
    (rewrite-in-seq 
     (insert-rests)
     (measure->mxml-measure) #:capture [time-sig]))))

(write-xml
  (realize (unload-musicxml)
    (voice@ (melody) (-- [8 (rudolph-the-red)] [8 (had-a-very-shiny)] [8 (and-if-you-ever)] [8 (you-would-even)]))

    (interpret rudolph)
    (apply-rhythm)

    (voice@ (accomp)
      (-- [12 (loop 4 (seq (^s 1 5 -2 5)))] [16 (loop 4 (seq (^s 2 5 -2 5)))] [4 (seq (^s 1 5 -2 5))]))
    (voice@ (accomp) (i@ [0 32] (loop 4 (uniform-rhythm 1))))
    (expand-loop)
    (apply-rhythm)

    (key b -1 major)
    (voice@ (melody) (octave 4))
    (voice@ (accomp) (octave 3))
    (^->note)

       
    (time-sig 4 4)
    (enclose-in-measures [note])
    (rewrite-in-seq 
     (insert-rests)
     (measure->mxml-measure) #:capture [time-sig]))
  (open-output-file "rudolph.musicxml" #:exists 'replace))



(realize (draw-trace-realizer [1200 800])
  (reflected
   (music
    (voice@ (melody) (-- [8 (rudolph-the-red)] [8 (had-a-very-shiny)] [8 (and-if-you-ever)] [8 (you-would-even)]))

    (interpret rudolph)
    (apply-rhythm)

    (voice@ (accomp)
      (-- [12 (loop 4 (seq (^s 1 5 -2 5)))] [16 (loop 4 (seq (^s 2 5 -2 5)))] [4 (seq (^s 1 5 -2 5))]))
    (voice@ (accomp) (i@ [0 32] (loop 4 (uniform-rhythm 1))))
    (expand-loop)
    (apply-rhythm)

    (key b -1 major)
    (voice@ (melody) (octave 4))
    (voice@ (accomp) (octave 3))
    (^->note)
    
    (tempo 120)
    (apply-tempo)
    
    (voice@ (melody) (instrument Clarinet))
    (voice@ (accomp) (instrument |Yamaha Grand Piano|))
    (note->midi))))

#;(realize (draw-realizer [800 800])
  (seq (ix-- (note a 0 4) (note b 0 4))))