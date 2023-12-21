#lang at-exp racket

(require art art/sequence art/timeline tonart/private/rewriter/lib tonart/common-practice xml 2htdp/image 2htdp/universe (prefix-in cute: 2htdp/planetcute)
  (for-syntax racket/string syntax/parse racket/list racket/match racket/dict racket/set syntax/to-string fmt
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
    (syntax-parse n
      [(_ p a o)
       #:with (_ _ denom) (require-context (current-ctxt) n #'time-sig)
       #:do [
        (define the-dur- (- (expr-interval-end n) (expr-interval-start n)))
        (define the-dur (/ the-dur- (syntax-e #'denom)))]
       #:with (dur dot ...)
         (match the-dur
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
         (match the-dur
           [1/16 #'(sixteenth)]
           [1/8 #'(eighth)] [3/16 #'(eighth dot)]
           [1/4 #'(quarter)] [3/8 #'(quarter dot)]
           [1/2 #'(half)] [3/4 #'(half dot)]
           [1 #'(whole)])
       (qq-art r (|@| [(duration #,the-dur-)] (mxml-rest dur dot ...)))])))
       

(define-mapping-rewriter (measure->mxml-measure [(: m measure)])
  (λ (stx m)
    (syntax-parse m
      [(_ expr ...)
       (define sig (require-context (current-ctxt) m #'time-sig))
       (qq-art m (mxml-measure 
         (seq (ix-- 
           #,@(run-art-exprs 
            (list #'(note->mxml-note) #'(rest->mxml-rest) (delete-expr sig)) 
            (cons sig (syntax->list #'(expr ...))))))))])))

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

(write-xml
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
       #`(text #,(program-format (string-trim (syntax->string #`(#,@(current-ctxt))))) 24 '#,(syntax-e #'colo))])))

#;(define-art-realizer draw-music-realizer
  )

(define-art-realizer trace-rewrite-realizer
  (λ (stx)
    (define-values (width height) 
      (syntax-parse stx [(_ [w:number h:number]) (values (syntax-e #'w) (syntax-e #'h))]))
    (define refs (context-ref* (current-ctxt) #'reflected))
    #`(overlay
        #,(for/fold ([im #'empty-image])
                    ([ref refs])
          #`(above/align 'left #,im 
            #,(syntax-parse ref
               [(_ expr ...)
                ;; we'll rewrite step by step and draw each step
                (for/fold ([acc '()] [im #'empty-image] #:result im) 
                          ([e (syntax->list #'(expr ...))])
                  (define rewriter-image 
                    #`(underlay (triangle 10 'solid 'blue) #,(realize-art-exprs #'(draw-quoted blue) (list e))))
                  (define rewritten (run-art-expr e acc))
                  (define rewritten-image (realize-art-exprs #'(draw-quoted red) rewritten))
                  (values rewritten #`(above/align 'left #,im #,rewriter-image #,rewritten-image)))])))
        (rectangle #,width #,height 'solid 'transparent))))


  (realize (trace-rewrite-realizer [800 800])
    (reflected
      (time-sig 3 4)
      (voice@ (melody)
        (-- [1 (note a 0 4)] [1 (note b 0 4)] [1 (note c 1 5)] [2 (note d 0 5)]
            [1 (note f 1 4)] [1 (note g 1 4)] [1 (note a 1 5)]))
      (voice@ (bass)
        (-- [4 (note f 1 3)] [2 (note b 0 3)] [2 (note c 1 4)] [1 (note f 1 3)]))
      (enclose-in-measures [note])
      (rewrite-in-seq 
        (insert-rests)
        (measure->mxml-measure) #:capture [time-sig])))
