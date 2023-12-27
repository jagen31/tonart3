#lang at-exp racket

(require art art/sequence art/timeline art/sequence/ravel 
         tonart/private/lib tonart/common-practice tonart/rsound 
         xml 2htdp/image 2htdp/universe (prefix-in cute: 2htdp/planetcute)
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


(define-art-object (mxml-note [p a o]))
(define-art-object (mxml-rest []))

(define-art-embedding (mxml-measure [items])
  (λ (stx ctxt)
    (syntax-parse stx
      [(head:id expr ...)
       (rewrite (quasisyntax/loc stx (|@| () expr ...)))])))


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
       (define result (qq-art m (mxml-measure 
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


(define-for-syntax (generate-mxml score-parts parts)
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
     #`(read-xml (open-input-string #,(generate-mxml score-parts parts)))))

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

(define-drawer draw-mxml-measure
  (λ (stx)
    (syntax-parse stx
      [(_ expr ...)
       (define d (do-draw-music-voice (syntax->list #'(expr ...)) (drawer-height)))
       #`(beside #,(measure-spacer) #,d #,(measure-spacer))])))

(register-drawer! mxml-rest draw-mxml-rest)
(register-drawer! mxml-note draw-mxml-note)
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
