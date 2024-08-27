#lang at-exp racket

(require (except-in art bitmap) art/sequence art/timeline art/sequence/ravel 
         tonart/private/lib tonart/common-practice tonart/rsound 
         xml 2htdp/image
  (for-syntax racket/string syntax/parse racket/list racket/match racket/dict racket/set syntax/to-string fmt racket/math
              (except-in xml attribute) "mxml.rkt" syntax/id-table syntax/id-set))

(provide (all-defined-out) (for-syntax (all-defined-out)))
  
(define-coordinate (duration [d]))
;; FIXME jagen cruft- this should be default
(define-hom-merge-rule duration (λ (l r _ __ ___) (or r l)))
(define-hom-within?-rule duration (λ (l r _ __ ___) #t))

(define-for-syntax (expr-duration stx)
  (syntax-parse (context-ref (get-id-ctxt stx) #'duration) 
    [(_ n:number) (syntax-e #'n)]
    [_ #f]))

;; should this even me a coord or just an object that goes in id ctxt
(define-coordinate (tie [start]))
(define-hom-merge-rule tie (λ (l r _ __ ___) (or r l)))
(define-hom-within?-rule tie (λ (l r _ __ ___) #t))

(define-art-object (mxml-note [p a o]))
(define-art-object (mxml-rest []))
(define-art-object (mxml-chord [notes]))

(define-art-embedding (mxml-measure [items])
  (λ (stx ctxt)
    (syntax-parse stx
      [(head:id expr ...)
       (rewrite (quasisyntax/loc stx (context expr ...)))])))

(define-mapping-rewriter (rewrite-in-mxml-measure [(: s mxml-measure)])
  (let ()
    (define (go stx s)
      (syntax-parse stx
        [(_ expr ... {~seq #:capture [name:id ...]})
         (syntax-parse s
           [(_ expr* ...)
             #:do [
              (define names (immutable-free-id-set (syntax->list #'(name ...))))
              (define captures 
                (if (attribute name)
                  (filter 
                    (λ (e) (syntax-parse e 
                      [(head:id _ ...) (free-id-set-member? names #'head)]))
                    (current-ctxt))
                  '()))]
             #:with (result ...) 
               (rewrite-in (append captures (syntax->list #'(expr* ...))) #`(context expr ... #,@(map delete-expr captures)))
             #`(context #,(qq-art s (mxml-measure result ...)))])]
       ;; FIXME jagen
       [(head expr ...) (go #'(head expr ... #:capture []) s)]))
    go))

(define-mapping-rewriter (note->mxml-note [(: n note)])
  (λ (stx n)
    (syntax-parse n
      [(_ p a o)
       #:with (_ _ denom) (require-context (lookup-ctxt) n #'time-sig)
       #:with (_ divisions) (require-context (lookup-ctxt) n #'divisions)
       #:do [
        (define the-dur- (- (expr-interval-end n) (expr-interval-start n)))
        (define the-dur (/ the-dur- (syntax-e #'denom) (syntax-e #'divisions)))]
       #:with (dur dot ...)
         (match (inexact->exact the-dur)
           [1/32 #'(16th)]
           [1/16 #'(16th)]
           [1/12 #'(eighth triplet)]
           [1/8 #'(eighth)] [3/16 #'(eighth dot)]
           [1/4 #'(quarter)] [3/8 #'(quarter dot)]
           [1/2 #'(half)] [3/4 #'(half dot)]
           [1 #'(whole)])
       (qq-art n (|@| [(duration #,the-dur-)] (mxml-note p a o dur dot ...)))])))

(define-mapping-rewriter (rest->mxml-rest [(: r music-rest)])
  (λ (stx r)
    (syntax-parse r
      [(_)
       #:with (_ _ denom) (require-context (lookup-ctxt) r #'time-sig)
       #:with (_ divisions) (require-context (lookup-ctxt) r #'divisions)
       #:do [
        (define the-dur- (- (expr-interval-end r) (expr-interval-start r)))
        (define the-dur (/ the-dur- (syntax-e #'denom) (syntax-e #'divisions)))]
       #:with (dur [dot ...] [triplet ...])
         (match (inexact->exact the-dur)
           [1/16 #'(16th [] [])]
           [1/12 #'(eighth [] [triplet])]
           [1/8 #'(eighth [] [])] [3/16 #'(eighth [dot])]
           [1/4 #'(quarter [] [])] [3/8 #'(quarter [dot] [])]
           [1/2 #'(half [] [])] [3/4 #'(half [dot] [])]
           [1 #'(whole [] [])])
       (qq-art r (|@| [(duration #,the-dur-)] (mxml-rest dur dot ... triplet ...)))])))
       
(define-mapping-rewriter (measure->mxml-measure [(: m measure)])
  (λ (stx m)
    (syntax-parse m
      [(_ expr ...)
       (define sig (require-context (lookup-ctxt) m #'time-sig))
       (define result (qq-art m (mxml-measure 
         (seq (ix-- 
           #,@(run-art-exprs 
            (list #'(note->mxml-note) #'(rest->mxml-rest) (delete-expr sig)) 
            (cons sig (syntax->list #'(expr ...)))
            (lookup-ctxt)))))))
       result])))

(define-art-object (note-group []))
(define-mapping-rewriter (mxml-chord->note-group [(: c mxml-chord)])
  (λ (stx c)
    (syntax-parse c
      [(_ ([p a o] ...) _ ...) 
      (qq-art c (note-group [p a o] ...))])))

(define-mapping-rewriter (ungroup-notes [(: ng note-group)])
  (λ (stx ng) (syntax-parse ng [(_ [p a o] ...) (qq-art ng (context (note p a o) ...))])))

(define-mapping-rewriter (mxml-measure->measure [(: m mxml-measure)])
  (λ (stx m)
    (syntax-parse m
      [(_ expr ...)
       (define result 
         (qq-art m (measure 
           #,@(run-art-exprs 
                (list #'(rewrite-in-seq (mxml-note->note) (mxml-rest->rest) (mxml-chord->note-group)))
                (syntax->list #'(expr ...))
                (lookup-ctxt)))))
       result])))

(define-mapping-rewriter (mxml-note->note [(: n mxml-note)])
  (λ (stx n)
    (syntax-parse n [(_ p a o _ ...) (qq-art n (note p a o))])))

(define-for-syntax (mxml-notes->chord ns)
  (define/syntax-parse (mods ...)
    (syntax-parse (car ns)
      [(_ p a o mods ...) #'(mods ...)]))
  (define/syntax-parse (clauses ...)
    (for/list ([n ns])
      (syntax-parse n
        [(_ p a o _ ...) #'[p a o]])))
  (qq-art (car ns) (mxml-chord (clauses ...) mods ...)))

(define-art-rewriter extract-mxml-chords
  (λ (stx)
    (define result
      (for/fold ([acc '()] [current-chord '()] [i -1] #:result (reverse acc))
                ([e (append (current-ctxt) (list #'(mxml-rest)))])
        (syntax-parse e
          [({~literal mxml-note} _ ... {~datum chord} _ ...)
           (values acc (cons e current-chord) i)]
          [({~or {~literal mxml-note} {~literal mxml-rest}} _ ...)
           (cond 
            ;; single note following non-note
            [(null? current-chord) (values acc (list e) (add1 i))] 
            ;; single note following note
            [(= (length current-chord) 1) 
             (values (cons (put-in-id-ctxt (car current-chord) #`(index #,i)) acc) (list e) (add1 i))]
            ;; end of chord
            [else 
              (values 
                (cons (put-in-id-ctxt (mxml-notes->chord current-chord) #`(index #,i)) acc)
                (list e) (add1 i))])])))
    #`(replace-full-context #,@result)))

(define-mapping-rewriter (durations->intervals [(: s seq)])
  (λ (stx s)
    (syntax-parse s
      [(_ expr ...)
       (define ctxt (syntax->list #'(expr ...)))
       (define ordered (sort ctxt < #:key expr-single-index))
       (define result
         (for/fold ([result '()] [t 0] #:result (reverse result)) 
                   ([expr ordered])
           (define dur (expr-duration expr))
           (values (cons #`(i@ [#,t #,(+ t dur)] #,(remove-from-id-ctxt (remove-from-id-ctxt expr #'duration) #'index)) result) (+ t dur))))
       (qq-art stx (context #,@(map delete-expr ordered) #,@result))])))

(define-mapping-rewriter (measure->music [(: m measure)])
  (λ (stx m)
    (syntax-parse m
      [(_ expr ...) (qq-art m (music expr ...))])))

(define-mapping-rewriter (mxml-rest->rest [(: r mxml-rest)])
  (λ (stx r)
    (syntax-parse r [(_ _ ...) (qq-art r (music-rest))])))


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
                   (define rest? (not (null? (xml-path note rest))))
                   (match-define (list p a o dur g)
                     (map (compose syntax-e (λ (x) (if (and x (not (null? x))) (car x) #'#f)))
                       (list (xml-path note pitch step) (xml-path note pitch alter) (xml-path note pitch octave) 
                             (xml-path note duration) (xml-path note type))))
                   ;; FIXME jagen31 bad bad bad
                   (when (and rest? (not g))
                     (set! g #'(_ _ "whole")))
                   (define t- (xml-path note tie))
                   (define t
                     (cond
                       [(null? t-) '()]
                       [(= (length t-) 2) (list #'(tie continue))]
                       [else (list #`(tie #,(string->symbol (syntax-e (xml-attr (car t-) type)))))]))
                    #`(|@| [(duration #,(string->number (syntax-e (xml-value dur)))) #,@t]
                      #,(quasisyntax/loc note 
                       #,(if rest?
                         #`(mxml-rest 
                             #,(string->symbol (syntax-e (xml-value g)))
                             #,@(if (null? (xml-path note dot)) '() (list #'dot)))
                         #`(mxml-note 
                           #,(string->symbol (string-downcase (syntax-e (xml-value p))))
                           #,(string->number (syntax-e (if a (xml-value a) #'"0"))) 
                           #,(string->number (syntax-e (xml-value o)))
                           #,(string->symbol (syntax-e (xml-value g)))
                           #,@(if (null? (xml-path note dot)) '() (list #'dot))
                           #,@(if (null? (xml-path note chord)) '() (list #'chord))))))))
               (define time-sigs 
                 (for/list ([time-sig (xml-path measure attributes time)])
                   (match-define (list n d)
                     (list (xml-value (car (xml-path time-sig beats))) (xml-value (car (xml-path time-sig beat-type)))))
                   (quasisyntax/loc time-sig (time-sig #,(string->number (syntax-e n)) #,(string->number (syntax-e d))))))
               (define divisions
                 (for/list ([division (xml-path measure attributes divisions)])
                   (quasisyntax/loc division (divisions #,(string->number (syntax-e (xml-value division)))))))
             #`(mxml-measure #,@time-sigs #,@divisions (seq (ix-- #,@notes)))))
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
    [({~literal mxml-note} p a o g {~optional {~and the-dot {~datum dot}}}
                                   {~optional {~and the-triplet {~datum triplet}}})
     #:with (_ dur:number) (require-context (lookup-ctxt) n #'duration)
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
        @(if (attribute the-triplet)
          @string-append{
        <time-modification>
          <actual-notes>3</actual-notes>
          <normal-notes>2</normal-notes>
        </time-modification>
          } "")
        <stem>down</stem>
        <notations>
          @visual-tie
        </notations>
        </note>
      }]
      [({~literal mxml-rest} g {~optional {~and the-dot {~datum dot}}})
     #:with (_ dur:number) (require-context (lookup-ctxt) n #'duration)
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
     #:with (_ ix:number) (require-context (lookup-ctxt) m #'index)
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
    (define seqs- (context-ref* (current-ctxt) #'seq))
    (define seqs (filter (λ (x) (syntax-parse x [(_ ({~literal mxml-measure} _ ...) ...) #t] [_ #f])) seqs-))
    (define-values (score-parts parts) 
      (for/foldr ([score-parts '()] [parts '()])
                 ([seq seqs])
        (define/syntax-parse (_ v) (require-context (lookup-ctxt) seq #'voice))
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


#;(define-interpretation rudolph)
#;(interpretation+ rudolph
  [rudolph-the-red (context (rhythm 0.5 1 0.5 1 1 1 3) (seq (^s 5 6 5 3 8 6 5)))]
  [had-a-very-shiny (context (rhythm 0.5 0.5 0.5 0.5 1 1 4) (seq (^s 5 6 5 6 5 8 7)))]
  [and-if-you-ever (context (rhythm 0.5 1 0.5 1 1 1 3) (seq (^s 4 5 4 2 7 6 5)))]
  [you-would-even (context (rhythm 0.5 0.5 0.5 0.5 1 1 3) (seq (^s 5 6 5 6 5 6 3)))])

#;(rs-write
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

  #;(realize (draw-trace-realizer [1200 800])
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

#;(write-xml
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



#;(realize (draw-trace-realizer [1200 800])
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


(define-art-rewriter musicxml->tonart
  (λ (stx)
    (qq-art stx
      (context
       (rewrite-in-seq
        (rewrite-in-mxml-measure
         (rewrite-in-seq (extract-mxml-chords)))
        (mxml-measure->measure)
        (rewrite-in-measure (durations->intervals) (ungroup-notes))
        (measure->music))
       ;; FIXME jagen do it
       (inline-music-seq) #;(unsubdivide)))))