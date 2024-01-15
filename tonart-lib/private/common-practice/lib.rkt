#lang racket

(require art art/sequence art/timeline
         tonart/private/lib
         tonart/private/electronic/lib
         tonart/private/common-practice/coordinate/metric-interval
         2htdp/image
         (for-syntax syntax/parse racket/match racket/math racket/list tonart/liszt
                     racket/set syntax/id-set))
(provide (all-defined-out) (for-syntax (all-defined-out)))

(module+ test (require rackunit (for-syntax rackunit)))

;;;;;;;;;; notes!  considered fairly fundamental...
(define-art-object (note [pitch accidental octave]))
(define-art-object (tuning [type]))
(define-art-object (music-rest []))

(define-art-rewriter notes
  (λ (stx)
    (syntax-parse stx
      [(_ the-note ...)
       #:with (the-note* ...)
        (for/list ([n (syntax->list #'(the-note ...))])
         (syntax-parse n
          [(p:id a:number o:number) (qq-art n (note p a o))]))
       (qq-art stx (ix-- the-note* ...))])))

(define-for-syntax (do-draw-note p a o)
  (define p* (string-upcase (symbol->string (syntax-e p))))
  (define a* (match (syntax-e a) [0 ""] [1 "#"] [-1 "b"]))
  (define o* (syntax-e o))
  #`(add-line (overlay (text #,(format "~a~a~s" p* a* o*) 12 'red) (circle 9 'outline 'black) (circle 8 'solid 'blue)) 18 9 18 -20 'black))

(define-drawer draw-note
  (λ (stx)
    (syntax-parse stx
      [({~datum note} p a o) (do-draw-note #'p #'a #'o)])))

(register-drawer! note draw-note)

;; convert notes in a context to tones. requires a tuning
(begin-for-syntax
  (define (semitone->freq value octave)
    (define freq (vector-ref #(261.626 277.183 293.665 311.127 329.628 349.228 369.994 391.995 415.305 440.000 466.164 493.883) value))
    (* freq (expt 2 (- octave 4)))))
(define-art-rewriter note->tone
  (syntax-parser
    [_ 
     #:with (result ...)
       (for/fold ([acc '()] #:result (reverse acc)) 
                 ([expr (current-ctxt)])
         (syntax-parse expr
           [({~literal note} p a o)
            (define semis
              (match (syntax-e #'p)
                ['c 0] ['d 2] ['e 4] ['f 5] ['g 7] ['a 9] ['b 11]))
            (define tuning (context-ref/surrounding (current-ctxt) (get-id-ctxt expr) #'tuning))
            (unless tuning (raise-syntax-error 'note->tone "no tuning in context for note" expr))
            (syntax-parse tuning
              [({~literal tuning} {~literal 12tet})
               (with-syntax ([tone-stx (quasisyntax/loc expr (tone #,(semitone->freq (modulo (+ semis (syntax-e #'a)) 12) (syntax-e #'o))))])
                 (values (cons (qq-art expr (context tone-stx)) (cons (delete-expr expr) acc))))])]
           [_ acc]))
     #'(@ () result ...)]))

(define-art-rewriter note->midi
  (syntax-parser
    [_ 
     #:with (result ...)
       (begin
       (define-values (exprs deletes)
         (for/fold ([acc1 '()] [acc2 '()] #:result (values (reverse acc1) (reverse acc2)))
                   ([expr (current-ctxt)])
           (syntax-parse expr
             [({~literal note} p a o)
              (define semis
                (match (syntax-e #'p)
                  ['c 0] ['d 2] ['e 4] ['f 5] ['g 7] ['a 9] ['b 11]))
              (with-syntax ([midi-stx (quasisyntax/loc expr (midi #,(+ 61 semis (syntax-e #'a) (* 12 (- (syntax-e #'o) 4)))))])
                (values (cons (qq-art expr midi-stx) acc1) (cons (delete-expr expr) acc2)))]
             [_ (values acc1 acc2)])))
         (append deletes exprs))
     #'(@ () result ...)]))


(define-art-object (key [pitch accidental mode]))
(define-art-object (^ [degree]))

(begin-for-syntax
  (define-syntax-class ^/sc
    (pattern ({~literal ^} num:number) #:attr accidental #'0)
    (pattern ({~literal ^} num:number accidental:number)))
  (define-syntax-class shorthand-^
    (pattern num:number #:attr accidental #'0)
    (pattern [num:number accidental:number])))

(define-drawer draw-^
  (λ (stx)
    (syntax-parse stx
      [d:^/sc 
       #:with acc (match (syntax-e (attribute d.accidental)) [0 #'""] [1 #'"#"] [-1 #'"b"])
       #'(beside (above (text "^" 24 'blue) (text (~a d.num) 24 'blue)) (text acc 24 'blue))])))

(register-drawer! ^ draw-^)

(define-art-rewriter ^s 
  (λ (stx)
    (syntax-parse stx
      [(_ degree:shorthand-^ ...)
       (qq-art stx (ix-- (^ degree.num degree.accidental) ...))])))

(define-art-object (octave [o]))

(define-art-object (pitch [p a]))

(define-art-rewriter ^->note
  (syntax-parser
    [_ 
     #:with (result ...)
       (begin
         (define-values (exprs deletes)
           (for/fold ([acc1 '()] [acc2 '()] #:result (values (reverse acc1) (reverse acc2)))
                     ([expr (current-ctxt)])
             (syntax-parse expr
               [degree:^/sc
                (define key (require-context (lookup-ctxt) expr #'key))
                (define octave- (require-context (lookup-ctxt) expr #'octave)) 
                (syntax-parse #`(#,key #,octave-)
                  [(({~literal key} pitch:id accidental:number mode:id) ({~literal octave} oct:number))
                   (define octave (syntax-e #'oct))
                   (define scale (generate-scale (syntax->datum #'pitch) (syntax->datum #'accidental) (syntax->datum #'mode)))
                   (define ix* (sub1 (syntax-e #'degree.num)))
                   (match-define (list p a) (list-ref scale (modulo ix* 7)))

                   (define c (index-where scale (λ (x) (eq? (car x) 'c))))
                   (define o (+ octave (floor (/ (- ix* c) 7))))

                   (values (cons (qq-art expr (note #,p #,(+ a (syntax-e #'degree.accidental)) #,o)) acc1) (cons (delete-expr expr) acc2))])]
               [_ (values acc1 acc2)])))
         (append deletes exprs))
     #'(@ () result ...)]))

(define-art-object (^o [degree]))

(begin-for-syntax
  (define-syntax-class ^o/sc
    (pattern ({~literal ^o} num:number octave:number) #:attr accidental #'0)
    (pattern ({~literal ^o} num:number accidental:number octave:number)))
  (define-syntax-class shorthand-^o
    (pattern [num:number octave:number] #:attr accidental #'0)
    (pattern [num:number accidental:number octave:number])))

(define-drawer draw-^o
  (λ (stx)
    (syntax-parse stx
      [d:^o/sc 
       #:with acc (match (syntax-e (attribute d.accidental)) [0 #'""] [1 #'"#"] [-1 #'"b"])
       #'(beside (above (text "^" 24 'blue) (text (~a d.num) 24 'blue)) (text acc 24 'blue) (text (~a d.octave) 24 'blue))])))

(register-drawer! ^o draw-^o)

(define-art-rewriter ^o->note
  (syntax-parser
    [_ 
     #:with (result ...)
       (begin
         (define-values (exprs deletes)
           (for/fold ([acc1 '()] [acc2 '()] #:result (values (reverse acc1) (reverse acc2)))
                     ([expr (current-ctxt)])
             (syntax-parse expr
               [degree:^o/sc
                (define key (require-context (lookup-ctxt) expr #'key))
                (syntax-parse key
                  [({~literal key} pitch:id accidental:number mode:id)
                   (define octave (syntax-e (attribute degree.octave)))
                   (define scale (generate-scale (syntax->datum #'pitch) (syntax->datum #'accidental) (syntax->datum #'mode)))
                   (define ix* (sub1 (syntax-e #'degree.num)))
                   (match-define (list p a) (list-ref scale (modulo ix* 7)))

                   (define c (index-where scale (λ (x) (eq? (car x) 'c))))
                   (define o (+ octave (floor (/ (- ix* c) 7))))

                   (values (cons (qq-art expr (note #,p #,(+ a (syntax-e #'degree.accidental)) #,o)) acc1) (cons (delete-expr expr) acc2))])]
               [_ (values acc1 acc2)])))
         (append deletes exprs))
     #'(@ () result ...)]))

(define-mapping-rewriter (note->^o [(: n note)])
  (λ (stx n)
    (syntax-parse n
      [(_ p a o)
       (define/syntax-parse (_ key-pitch key-accidental mode) (require-context (lookup-ctxt) n #'key))
       #;(define octave- (require-context (lookup-ctxt) expr #'octave)) 
       (define scale (generate-scale (syntax->datum #'key-pitch) (syntax->datum #'key-accidental) (syntax->datum #'mode)))
       (define ix (index-where scale (λ (n*) (eq? (car n*) (syntax-e #'p)))))
       (match-define (list _ a*) (list-ref scale ix))
       (define a** (- a* (syntax-e #'a)))

       (qq-art n (^o #,(add1 ix) #,a** o))])))

(define-art-object (transpose-diatonic []))
(define-mapping-rewriter (run-transpose-diatonic [(: transposes transpose-diatonic)])
  (λ (stx expr)
  (syntax-parse expr
    [(_ val:number) 
     #:with (result ...)
       (begin
         (define-values (exprs deletes)
           (for/fold ([acc1 '()] [acc2 '()] #:result (values (reverse acc1) (reverse acc2)))
                     ;; FIXME jagen
                     ([expr (filter (λ (e) (context-within? (get-id-ctxt e) (get-id-ctxt expr) (current-ctxt))) (current-ctxt))])
             (syntax-parse expr
               [({~literal ^} ix:number)
                (values (cons (qq-art expr (context (^ #,(+ (syntax-e #'val) (syntax-e #'ix))))) acc1) (cons (delete-expr expr) acc2))]
               [_ (values acc1 acc2)])))
         (append deletes exprs))
     #'(@ () result ...)])))

(define-art-object (time-sig [n d]))
(define-art-object (dynamic [level]))

(define-art-rewriter mi@
  (λ(stx)
    (syntax-parse stx
      [(_ [(mstart* bstart*)] expr ...)
       ;; FIXME jagen fix this!!
       (qq-art stx (mi@ [(mstart* bstart*) (+inf.0 +inf.0)] expr ...))]
      [(_ [(mstart* bstart*) (mend* bend*)] expr ...)
       (qq-art stx (@ [(metric-interval (start mstart* bstart*) (end mend* bend*))] expr ...))])))

(define-art-rewriter measure@
  (λ(stx)
    (syntax-parse stx
      [(_ start:number expr ...) (qq-art stx (mi@ [(start 1)] expr ...))]
      [(_ [start:number end:number] expr ...) (qq-art stx (mi@ [(start 1) (#,(add1 (syntax-e #'end)) 1)] expr ...))])))

(define-art-rewriter music@
  (λ(stx)
    (syntax-parse stx
      [(_ [(measure:number beat:number) (voice:id ...)] expr ...) 
       (qq-art stx (music@ [(measure beat) (+inf.0 +inf.0) (voice ...)] expr ...))]
      [(_ [(mstart:number bstart:number) (mend:number bend:number) (voice:id ...)] expr ...)
       (qq-art stx (mi@ [(mstart bstart) (mend bend)] (voice@ (voice ...) expr ...)))])))

(define-for-syntax (do-metric-interval->interval stx ctxt)
  (syntax-parse stx 
    [(_ (_ ms*:number bs*:number) (_ me*:number be*:number))
     (qq-art stx
       (interval 
         (start #,(+ (* 4 (sub1 (syntax-e #'ms*))) (sub1 (syntax-e #'bs*))))
         (end #,(+ (* 4 (sub1 (syntax-e #'me*))) (sub1 (syntax-e #'be*))))))]
    [_ #f]))

(define-art-rewriter metric-interval->interval
  (λ (stx)
    (syntax-parse stx
      [_
       (define exprs
         (flatten
           (for/list ([expr (current-ctxt)])
             (define the-minterval (context-ref (get-id-ctxt expr) #'metric-interval))
             (if the-minterval
               (list (delete-expr expr) 
                     (put-in-id-ctxt 
                       (remove-from-id-ctxt expr #'metric-interval) 
                       (do-metric-interval->interval the-minterval (current-ctxt))))
               '()))))
       #`(@ () #,@exprs)])))

(define-for-syntax (do-interval->metric-interval stx ctxt)
  (syntax-parse stx 
    [(_ (_ start*:number) (_ end*:number))
     (qq-art stx
       (metric-interval 
         (start #,(add1 (floor (/ (syntax-e #'start*) 4))) #,(add1 (float-modulo (syntax-e #'start*) 4)))
         (end #,(add1 (floor (/ (syntax-e #'end*) 4))) #,(add1 (float-modulo (syntax-e #'end*) 4)))))]
    [_ #f]))

(module+ test
  (begin-for-syntax
  (check-equal? 
    (syntax->datum 
      (do-interval->metric-interval #'(interval (start 3) (end 20))
        (list (set-id-ctxt #'(time-sig 4 4) (list #'(interval (start 0) (end 100)))))))
    '(metric-interval (start 1 4) (end 6 1)))))

(define-art-rewriter interval->metric-interval
  (λ (stx)
    (syntax-parse stx
      [_
       (define exprs
         (flatten
           (for/list ([expr (current-ctxt)])
             (define the-interval (context-ref (get-id-ctxt expr) #'interval))
             (if the-interval
               (list (delete-expr expr) 
                     (put-in-id-ctxt 
                       (remove-from-id-ctxt expr #'interval) 
                       (do-interval->metric-interval the-interval (current-ctxt))))
               '()))))
       #`(@ () #,@exprs)])))

(module+ test
  (begin-for-syntax
  (check-equal? 
    (syntax->datum 
      (do-metric-interval->interval #'(metric-interval (start 1 4) (end 6 1))
        (list (set-id-ctxt #'(time-sig 4 4) (list #'(interval (start 0) (end 100)))))))
    '(interval (start 3) (end 20)))))

(define-nonhom-merge-rule metric-interval interval #:keep-right
  (λ (l r l* _ ctxt)
    (do-merge-interval (do-metric-interval->interval l ctxt) r)))

(define-nonhom-merge-rule interval metric-interval #:keep-right
  (λ (l r l* _ ctxt)
    (do-merge-metric-interval (do-interval->metric-interval l ctxt) r)))

(define-nonhom-within?-rule metric-interval interval
  (λ (l r l* _ ctxt)
    (do-interval-within? (do-metric-interval->interval l ctxt) r)))

(define-nonhom-within?-rule interval metric-interval
  (λ (l r l* _ ctxt)
    (do-interval-within? l (do-metric-interval->interval r ctxt))))

(define-art-object (chord [pitch accidental mode]))

(define-art-rewriter chords
  (λ (stx)
    (syntax-parse stx
      [(_ the-chord ...)
       #:with (the-chord* ...)
        (for/list ([ch (syntax->list #'(the-chord ...))])
         (syntax-parse ch
          [(p:id a:number mods ... {~seq #:v [voice ...]}) (qq-art ch (voiced-chord p a [mods ...] voice ...))]
          [(p:id a:number mods ...) (qq-art ch (chord p a [mods ...]))]))
       (qq-art stx (ix-- the-chord* ...))])))

(define-art-object (relative-harmony [chords]))

(define-for-syntax (odd-even-list li) 
  (define (odd-even-list li lacc racc)
    (cond 
      [(null? li) (values (reverse lacc) (reverse racc))]
      [(null? (cdr li)) (values (cons (car li) (reverse lacc)) (reverse racc))]
      [else (odd-even-list (cddr li) (cons (car li) lacc) (cons (cadr li) racc))]))
  (odd-even-list li '() '()))

(define-mapping-rewriter (relative-harmony->chord-seq [(: harm relative-harmony)])
  (λ (stx harm)
    (syntax-parse harm
      [(_ harmony ...)
       (define start-pitch (context-ref/surrounding (current-ctxt) (get-id-ctxt harm) #'pitch))
       (unless start-pitch (raise-syntax-error 'relative-harmony->chord-seq "no pitch in context for relative harmony" harm)) 
       (syntax-parse start-pitch
         [(_ p*:id a*:number)
          #:do
            [(define pitch (map syntax-e (list #'p* #'a*))) 
             (define-values (chord-types transitions) (odd-even-list (syntax->datum #'(harmony ...))))]
          #:with (chords ...)
            (for/fold ([chords '()] [pitch pitch] #:result (reverse chords)) 
                      ([chord-type chord-types] [transition (cons '(P 1) transitions)])
              (define new-pitch (transpose-by-interval (first pitch) (second pitch) (second transition) (first transition)))
              (values (cons #`(chord #,(first new-pitch) #,(second new-pitch) #,chord-type) chords) new-pitch))
          (qq-art harm (seq (ix-- chords ...)))])])))

(define-mapping-rewriter (chord->notes/simple [(: crd chord)])
  (λ (stx crd)
    (syntax-parse stx
      [(_ octave*:number)
       (syntax-parse crd
         [(_ root accidental [mod ...])
          #:do [ 
            (define octave (syntax-e #'octave*))
            (define pcs (generate-chord (syntax-e #'root) (syntax-e #'accidental) (syntax->datum #'(mod ...))))
            (define cdis (distance-above-c (caar pcs)))]
          #:with (result ...)
            (for/list ([pc pcs])
              (with-syntax ([p (first pc)] 
                            [a (second pc)] 
                            [o (if (>= (distance-above-c (first pc)) cdis) octave (add1 octave))])
                (qq-art crd (note p a o))))
          #'(@ () result ...)])])))
 
(define-art-object (voiced-chord [pitch accidental mode notes ...]))

;; (seq chord | voiced-chord) -> (seq voiced-chord)
(define-art-rewriter voice-lead
  (λ (stx)
    (syntax-parse stx
      [(_ n:number)
       #:do [
        (define chords
        ;; FIXME jagen context-ref/within!!!
          (filter 
            (λ (e) 
              (and 
                (free-identifier=? (car (syntax->list e)) #'voiced-chord)
                (context-within? (get-id-ctxt e) (get-id-ctxt stx) (current-ctxt))))
            (current-ctxt)))
      
        (define empty-hint (build-list (syntax-e #'n) (λ (_) #f)))
        (define hints 
           (for/list ([chord chords])
             (syntax-parse chord
               [({~literal chord} p a [mod ...])
                empty-hint]
               [({~literal voiced-chord} cp ca [cam ...] val ...)
                (map (λ (v)
                  (syntax-parse v
                    [(p:id a:number o:number) (syntax->datum #'(p a o))]
                    [{~literal _} #f]))
                  (syntax->list #'(val ...)))])))

          (define chords*
            (for/list ([chord chords])
              (syntax-parse chord
                [({~literal voiced-chord} cp ca [cm ...] _ ...)
                 (syntax->datum #'(cp ca [cm ...]))])))

          (define result (generate-voice-leading chords* hints))]
          #:with (result* ...)
            (flatten
              (for/list ([old-chord chords] [chord chords*] [notes result]) 
                (list (delete-expr old-chord) (qq-art old-chord (voiced-chord #,@chord #,@notes)))))
            #'(@ () result* ...)])))

(define-mapping-rewriter (voiced-chord->note-seq [(: ch voiced-chord)])
  (λ (stx ch)
    (syntax-parse ch
      [(_ _ _ _ (p a o) ...)
       (qq-art ch (seq (ix-- (note p a o) ...)))])))

(define-mapping-rewriter (chord->voiced-chord [(: ch chord)])
  (λ (stx ch)
    (syntax-parse stx
      [(_ n:number)
       (syntax-parse ch
         [(_ p a o) 
          ;; build a list of n underscores, indicating an unspecified voice leading for n voices
          #:with (uscores ...) (build-list (syntax-e #'n) (λ (_) #'_))
         (qq-art ch (voiced-chord p a o uscores ...))])])))

(define-mapping-rewriter (fill-voice [(: vc voiced-chord)])
  (λ (stx ch)
    (syntax-parse stx
      [(_ voice-ix:number)
       (syntax-parse ch
        [(_ p a mods voice ...)
         #:do [
           (define the-note-sequences
             (filter (λ (x) (syntax-parse x [(_ ({~literal note} _ ...) ...) #t] [_ #f]))
                     (context-ref*/surrounding (current-ctxt) (get-id-ctxt ch) #'seq)))
           (when (null? the-note-sequences) (raise-syntax-error 'fill-voice "oops" ch))
           (define the-note-sequence 
             (syntax-parse (car the-note-sequences)
               [(_ (_ p a o) ...) (syntax->list #'((p a o) ...))]))
         ]
         #:with (voice* ...) 
           (list-set (syntax->list #'(voice ...)) (syntax-e #'voice-ix) 
             (list-ref the-note-sequence 
               (syntax-parse (context-ref (get-id-ctxt ch) #'index)
                 [(_ ix:number) (syntax-e #'ix)])))
         (qq-art ch (context #,(delete-expr (car the-note-sequences)) (voiced-chord p a mods voice* ...)))])])))

(define-mapping-rewriter (fill-harmony [(: ch voiced-chord)])
  (λ (stx ch)
    (syntax-parse stx
      [(_ chord-ix:number)
       #:when 
         (syntax-parse (context-ref (get-id-ctxt ch) #'index)
           [(_ ix:number) (= (syntax-e #'chord-ix) (syntax-e #'ix))])
       (syntax-parse ch
         [(_ p a mods _ ...)
          #:do [
            (define the-note-sequences
              (filter (λ (x) (syntax-parse x [(_ ({~literal note} _ ...) ...) #t] [_ #f]))
                      (context-ref*/surrounding (current-ctxt) (get-id-ctxt ch) #'seq)))
            (when (null? the-note-sequences) (raise-syntax-error 'fill-harmony "oops" ch))]
          #:with (_ (_ p* a* o*) ...) (car the-note-sequences)
          (qq-art ch (context #,(delete-expr (car the-note-sequences)) (voiced-chord p a mods (p* a* o*) ...)))])]
      [_ ch])))

(define-art-embedding (measure [items])
  (λ (stx ctxt)
    (syntax-parse stx
      [(head:id expr ...)
       (rewrite (quasisyntax/loc stx (context expr ...)))])))

(define-mapping-rewriter (rewrite-in-measure [(: s measure)])
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
             #`(context #,(qq-art s (measure result ...)))])]
       ;; FIXME jagen
       [(head expr ...) (go #'(head expr ... #:capture []) s)]))
    go))

(define-for-syntax (measure-spacer)
  #'(overlay (line 0 40 'black) (rectangle 10 40 'solid 'transparent)))

(define-drawer draw-measure
  (λ (stx)
    (syntax-parse stx
      [(_ expr ...)
       (define d (do-draw-music-voice (syntax->list #'(expr ...)) (drawer-height)))
       #`(beside #,(measure-spacer) #,d #,(measure-spacer))])))

(register-drawer! measure draw-measure)

(define-art-rewriter enclose-in-measures
  (λ (stx)
    
    (define objs-to-enclose (syntax-parse stx [(_ [objs:id ...]) (immutable-free-id-set (syntax->list #'(objs ...)))]))
    (define voices (voice-find-all (current-ctxt)))

    (define ctxt*-
      (filter 
        (λ (e) (syntax-parse e [(head:id _ ...) (free-id-set-member? objs-to-enclose #'head)]))
        (current-ctxt)))

    ;; make end the same for all voices so we get the same number of measures
    (define end (apply max (cons 0 
      (map (λ (e) (or (and (not (infinite? (expr-interval-end e))) (expr-interval-end e)) 0)) ctxt*-))))

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
                  (define result 
                    (put-in-id-ctxt 
                      (remove-from-id-ctxt (cdr obj) #'voice) 
                      #`(interval (start #,(- (car iv) s)) (end #,(- (cdr iv) s)))))
                  (cond
                    [(equal? iv iv*) result]
                    [(= (car iv) (car iv*)) (put-in-id-ctxt result #'(tie start))]
                    [(= (cdr iv) (cdr iv*)) (put-in-id-ctxt result #'(tie stop))]
                    [else (println obj) (println iv*) 
                          result
                          #;(error 'enclose-in-measures "not sure yet")])))
              #`(measure #,@objs)))

        #`(voice@ (#,v) #,@(map delete-expr ctxt*) (seq (ix-- #,@result))))))
    #`(context #,@voice-results)))

(define-art-rewriter insert-rests
  (λ (stx)
    (define voices (voice-find-all (current-ctxt)))

    (define voice-results
      (for/list ([v (in-set voices)])
        (define ctxt*
          (filter (λ (e) (context-within? (get-id-ctxt e) (list #`(voice #,v)) (current-ctxt)))
            (current-ctxt)))
        (define measures (context-ref*/within ctxt* (get-id-ctxt stx) #'measure))
        (define result 
          (for/list ([i (in-naturals)] [m measures])
            (syntax-parse m
              [(_ expr ...)
               (define/syntax-parse (_ num _) (require-context (lookup-ctxt) m #'time-sig))
               (define sorted-exprs (sort (syntax->list #'(expr ...)) < #:key expr-interval-start))
               (define init 
                 (cond [(or (empty? sorted-exprs) (zero? (expr-interval-start (car sorted-exprs))))
                        '()]
                       [else
                        (list (put-in-id-ctxt (qq-art (car sorted-exprs) (music-rest)) 
                                              #`(interval (start 0) (end #,(expr-interval-start (car sorted-exprs))))))]))
               (qq-art m 
                 (measure
                   #,@(for/fold ([acc init] #:result (reverse acc))
                                ([e sorted-exprs] 
                                 [e2 (cdr (append sorted-exprs
                                             (list (put-in-id-ctxt (ensure-id-ctxt #'(music-rest))
                                                     #`(interval (start #,(syntax-e #'num)) (end +inf.0))))))])
                        (define t (expr-interval-end e))
                        (define t* (expr-interval-start e2))
                        (if (= t t*)
                          (cons e acc)
                          (cons e (cons (put-in-id-ctxt (ensure-id-ctxt #'(music-rest)) #`(interval (start #,t) (end #,t*))) acc))))))])))
        #`(voice@ (#,v) #,@(map delete-expr measures) #,@result)))
      #`(context #,@voice-results)))

(define-mapping-rewriter (number->^ [(: n number)])
  (λ (stx n)
    (syntax-parse n
      [(_ val) (qq-art n (^ val))])))
