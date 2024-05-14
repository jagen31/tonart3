#lang racket

(require (prefix-in coll: data/collection))
(provide (except-out (all-defined-out) line circle))

(module+ test (require rackunit))

;; starting on f is nice for computations
(define circle (coll:cycle '(f c g d a e b)))
(define rev-circle (coll:cycle '(b e a d g c f)))
(define line (coll:cycle '(c d e f g a b)))

;; FIXME jagen unsafe (invalid pitch causes infinite loop!! :O )
(define (distance-above-c pitch) (coll:index-of line pitch))

(define (transpose-by-interval pitch accidental num type)
  (define line* (coll:drop (coll:index-of line pitch) line))
  (define pitch* (coll:first (coll:drop (sub1 num) line*)))
  ;; FIXME jagen maybe a better way?
  ;; currently the algorithm produces P1 M2 M3 A4 P5 M6 M7
  (define ix-of-pitch (coll:index-of circle pitch))
  (define ix-of-pitch* (coll:index-of circle pitch*))
  (define accidental* (if (< ix-of-pitch* ix-of-pitch) (add1 accidental) accidental))

  (define num* (add1 (modulo (sub1 num) 7)))

  (define accidental** (+ accidental* 
    (match type 
      [(or 'perfect 'P) (if (= num* 4) -1 0)] 
      [(or 'major 'M) 0] [(or 'minor 'm) -1] 
      [(or 'diminished 'dim) (if (or (= num* 1) (= num* 5)) -1 -2)] [(or 'augmented 'aug) (if (eq? num* 4) 0 1)])))
  (list pitch* accidental**))


(module+ test

  (define input-result-pairs
    '(((a 1 3 minor) . (c 1))
      ((f 0 4 perfect) . (b -1))
      ((f 0 4 augmented) . (b 0))
      ((a 0 3 major) . (c 1))
      ((f 0 5 augmented) . (c 1))
      ((g 0 5 perfect) . (d 0))
      ((c 0 4 perfect) . (f 0))
      ((d 0 3 major) . (f 1))
      ((c 0 3 major) . (e 0))
      ((c 0 6 major) . (a 0))
      ((c 0 7 major) . (b 0))
      ((d 0 7 major) . (c 1))

      ((e -1 1 perfect) . (e -1))
      ((e -1 2 major) . (f 0))
      ((e -1 3 minor) . (g -1))
      ((e -1 4 perfect) . (a -1))
      ((e -1 5 perfect) . (b -1))
      ((e -1 6 minor) . (c -1))
      ((e -1 7 minor) . (d -1))

      ((a 0 1 perfect) . (a 0))
      ((a 0 1 augmented) . (a 1))
      ((a 0 1 diminished) . (a -1))
      ((a 0 8 perfect) . (a 0))
      ((a 0 8 augmented) . (a 1))
      ((a 0 8 diminished) . (a -1))))

  (for-each (λ (x) (check-equal? (apply transpose-by-interval (car x)) (cdr x))) input-result-pairs))


#;(define (identify-interval p1 a1 p2 a2)
  (define interval (add1 (- (coll:index-of p2 line) (coll:index-of p1 line))))
  (define-values (index-of-p1 index-of-p2) (index-of p1 cycle) (index-of p2 cycle))
  (define type 
    (if (< index-of-p1 index-of-p2)
      (match* ((- a1 a2) interval)
        [(0 5) 'perfect]
        [(1 5)]
        [(0 4) 'augmented]
        [(0 6) 'minor]
        [(0 _ 'major)]))))

(define (note->integer n)
  (define semis (match (car n) ['c 0] ['d 2] ['e 4] ['f 5] ['g 7] ['a 9] ['b 11]))
  (+ 61 semis (cadr n) (* 12 (- (caddr n) 4))))

(define (generate-stack pitch accidental intervals types)
  (map transpose-by-interval 
    (build-list (length intervals) (λ(x) pitch))
    (build-list (length types) (λ(x) accidental))
    intervals
    types))

(module+ test
  (check-equal? (generate-stack 'c 0 '(3) '(major)) '((e 0))))

(define (generate-scale pitch accidental type)
  (define types
    (match type
      [(or 'M 'major) '(perfect major major perfect perfect major major)]
      [(or 'm 'minor) '(perfect major minor perfect perfect minor minor)]))
  (generate-stack pitch accidental (build-list 7 add1) types))

(module+ test
  (check-equal? (generate-scale 'a 0 'major) '((a 0) (b 0) (c 1) (d 0) (e 0) (f 1) (g 1)))
  (check-equal? (generate-scale 'e -1 'minor) '((e -1) (f 0) (g -1) (a -1) (b -1) (c -1) (d -1))))

(define (get-guide-tones pitch accidental modifiers)

  (define-syntax-rule (mod? x) (member x modifiers))

  (append
    (cond 
      [(mod? 'sus) '()]
      [(or (mod? 'major) (mod? 'M)) '((3 . major))]
      [(or (mod? 'minor) (mod? 'm) (mod? 'diminished) (mod? 'dim)) '((3 . minor))]
      [else (error 'get-guide-tones "no third provided!")])
    (cond 
      [(mod? 7) '((7 . minor))]
      [(mod? '(M 7)) '((7 . major))]
      [else '()])
    (if (mod? 4) '((4 . perfect)) '())
    ;; ... more to come
    ))


(define (generate-guide-tones pitch accidental modifiers)

  (define all (get-guide-tones pitch accidental modifiers))
  (generate-stack pitch accidental (map car all) (map cdr all)))

(define (generate-chord pitch accidental modifiers)

  ;; FIXME jagen dim 5
  (define all (sort (append `((1 . perfect) 
    (5 . ,(if (member 'diminished modifiers) 'diminished 'perfect))) (get-guide-tones pitch accidental modifiers)) < #:key car))
  (generate-stack pitch accidental (map car all) (map cdr all)))

(module+ test
  (check-equal? (generate-chord 'f 1 '(minor)) '((f 1) (a 0) (c 1)))
  (check-equal? (generate-chord 'a -1 '(diminished)) '((a -1) (c -1) (e -2)))
  (check-equal? (generate-chord 'g 1 '(sus 4)) '((g 1) (c 1) (d 1))))


#|
(define (generate-voice-leading chords hint-matrix prev)
  (cond
    [(null? chords) matrix*]
    [(car chords) => (λ (c) (generate-next-voice-leading c (car hint-matrix) prev))]))
|#
    
(define (note->semitone n)
  (match n
    [(list p a o)
     (define semis
       (match p
         ['c 0] ['d 2] ['e 4] ['f 5] ['g 7] ['a 9] ['b 11]))
         ;; arbitrarily use 61 (well, "arbitrarily" :P)
         (+ 61 semis a (* 12 (- o 4)))]))

(define (semitone-distance note1 note2)
  (- (note->semitone note2) (note->semitone note1)))

(module+ test
  (check-equal? (semitone-distance '(a 0 4) '(a 0 4)) 0)
  (check-equal? (semitone-distance '(a 0 3) '(c 1 5)) 16)
  (check-equal? (semitone-distance '(b 0 2) '(c 1 1)) -22))

(define (pc-semitone-distance note pc)
  (let/ec return
  (match* (pc note)
    [((list p a) (list p2 a2 o)) 
     ;; equal means 0 up 0 down
     (when (and (eq? p p2) (= a a2)) (return (list (cons note 0) (cons note 0))))
     ;; try the octave above, the same octave, and the octave below remove the one that is
     ;; more than an octave away.  Result will always be length 2 given the base case above ^^^
     (filter (λ (d) (< (abs (cdr d)) 12))
       (map (compose (λ (note*) (cons note* (semitone-distance note note*))) (λ (o) `(,p ,a ,o)))
            (list (add1 o) o (sub1 o))))])))

(define note-octave caddr)
(define (map-octave f note)
  `(,(car note) ,(cadr note) ,(f (caddr note))))

(module+ test
  (check-equal? (pc-semitone-distance '(a 0 4) '(a 0)) '(((a 0 4) . 0) ((a 0 4) . 0)))
  (check-equal? (pc-semitone-distance '(a 0 3) '(c 1)) '(((c 1 4) . 4) ((c 1 3) . -8))))

(define (lower-than n1 n2) (> (semitone-distance n1 n2) 0))
(define (higher-than n1 n2) (< (semitone-distance n1 n2) 0))

(define (pc->note pc o) (match pc [`(,p ,a) `(,p ,a ,o)]))
(define (note->pc n) (match n [`(,p ,a ,o) `(,p ,a)]))



(define (arpeggiate-span pcs bottom top)
  ;; `current` is equal to the "last" item in `pcs` (which is a cycle, so the last non-repeating)
  (define (arpeggiate-span pcs top acc)
    (cond [(higher-than (car acc) top) (reverse (cdr acc))]
          [else 
           (define current (car acc))
           (define curr-o (note-octave current))
           (define next (pc->note (coll:first pcs) (note-octave current)))
           ;; if next is closer to c then we've wrapped around and need to increase the octave
           (define next* 
             (if (<= (semitone-distance current `(c 0 ,curr-o))
                     (semitone-distance next `(c 0 ,curr-o)))
               (map-octave add1 next)
               next))
           (arpeggiate-span (coll:rest pcs) top (cons next* acc))]))

  ;; get the distances above for each note in chord
  (define-values (closest-note closest-index)
    (for/fold ([index #f] [note* #f] [minimum 12] #:result (values note* index)) 
              ([pc pcs] [i (in-naturals)])
      ;; find the closest pc in the chord _counting from beneath_
      (match-define (cons n* dis) (first (pc-semitone-distance bottom pc)))
      (if (< dis minimum)
          (values i n* dis)
          (values index note* minimum))))
  (arpeggiate-span (coll:drop (add1 closest-index) (coll:cycle pcs)) top (list closest-note)))

(define (arpeggiate-chord-span chord bottom top) 
  (arpeggiate-span (apply generate-chord chord) bottom top))

(module+ test
  (arpeggiate-chord-span '(c 0 (major)) '(c 0 3) '(c 0 4)) '((c 0 3) (e 0 3) (g 0 3) (c 0 4)))

(define (get-combinations ls)
  (cond 
    [(null? ls) '()]
    [(null? (cdr ls)) (map list (car ls))]
    [else 
      (define combinations-rest (get-combinations (cdr ls)))
      ;; FIXME jagen bad performance
      (for/foldr ([acc '()]) 
                 ([note (car ls)])
        (append (map (λ (comb) (cons note comb)) combinations-rest) acc))]))

(module+ test
  (check-equal? (list->set (get-combinations '(((c 0 4) (f 0 4)) ((c 0 3) (f 0 3)))))
                (list->set '(((c 0 4) (c 0 3)) ((f 0 4) (c 0 3)) ((c 0 4) (f 0 3)) ((f 0 4) (f 0 3))))))

(define (generate-next-voice-leading chord hints prev)
  (define scores
    (for/list ([pnote prev] [hint hints])
      (cond
        [hint (list (cons hint 13))]
        [else
         (match pnote
           [`(,p ,a ,o)
            ;; generate all notes in the chord octave below and above
            (define candidates (arpeggiate-chord-span chord `(,p ,a ,(sub1 o)) `(,p ,a ,(add1 o))))
            (map (λ (n) (cons n (- 12 (abs (semitone-distance n pnote))))) candidates)])])))
    
  (define possibilities (get-combinations scores))
  (define mandatory-tones (list->set (cons (list (car chord) (cadr chord)) (apply generate-guide-tones chord))))

  ;; just nix the ones that dont contain the guide tones for the chord
  (define possibilities*
    (filter 
      (λ (schord) 
        (subset? mandatory-tones (list->set (map (compose note->pc car) schord))))
      possibilities))

  (define options
    (map (λ (leading)
      (define leading* (map car leading))
      (define score (for/sum ([score (map cdr leading)]) score))
      (cons leading* score))
    possibilities*))
  
  (car (argmax cdr options)))

(module+ test
  (generate-next-voice-leading '(f 0 (major)) '(#f #f #f (f 0 3)) '((c 0 5) (g 0 4) (e 0 4) (c 0 3))))

(define (generate-voice-leading chords hints)
  (define fst (car hints))
  (unless (andmap (λ(x) x) fst) (error 'generate-voice-leading "requires a starting chord"))
  (for/fold ([acc (list fst)] #:result (reverse acc))
            ([chord (cdr chords)] [hint (cdr hints)])
    (define result (generate-next-voice-leading chord hint (car acc)))
    (cons result acc)))

(module+ test
  (generate-voice-leading 
    '((c 0 (major)) (f 0 (major))) 
    '(((c 0 5) (g 0 4) (e 0 4) (c 0 3)) (#f #f #f #f)))
    
  (generate-voice-leading 
    '((c 1 (minor)) (a 0 (major)) (d 0 (major)) (g 1 (major)) (c 1 (minor)) (c 1 (minor)) (g 1 (major))) 
    '(((e 0 4) (c 1 4) (g 1 3)) (#f #f #f)(#f #f #f)(#f #f #f)(#f #f #f)(#f #f #f)(#f #f #f))))

(define (beats->seconds beats tempo)
  (* beats (/ 60 tempo)))

(module+ test
  (check-equal? (beats->seconds 6 120) 3))

(define (interval-intersection i1 i2)
  (match* (i1 i2)
    [((list s1 e1) (list s2 e2))
     (define maxs (max s1 s2))
     (define mine (min e1 e2))
     (and (< maxs mine) (list maxs mine))]))

(define (normalize-tempo-interval tempo-interval tempo interval)
  (match* (tempo-interval interval)
    [((list s1 e1) (list s2 e2))
     (define total-amount (- e1 s1))
     (match-define (list int-s int-e) (or (interval-intersection tempo-interval interval) '(0 0)))

     (define beats-before (if (< s1 s2) (- s2 s1) 0))
     (define beats-during (- int-e int-s))

     (define seconds-before (- (beats->seconds beats-before tempo) beats-before))
     (define seconds-during (- (beats->seconds beats-during tempo) beats-during))
     
     (list (+ s2 seconds-before) (+ e2 seconds-before seconds-during))]))

(module+ test
  (check-equal? (normalize-tempo-interval '(3 12) 180 '(6 18)) '(4 12)))
