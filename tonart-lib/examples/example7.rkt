#lang racket

(require art art/sequence/lib art/timeline/lib
         "../rewriter/stdlib.rkt" "../rewriter/common-practice/lib.rkt" 
         "../realizer/electronic/lib.rkt" 
         "../realizer/electronic/linuxsampler/lib.rkt"
         "../realizer/visual/musicxml/lib.rkt"
         "../realizer/electronic/rsound/lib.rkt"
         "../rewriter/church/hymn.rkt"
  rsound (for-syntax syntax/parse))

(set-output-device! 1)

(define-interpretation winter-wonderland)

(interpretation+ winter-wonderland
  [sb-rhythm (rhythm 2/3 1/3 3 2/3 1/3 1 2)]
  [sleigh-bells (seq (notes (c 0 5) (c 0 5) (c 0 5) (c 0 5) (c 0 5) (a 0 4) (c 0 5)))]
  [in-the-lane (seq (notes (c 0 5) (c 0 5) (c 0 5) (c 0 5) (c 0 5) (b -1 4) (c 0 5)))]
  [phrase1
    (seq
      (ix-- (sleigh-bells) (in-the-lane)) ; (@ [index] winter-wonderland)
      (ix-loop 2 (sb-rhythm)) (expand-ix-loop) ; (@ [index] winter-wonderland)
      (interpret winter-wonderland) ; (@ [index] (seq (@ [index] note)) rhythm)
      (apply-rhythm) ; (@ [index(2 dimensions), interval] note)
      #;(split music) ; (@ [index(1 dim)] (music (@ [index(1 dim), interval] note)))
    )
     ; (@ [interval] note)
  ])

(interpretation+ winter-wonderland
  [fu-rhythm (rhythm 0.5 0.5 0.5 0.5 2.5)]
  [ww-rhythm (rhythm 0.5 0.5 0.5 0.5)]
  [face-unafraid (seq (notes (c 0 5) (e 0 5) (e 0 5) (e 0 5) (d 0 5)))]
  [plans-that (seq (notes (d 0 5) (c 0 5) (c 0 5) (c 0 5) (b -1 4)))]
  [walkin (seq (notes (a 0 4) (a 0 4) (a 0 4) (a 0 4)))]
  [winter (seq (notes (g 0 4) (g 0 4) (g 0 4) (g 0 4)))]
  [land (seq (notes (f 0 4)))]
  [phrase2
    (seq
      (ix-- (face-unafraid) (plans-that) (walkin) (winter) (land))
      (ix-loop [0 2] (fu-rhythm)) (ix-loop [2 4] (ww-rhythm)) (ix@ 4 (rhythm 3)) (expand-ix-loop) 
      (interpret winter-wonderland) (apply-rhythm) 
      (split music))
    (inline-music-seq)])


(set-output-device! 1)

(perform (quote-performer)
  
  (phrase1)
  (interpret winter-wonderland)
  #;(inline-music-seq))


#;(define file (open-output-file "tonart-lib/realizer/electronic/linuxsampler/.test/test.cpp" 
                                   #:exists 'replace))
#;(displayln sound file)
#;(close-output-port file)