#lang racket

(require tonart tonart/private/electronic/chuck/lib tonart/private/common-practice/transform)

(define-art-rewriter prelude
  (λ (stx)
    #'(context
    (sound-map 
      [ding . "/Users/jagen31/git/tonart-compositions/compositions/chuck/ding.wav"]
      [left . "/Users/jagen31/git/tonart-compositions/compositions/chuck/low-bongo.wav"]
      [fart . "/Users/jagen31/git/tonart-compositions/compositions/chuck/fart.aifc"]
      [right . "/Users/jagen31/git/tonart-compositions/compositions/chuck/high-bongo.wav"]
      [loud . "/Users/jagen31/git/tonart-compositions/compositions/chuck/loud.aifc"]
      [mrmr . "/Users/jagen31/git/tonart-compositions/compositions/chuck/mrmr.aifc"]
      [silly . "/Users/jagen31/git/tonart-compositions/compositions/chuck/.aifc"]
      [tooloud . "/Users/jagen31/git/tonart-compositions/compositions/chuck/tooloud.aifc"])
    (voice@ (melody) (octave 6) (volume 8))
    (voice@ (chord) (octave 4) (volume 3))
    (tuning 12tet))))

(define-art-rewriter postlude 
  (λ (stx) 
    #'(context (run-transforms) (apply-rhythm) (chord->notes/simple 4) (^->note) 
               (note->tone) (tone->full-tone) #;(tempo 120) (apply-tempo) (d/dt))))
