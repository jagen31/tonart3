#lang racket

(require tonart)


(realize (music-chuck-live-realizer #:advance 10)
  (sound-map [ding . "ding.wav"])
  
  (i@ [0 4] (sound ding))
  (d/dt))