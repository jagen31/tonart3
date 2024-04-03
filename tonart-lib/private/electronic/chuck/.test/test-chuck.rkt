#lang racket

(require tonart)

(define file (open-output-file "test.ck" #:exists 'replace))

(define-art trash-sound-map
  (sound-map [ding . "sounds/ding.wav"] [left . "sounds/left.wav"] [right . "sounds/right.wav"]))

(define-art real-sound-map 
  (sound-map [ding . "sounds/splash.wav"] [left . "sounds/low-bongo.wav"] [right . "sounds/high-bongo.wav"]))

(define-art song
  (loop 4 (sound ding)) (loop 3 (sound left)) (loop 2 (sound right)))

(define-art out 
  (st-flavian) (st-flavian->tune) (key c 0 minor) (octave 4) (^->note) (tuning 12tet) (note->tone) 
  (i@ [0 32] song (expand-loop))
  (-- [16 trash-sound-map] [16 real-sound-map])
  (dilate 0.5))

(qr out (merge-sound-maps) (d/dt))
(displayln (realize (music-chuck-realizer) out (merge-sound-maps) (d/dt)) file)

(close-output-port file)
