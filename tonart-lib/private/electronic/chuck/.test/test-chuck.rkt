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
  (i@ [0 24] song (expand-loop))
  (-- [12 trash-sound-map] [12 real-sound-map]))

(qr out (merge-sound-maps) (d/dt))
(displayln (realize (music-chuck-realizer) out (merge-sound-maps) (d/dt)) file)

(close-output-port file)
