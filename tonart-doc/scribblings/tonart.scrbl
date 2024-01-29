#lang scribble/doc

@require[scribble/manual
         scribble-abbrevs/manual]

@title{Tonart: Composable Compositions}

@defmodule[tonart]

An extensible markup language and meta-language, for creating and manipulating music.

@table-of-contents[]

@(require scribble/eval)
 
@(define helper-eval (make-base-eval))
@interaction-eval[#:eval helper-eval
                  (require tonart)]
@examples[
  #:eval helper-eval

  (define-art m 
    (voice@ (melody) (seq (^s 3 4 5)) (rhythm 2 2 4)))

  (realize (draw-realizer [800 200]) (music m))
   
  (define-art a
    (voice@ (accomp) (seq (^s 1 -2 1)) (rhythm 3 1 4)))
  
  (realize (draw-realizer [800 200]) (music a))

  (realize (draw-realizer [800 200]) (music m a))

  (realize (draw-realizer [800 200]) (music m a (apply-rhythm)))

  (define-art comp 
    m a (apply-rhythm) 
    (key b -1 major) 
    (voice@ (accomp) (octave 3)) (voice@ (melody) (octave 4)) (^->note))

  (realize (draw-realizer [800 200]) (music comp))
] 