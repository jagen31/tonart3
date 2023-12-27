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
     (realize (draw-realizer [800 800]) (music (i@ [0 4] (note a 0 4))))
   ]}