#lang scribble/doc

@require[(except-in scribble/manual index)
         scribble-abbrevs/manual (for-label tonart)]

@title{Core Library}

Tonart has a library of objects, coordinates, contexts, rewriters, and realizers which will be
useful for manipulating music.  It also benefits from @code{art} libraries.

@table-of-contents[]

@(require scribble/eval)
 
@(define helper-eval (make-base-eval))
@interaction-eval[#:eval helper-eval
                  (require art art/sequence/ravel)]

@section{Music Contexts}

Music is a context equipped with an interval coordinate, and a voice coordinate.  

Tonart uses the standard library @code{interval} from @code{timeline}.

@code{voice} is a @code{superset} coordinate.  This models the concept of musical @code{Voice}.

@defform[(voice@ id expr ...)]
@defform[(voice@ (id ...) expr ...)]

A good way to
visualize it is like a sheet music, with the intervals rolling along horizontally like measures, and each voice laid
out vertically as staves.

One thing to note is supplying a set of multiple voices for voice-at represents a region containing all those voices.

There is currently no corresponding construct to represent multiple intervals, but it's not a terrible idea.

@defform[(music expr ...)]
@defform[(rewrite-in-music expr ...)]

@defform[(m! id)]
Inline the contents of an art binding which is a music context into this context.

@section{Standard Objects}

@defform[(key root accidental modality)]
A musical key with numeric accidental. See @code{Keys}

@defform[(timesig numerator denominator)]
A time signature. Time signatures are used in computing metric intervals, and in music involving measures.
See @code{Time Signature}

@defform[(tempo number)]
Indicates a given area is at a certain tempo.  See @code{Time Signature}

@defform[(instrument name)]
An instrument.  Instruments may be free instrument names, or, for example, a mapping into a midi bank, depending on
what rewriter/realizer is interpreting it.

@defform[(pitch-class symbol)]
A pitch class. See @code{Notes and Pitch Classes}
@defform[(octave number)]
An octave indication, by itself.  Octaves are conventionally interpreted relative to C.  However, some 
transformations use the tonic of the current key as a basis for the octave system, which often makes sense in practice.
@defform[(note pitch accidental octave)]
A note, with numeric accidental.  See @code{Notes and Pitch Classes}

@defform[(music-rest)]

@defform[(^ index)]
A scale degree.  Generally to be interpreted within a key.
@defform[(^o index octave)]
A scale degree, with octave specified context-free.  Generally to be interpreted within a key.

@defform[(measure expr ...)]
Embedded context representing a measure

@defform[(chord root accidental [mode/extension ...])]
A musical chord with numeric accidental.
@defform[(voiced-chord root accidental octave [(p a o) ...])]
A voiced chord.  Chords may be partially voiced, missing voices being indicated with @code{_}.

@defform[(fill-harmony)]
Voice the chord in context with the sequence of notes in context
@defform[(fill-voice n)]
Voice the nth voice of the chord in context with the note in context for that chord
@defform[(voice-lead n)]
Voice lead the voiced-chords in context with n voices.  The first chord must be fully-voiced, provide
partial voicings to give hints to the voice leader.
 
@section{Electronic}

@defform[(tone number)]
A tone, in hz.
@defform[(midi number)]
A midi note number.
@defform[(full-midi number velocity instrument)]
A context-free midi note event, complete with number, velocity, and instrument.
@defform[(instrument-map mapping)]
Holds a syntactic assoc of instrument names to soundfont presets

@defform[(load-musicxml filename)]
Rewriter which loads the given file into MusicXML art.  Keep in mind that
the file must be present during compilation, but does not need to be present
during runtime.

@defform[(mxml-note pitch accidental octave)]
@defform[(mxml-measure expr ...)]
Embedded context representing a musicxml measure

@section{Sequence Shorthands}

@defform[(notes expr ...)]
@defform[(^s expr ...)]
@defform[(chords expr ...)]
@defform[(tones expr ...)]
Indexed series of the corresponding objects.

@section{Conversions}

@defform[(^->note)]
@defform[(^->pitch)]
@defform[(^o->note)]
@defform[(note->^o)]
@defform[(note->midi)]
@defform[(note->tone)]
@defform[(measure->music)]
@defform[(measure->mxml-measure)]
@defform[(mxml-measure->measure)]
@defform[(rest->mxml-rest)]
@defform[(mxml-rest->rest)]
@defform[(chord->voiced-chord)]

Converters between the corresponding objects.

@section{Additional Rewriters}

@defform[(inline-music-seq)]
Take a sequence of music contexts and inline them into this context, occuring one after the other in time.  
This is a way to append musics of unknown lengths, as opposed to @code{--}, which requires known lengths
for each segment ahead of time.

@defform[(transpose-diatonic* number)]
Immediately run a diatonic transpose.
@defform[(transpose-diatonic number)]
Reification of a diatonic transposition (this is an object).
@defform[(apply-transpose-diatonic number)]
Apply the diatonic transposes.

@defform[(apply-tempo)]
Apply a tempo by dilating the intervals relative to 60 bpm.  Essentially, treat the intervals
as if they represent durations in seconds.
@defform[(tempo* number)]
Immediately apply a tempo.

@section{Realizers}

@defform[(music-rsound-realizer)]
Realize into an rsound.
@defform[(linuxsampler-realizer)]
Realize into text for a c++ file that can be linked against linuxsampler
@defform[(draw-music-realizer)]
Run the draw realizer, treating the art as toplevel music
