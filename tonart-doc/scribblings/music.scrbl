#lang scribble/doc

@require[(except-in scribble/manual index)
         scribble-abbrevs/manual (for-label tonart)]

@title{Music Reference}


@table-of-contents[]

@(require scribble/eval)

@section{Notes and Pitch Classes}

Notes are a standard set of symbols denoting a tone of a certain frequency.  First, understand that musical pitch
"repeats" every time you double it.  This is a perpection of the human ear, wherein there are no beats between
the two notes (since their frequency ratio is 2:1).  Thus the notes, though one is higher and one is lower, sound
"the same" in some sense.  

Notes are specified in terms of a symbol, an accidental, and an octave.  

The note symbols are C D E F G A B.  The next symbol is "C" again, and this "C" is exactly double in pitch from the
first one (FIXME jagen mention alpha tuning in margin note).  Starting on C is just an extremely useful convention.
Because we've gone through eight notes, we call this span from C to C an "octave".  The notes have labeled 7 distinct positions 
within that octave.

Accidentals are # and b, read "sharp" and "flat", to indicate the note should be raised in pitch or lowered in pitch.  
In practice this means raise or lower the note one "semitone".  "Semitone" is a further concept of dividing the octave into
12 parts.  Note symbols point to one of these 12 spots.  A # moves it up one spot.  A b moves it down.  A ## moves it up two spots, etc.
The pointers for the symbols are:

C D E F G A  B

1 3 5 6 8 10 12

Thus in the standard system, E# is the same semitone as F, and Cb is the same semitone as B.  Same for C# and Db.
But, sharps and flats usually encode contextual information, and thus C# and Db should not be substituted for one another
freely.  This type of equivalence is called "enharmonic equivalence".  Changing between enharmonically equivalent notes
is called "respelling" or "respelling pitches".

A pitch class is a note without an octave specified.  
It is an abstraction over notes, i.e. denotes a set of notes.

@subsection{Keys, Scales, and Degrees}

A sequential group of notes, as C D E F G A B, is called a "scale".  Common scales have names:


C D E F G A B - Major, or Ionian

D E F G A B C - Dorian Minor, or Dorian

E F G A B C D - Phrygian Minor, or Phrygian

F G A B C D E - Lydian Major, or Lydian

G A B C D E F - Mixolydian Major, or Mixolydian

A B C D E F G - Natural Minor, or Aeolian

B C D E F G A - Diminshed, or Locrian

D E F G A Bb C# - Harmonic Minor


Note: there is something commonly presented as a "scale" called melodic minor:
D E F G A B C# D C Bb A G F E
or
D E F G A B(b/nat) C(nat/#)


@subsection{Chords}

Chords represent sets of notes.  Chords can be unvoiced or voiced.  Unvoiced
chords are just called chords and represent all octaves of the notes they
contain.  Voiced chords have a finite set of notes with octaves specified.  The
arrangement of the notes in the voiced chord is called a "voicing". A voiced
chord with n notes is called an n-voice chord.  It could be said that a chord is
a set of pitches, while a voiced chord is a set of notes.  Or, if both are viewed as sets of
notes, the unvoiced chord is an infinite set and the voiced chord is a finite set.

The relationship between consecutive voiced chords is referred to as a "voice leading".  Voice leading
is an art unto itself.  A voice leading over chords of n voices is called an "n voice leading" ("voice-" is elided).
Sometimes voice leading is a primary concern.  Jazz piano, for example, requires high skill in this area.
Guitar, on the other hand, is far less concerned with voice leading due to physical limitations of the instrument.

Chords are not typically notated by spelling out the exact pitches in the chord, but rather by giving the chord's
"root", "modality" and any "extensions".  A chord's root is the foundation of the chord.  Everything else is interpreted
relative to the root.  The root is strongest in the bass, and that is where it will be by default unless otherwise specified.
Chords in this library can be ascribed "Major", "Minor", "diminshed", "augments, or "suspended" modalities.  
Major indicates a major third above the root.  Minor indicates a minor third over the root.  Diminished indicates a
minor third and a diminshed fifth above the root.  Augmented indicates a major third and an augmented fifth above the root.
Suspended indicates a missing third, and is more the absence of modality
than a real modality.  The implication is, "here is a chord which is not yet a chord, but will become one in the next chord".
It is not always used this way, which is just an artifact from many harmonies not being represented in standard
chord theory.  When using chords as a music construction tool and not a performable notation, Sus can come in handy, in nonstandard places.



@section{MusicXML}

MusicXML is an exchange format for music.  Score editors can usually
export and import MusicXML files.  Tonart provides an art representation
of MusicXML, but very few transforms, intended as an intermediate
representation to ease compiling.  Nonetheless, we'll document the structure
of a valid MusicXML art, in case someone wants to do some rewriting
at that level, or in case it is useful in debugging.
