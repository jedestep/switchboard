module SongExpression where
import Euterpea

-- The timing information for a single note. Start time and duration
type Note = (Rational, Rational)

-- The idea of these is that the lists can be zipped together, rhythm zipped with a list of pitches
type Pitches = [Pitch]
type Notes = [Note]

-- A phrase is a rhythm of notes to play and the list of pitches to zip to the notes in the phrase
type Phrase = (Pitches, Notes)

-- A measure is a list of phrases that start during the measure, paired with their start time (fractional of the measure)
type Measure = [(Rational, Phrase)]

-- A part is a list of measures to be played in time after one another
type Song = [Measure]

-- EXAMPLES __________________________________________________________________________________________________________

-- EXAMPLE 1: phrase1 is an arpeggio over four beats, for 1 measure
pitches1 :: Pitches
pitches1 = [(G,4),(B,4),(D,5),(G,5)]

notes1 :: Notes
notes1 = [(0,1/4),(1/4,1/4),(2/4,1/4),(3/4,1/4)]

phrase1 :: Phrase
phrase1 = (pitches1,notes1)

-- EXAMPLE 2: measure1 and measure2, when stitched together, will play the arpeggio from before three times in cannon
measure1 :: Measure
measure1 = [(0,phrase1), (2/4,phrase1)]

measure2 :: Measure
measure2 = [(0,phrase1)]

-- EXAMPLE 3: song1 is composition of measures 1, 2, and 3
measure3 :: Measure
measure3 = []

song1 :: Song
song1 = take 6 (cycle [measure1, measure2, measure3])