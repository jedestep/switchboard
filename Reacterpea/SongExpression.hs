module SongExpression where
import Euterpea

-- The timing information for a single note. Start time and duration
type Note = (Rational, Rational)

-- The idea of these is that the lists can be zipped together, rhythm zipped with a list of pitches
data PitchInfo = Ptch Pitch
               | Perc PercussionSound
               | Chrd [Pitch]

type PitchSequence = [PitchInfo]
type NoteSequence = [Note]

transPitchSequence :: PitchSequence -> AbsPitch -> PitchSequence
transPitchSequence pSeq aP = map (f aP) pSeq where
    f :: AbsPitch -> PitchInfo -> PitchInfo
    f aP (Ptch p) = Ptch (pitch $ (absPitch p) + aP)
    f aP (Chrd c) = Chrd (map (\p -> pitch $ (absPitch p) + aP) c)

-- A phrase is a rhythm of notes to play and the list of pitches to zip to the notes in the phrase
type Phrase = (PitchSequence, NoteSequence)

transPhrase :: Phrase -> AbsPitch -> Phrase
transPhrase (pSeq,nSeq) aP = (transPitchSequence pSeq aP, nSeq)

-- A measure is a list of phrases that start during the measure, paired with their start time (fractional of the measure)
type Measure = [(Rational, Phrase)]

data PartItem = Meas Measure | PhAt PhraseAttribute

-- A part is a list of measures to be played in time after one another
type Part = (InstrumentName,[PartItem])

-- A song is a tempo scale and a list of parts
type Song = (Rational, [Part])

-- Common PhraseAttributes
piano, mezzoPiano, mezzoForte, forte :: PhraseAttribute
piano = Dyn $ StdLoudness P
mezzoPiano = Dyn $ StdLoudness MP
mezzoForte = Dyn $ StdLoudness MF
forte = Dyn $ StdLoudness NF

-- EXAMPLES __________________________________________________________________________________________________________

-- EXAMPLE 1: phrase1 is an arpeggio over four beats, for 1 measure
pitches1 :: PitchSequence
pitches1 = cycle [Ptch (G,4),Ptch (B,4),Ptch (D,5),Ptch (G,5)]

notes1 :: NoteSequence
notes1 = [(0,1/4),(1/4,1/4),(2/4,1/4),(3/4,1/4)]

phrase1 :: Phrase
phrase1 = (pitches1,notes1)

-- EXAMPLE 2: measure1 and measure2, when stitched together, will play the arpeggio from before three times in cannon
measure1 :: PartItem
measure1 = Meas [(0,phrase1), (2/4,phrase1)]

measure2 :: PartItem
measure2 = Meas [(0,phrase1)]

-- EXAMPLE 3: song1 is composition of measures 1, 2, and 3
measure3 :: PartItem
measure3 = Meas []

part1 :: Part
part1 =  (AcousticGrandPiano, take 6 (cycle [measure1, measure2, measure3]))

part2 :: Part
part2 =  (PanFlute, take 6 (cycle [measure1, measure2, measure3]))