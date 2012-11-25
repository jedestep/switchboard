module SongCompilation where
import Euterpea
import SongExpression

compileSong :: Song -> Music Pitch
--compileSong = error "compileSong not implemented"
compileSong song = f 0 song where
    f _ [] = rest 0
    f c (m:ms) = (compileMeasure c m) :=: (f (c+1) ms)

compileMeasure :: Rational -> Measure -> Music Pitch
--compileMeasure = error "compileMeasure not implemented"
compileMeasure t0 measure = (rest t0) :+: (foldl (:=:) (rest 0) (map compilePhrase measure))

compilePhrase :: (Rational, Phrase) -> Music Pitch
--compilePhrase = error "compilePhrase not implemented"
compilePhrase (t0, (pitches, notes)) = (rest t0) :+: (foldl (:=:) (rest 0) (map compileNote (zip pitches notes)))

compileNote :: (Pitch, Note) -> Music Pitch
--compileNote = error "compileNote not implemented"
compileNote (pitch, (t0, t1)) = (rest t0) :+: (note t1 pitch)

-- EXAMPLES __________________________________________________________________________________________________________

-- EXAMPLE 1: test compileNote
eg1 = compileNote ((C, 4), (1/4,1/4))

-- EXAMPLE 2: test compilePhrase
eg2 = compilePhrase (0, phrase1)

-- EXAMPLE 3 & 4: test compileMeasure
eg3 = compileMeasure 0 measure1
eg4 = compileMeasure 0 measure2

-- EXAMPLE 5: test compileSong
eg5 = compileSong song1