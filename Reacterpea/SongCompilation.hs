module SongCompilation where
import Euterpea
import SongExpression

compileSong :: Song -> Music Pitch
compileSong (tempoScale, parts) = tempo tempoScale $ foldl (:=:) (rest 0) (map compilePart parts)

compilePart :: Part -> Music Pitch
--compilePart = error "compilePart not implemented"
compilePart (iName, part) = instrument iName (f 0 part) where
    f _ [] = rest 0
    f c ((Meas m):ms) = (compileMeasure c m) :=: (f (c+1) ms)
    f c ((PhAt p):ms) = phrase [p] (f c ms)

compileMeasure :: Rational -> Measure -> Music Pitch
--compileMeasure = error "compileMeasure not implemented"
compileMeasure t0 measure = (rest t0) :+: (foldl (:=:) (rest 0) (map compilePhrase measure))

compilePhrase :: (Rational, Phrase) -> Music Pitch
--compilePhrase = error "compilePhrase not implemented"
compilePhrase (t0, (pitches, notes)) = (rest t0) :+: (foldl (:=:) (rest 0) (map compileNote (zip pitches notes)))

compileNote :: (PitchInfo, Note) -> Music Pitch
--compileNote = error "compileNote not implemented"
compileNote ((Ptch pitch), (t0, t1)) = (rest t0) :+: (note t1 pitch)
compileNote ((Perc pSound), (t0, t1)) = instrument Percussion $ (rest t0) :+: (perc pSound t1)
compileNote ((Chrd chord), (t0, t1)) = (rest t0) :+: (foldl (:=:) (rest 0) (map (\ pitch -> (note t1 pitch)) chord))

-- EXAMPLES __________________________________________________________________________________________________________

-- EXAMPLE 1: test compileNote
eg1a = compileNote (Ptch (C, 4), (1/4,1/4))
eg1b = compileNote (Chrd [(C, 4),(E, 4),(G, 4)], (1/4,1/4))

-- EXAMPLE 2: test compilePhrase
eg2 = compilePhrase (0, phrase1)

-- EXAMPLE 3 & 4: test compileMeasure
eg3 = compileMeasure 0 m where
    Meas m = measure1
eg4 = compileMeasure 0 m where
    Meas m = measure2

-- EXAMPLE 5: test compileSong
eg5 = compilePart part1
eg6 = compilePart part2

test1, test2 :: Music Pitch
test1 = note (1/2) (C, 4)
test2 = instrument PanFlute $ (phrase [forte] test1) :+: (phrase [piano] test1) :+: (phrase [piano] (phrase [forte] test1))