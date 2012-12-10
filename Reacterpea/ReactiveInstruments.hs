module ReactiveInstruments where

import Numeric
import Data.Ratio
import Euterpea hiding (Event)
import SongExpression
import SOE
import Data.IORef
import Data.Maybe
import Data.List
import FRP_IO

-- _______________________________________________________ COMPILE TO NoteEvents

type NoteEvent = (Double, AbsPitch)
type NoteScript = ([NoteEvent],[NoteEvent])

tpm = 2
rTime :: Rational -> Double
rTime rat = (fromRat rat) * tpm

addTime :: Double -> NoteScript -> NoteScript
addTime dT (l1, l2) = (f l1, f l2) where
    f noteEvents = map (\(t,p) -> (dT + t, p)) noteEvents

offsetStarts :: NoteScript -> NoteScript
offsetStarts (l1, l2) = (l1, f l2) where
    f noteEvents = map (\(t,p) -> (t - 0.05, p)) noteEvents
    
combineListTuples :: ([a],[b]) -> ([a],[b]) -> ([a],[b])
combineListTuples (l1a,l1b) (l2a,l2b) = (l1a ++ l2a, l1b ++ l2b)

sortNoteScript :: NoteScript -> NoteScript
--sortNoteScript = error "sortNoteScript not implemented"
sortNoteScript (l1, l2) = (f l1, f l2) where
    f x = sortBy (\(t1,_) (t2,_) -> if t1 < t2
                                       then LT
                                       else if t1 == t2
                                               then EQ
                                               else GT
                  ) x

compilePart :: Part -> NoteScript
--compilePart = error "compilePart not implemented"
compilePart (_, part) = sortNoteScript $ offsetStarts $ f 0 part where
    f _ [] = ([],[])
    f c ((Meas m):ms) = combineListTuples (compileMeasure c m) (f (c+1) ms)
    f c ((PhAt p):ms) = f c ms --phrase [p] (f c ms)
    
compileMeasure :: Rational -> Measure -> NoteScript
--compileMeasure = error "compileMeasure not implemented"
compileMeasure t0 measure = addTime (rTime t0) $ foldl combineListTuples ([],[]) (map compilePhrase measure) --(rest t0) :+: (foldl (:=:) (rest 0) (map compilePhrase measure))

compilePhrase :: (Rational, Phrase) -> NoteScript
--compilePhrase = error "compilePhrase not implemented"
compilePhrase (t0, (pitches, notes)) = addTime (rTime t0) $ foldl combineListTuples ([],[]) (map compileNote (zip pitches notes))

compileNote :: (PitchInfo, Note) -> NoteScript
--compileNote = error "compileNote not implemented"
compileNote ((Ptch pitch), (t0, t1)) = ([(rTime t0, absPitch pitch)], [(rTime (t0+t1), absPitch pitch)])
compileNote ((Perc pSound), t) = compileNote (Ptch $ percToPitch pSound,t)
compileNote ((Chrd chord), n@(t0, t1)) = foldl combineListTuples ([],[]) (map (\pitch -> compileNote (Ptch pitch, n)) chord)

percToPitch :: PercussionSound -> Pitch
percToPitch BassDrum1 = (B, 3)
percToPitch ClosedHiHat = (Bf, 3)
percToPitch _ = (C, 3)
-- _______________________________________________________ INSTRUMENTS

noteSign :: BC -> BP2 -> (Event a, Event b) -> BG
noteSign bc point (onEvent,offEvent) = ((switch black ((onEvent -=> bc) .|. (offEvent -=> black))) &* el point (point + (p2 35 35)))

rInstrument :: BC -> BP2 -> NoteScript -> BG
--rInstrument = error "rInstrument not implemented"
rInstrument bc point (ons, offs) = let idxs = [0..11]
                                       fltr idx x = x `mod` 12 == idx
                                       noteSigns point = map (\idx -> noteSign bc (point + (p2 0 (lift0 (idx*40) ))) (filterE (script ons) (fltr $ floor idx),
                                                                                                                      filterE (script offs) (fltr $ floor idx)) ) idxs in
    foldl1 ($$) (noteSigns point)

--test1 = animate $ return $ rInstrument (p2 10 10) ([(0,0), (0,2), (0,3)],[])
--test2 = animate $ return $ 
--            (rInstrument blue (p2 10 10) $ compilePart pianoPart) $$
--            (rInstrument green (p2 70 10) $ compilePart bassPart) $$
--            (rInstrument red (p2 130 10) $ compilePart drumPart)