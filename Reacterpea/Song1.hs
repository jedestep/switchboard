import Data.Ratio
import Euterpea
import SongExpression
import SongCompilation

numMeasures = 12

beat1 = 0
beat2 = qn
beat3 = hn
beat4 = hn+qn

-- _________________________________________ Percussion Part

hatClosed = Perc ClosedHiHat
bassDrum = Perc BassDrum1

hatPitchSeq = cycle [hatClosed]
hatNoteSeq = [(beat1,en),(beat1+1/6,en)
             ,(beat2,en),(beat2+1/6,en)
             ,(beat3+1/6,en)
             ,(beat4,en),(beat4+1/6,en)
             ]

bassDrumPitchSeq = cycle [bassDrum]
bassDrumNoteSeq = [(beat1,qn)
                  ,(beat2+1/6,qn)
                  ]
             
hatPhrase = (hatPitchSeq,hatNoteSeq)
bassDrumPhrase = (bassDrumPitchSeq,bassDrumNoteSeq)

drumMeasure = [(0,hatPhrase)
              ,(0,bassDrumPhrase)
              ]
              
drumPart = (Percussion, take numMeasures $ repeat (Meas drumMeasure))
              
drum = compilePart drumPart
-- _________________________________________ Bass Part

bassPitchSeq1 = [Ptch (E,3), Ptch (Af,3), Ptch (B,3), Ptch (Cs,4), Ptch (D,4), Ptch (Cs,4), Ptch (B,3), Ptch (Af,3)]
bassPitchSeq2 = [Ptch (B,3), Ptch (B,3), Ptch (Cs,4), Ptch (B,3), Ptch (D,4), Ptch (Cs,4), Ptch (B,3), Ptch (Af,3)] 
bassNoteSeq = [(0*qn,qn),(1*qn,qn),(2*qn,qn),(3*qn,qn),(4*qn,qn),(5*qn,qn),(6*qn,qn),(7*qn,qn)]

bassPhraseI = transPhrase (bassPitchSeq1,bassNoteSeq) (-12)
bassPhraseIV = transPhrase bassPhraseI 5
bassPhraseV = transPhrase (bassPitchSeq2,bassNoteSeq) (-12)

bassMeasureI = [(0,bassPhraseI)]
bassMeasureIV = [(0,bassPhraseIV)]
bassMeasureV = [(0,bassPhraseV)]

bassPart = (SlapBass2,
            [Meas bassMeasureI
            ,Meas []
            ,Meas bassMeasureI
            ,Meas[]
            ,Meas bassMeasureIV
            ,Meas[]
            ,Meas bassMeasureI
            ,Meas[]
            ,Meas bassMeasureV
            ,Meas[]
            ,Meas bassMeasureI
            ,Meas[]
            ]
           )

-- _________________________________________ Piano Part

pianoPitchSeqI = [Chrd [(E,4),(Af,4),(B,4)]
                 ,Ptch (Cs,5)
                 ,Chrd [(E,4),(Af,4),(B,4)]
                 ,Ptch (D,5)
                 ,Ptch (Cs,5)
                 ]

pianoPitchSeqV = [Chrd [(B,4),(Ef,5),(Gf,5)]
                 ,Ptch (Af,5)
                 ]
                 
pianoNoteSeq = [(beat1,wn), (beat2+en,hn+en)
               ,(1+beat1,wn), (1+beat2+en,qn+en), (1+beat4,qn)
               ]

pianoNoteSeq2 = [(beat1,wn), (beat2+en,hn+en)]
               
pianoPhraseI = (pianoPitchSeqI,pianoNoteSeq)
pianoPhraseIV = transPhrase pianoPhraseI 5
pianoPhraseV = (pianoPitchSeqV, pianoNoteSeq2)
pianoPhraseIV2 = transPhrase pianoPhraseV (-2)
                
pianoMeasureI = [(0,pianoPhraseI)]
pianoMeasureIV = [(0,pianoPhraseIV)]
pianoMeasureV = [(0,pianoPhraseV)]
pianoMeasureIV2 = [(0,pianoPhraseIV2)]

pianoPart = (AcousticGrandPiano,
             [Meas pianoMeasureI
             ,Meas []
             ,Meas pianoMeasureI
             ,Meas []
             ,Meas pianoMeasureIV
             ,Meas []
             ,Meas pianoMeasureI
             ,Meas []
             ,Meas pianoMeasureV
             ,Meas pianoMeasureIV2
             ,Meas pianoMeasureI
             ,Meas []
             ]
            )

song1 = (5%4,[drumPart,bassPart,pianoPart])