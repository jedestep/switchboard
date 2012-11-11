<CsoundSynthesizer>

  <CsOptions>
    csound -W -d -o tone.wav 
  </CsOptions>
  
  <CsInstruments>
    sr     = 96000           ; Sample rate.
    kr     = 9600            ; Control signal rate.
    ksmps  = 10              ; Samples per control signal.
    nchnls = 1               ; Number of output channels.
    
        instr 1 
    a1     oscil p4, p5, 1   ; Test derp.
    out a1                   ; Output.
    endin
  </CsInstruments>
 
</CsoundSynthesizer>