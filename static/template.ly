#(define sn 0)

#(define (score-number)
  (set! sn (1+ sn))
  (string-append (number->string sn)))  % "."

\header {
  tagline = ""
}

\layout {
  \context {
    \Score
    defaultBarType = ""
    \remove "Bar_number_engraver"
  }
  \context {
    \Staff
    \remove Time_signature_engraver
    \accidentalStyle no-reset
    \override InstrumentName #'font-size = 3
    \override InstrumentName #'font-series = #'bold
  }
}