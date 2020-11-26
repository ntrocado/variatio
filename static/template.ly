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
  }
  \context {
    \Staff
    \remove
    Time_signature_engraver
  }
}