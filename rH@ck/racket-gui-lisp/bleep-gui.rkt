#lang racket
(require racket/gui)
(require rsound)

;;; One of the strings of rkt is the number of built-in libs. 2-edge sword for me.
;;; So Racket was a pretty understandable example. It didn't work in the end because I couldn't get
; the RSound thingy to hook up. But since this is just so I can get a look at the different scheme
; GUIs, I'm not going to waste anymore time trying to get it to work. I've seen enough.

; Scale used by slider
(define *min-position* 0)
(define *max-position* 2000)
; Range of freqs
(define *min-frequency* 20)
(define *max-frequency* 20000)
; Notes -> frequency (middle A-G [A4-G4])
; http://pages.mtu.edu/~suits/notefreqs.html
(define notes (hash "A" 440.00
                    "B" 493.88
                    "C" 261.63
                    "D" 293.66
                    "E" 329.63
                    "F" 349.23
                    "G" 292.00))

; Generate a tone using RSound
; Explicitly set RSound sample rate in case differs by platform/version
(default-sample-rate 44100)
(define (generate-tone button event)
  (play (make-tone (string->number (send frequency-field get-value))
                    0.5
                    ; Duration in samples at sample rate of 44.1 kHz
                    (inexact->exact (* 44.1 (string->number (send duration-field get-value)))))))

; Logarithmic scale for frequency (so middle A [440] falls about in the middle)
; Adapted from https://stackoverflow.com/questions/846221/logarithmic-slider

(define min-freq (log *min-frequency*)) ;global params same as CL
(define max-freq (log *max-frequency*))
(define frequency-scale (/ (- max-freq min-freq) (- *max-position* *min-position*)))

; convert slider position to frequency
(define (position->frequency position)
  (inexact->exact (round (exp (+ min-freq (* frequency-scale (- position *min-position*)))))))

; Convert frequency to slider position
(define (frequency->position freq)
  (inexact->exact (round (/ (- (log freq) min-freq) (+ frequency-scale *min-position*)))))

; set frequency slider and display
(define (set-frequency freq)
  (send slider set-value (frequency->position freq))
  (send frequency-field set-value (~a freq)))


; Extend the text-field% to validate data when field loses focus. Field should contain only numbers
; within allowed range. Otherwise, set to min.
(define number-field%
  (class text-field%
    ; Add init vars to define allowed range
    (init min-value max-value)
    (define min-allowed min-value)
    (define max-allowed max-value)
    (super-new)
    (define/override (on-focus on?)
      (unless on?
        (define current-value (string->number (send this get-value)))
        (unless (and current-value
                     (>= current-value min-allowed)
                     (<= current-value max-allowed)) 
          (send this set-value (~a min-allowed))
          ; also reset slider position to make sure it still matches display
          (send slider set-value (string->number (send frequency-field get-value))))))))


; main window
(define frame (new frame% (label "Bleep")))

; Frequency Controls:

; Link slider to text field display of frequency
(define (adjust-frequency widget event)
  (send frequency-field set-value
    (~a (position->frequency (send widget get-value)))))

(define (adjust-slider entry event)
  (define new-freq (string->number (send entry get-value)))
  (send slider set-value
    (frequency->position (if new-freq new-freq *min-frequency*))))

; Buttons increase and decrease frequency by one octave
(define (adjust-octave modifier)
  (set-frequency (* (string->number (send frequency-field get-value)) modifier)))
(define (decrease-octave button event) (adjust-octave 0.5))
(define (increase-octave button event) (adjust-octave 2))


; slider with our new calculations using the constants
(define slider (new slider% (label #f)
                            (min-value *min-position*)
                            (max-value *max-position*)
                            (callback adjust-frequency)
                            (parent frame)
                            (init-value (frequency->position 440))
                            (style '(horizontal plain))
                            (vert-margin 25)
                            (horiz-margin 10)))

; a text field showing the current frequency and buttons to increase and decrease by an octave
(define frequency-pane (new horizontal-pane% (parent frame)
                                             (border 10)
                                             (alignment '(center center))))
(define lower-button (new button% (parent frequency-pane)
                                  (label "<")))
(define frequency-field (new number-field% (label #f)
                                        (parent frequency-pane)
                                        (min-value *min-frequency*)
                                        (max-value *max-frequency*)
                                        (callback adjust-slider)
                                        (init-value "440")
                                        (min-width 64)
                                        (stretchable-width #f)))

(define frequency-label (new message% (parent frequency-pane) (label "Hz")))
(define higher-button (new button% (parent frequency-pane) (label ">")))
                                        

; General Controls


(define control-pane (new horizontal-pane% (parent frame)
                                           (border 25)
                                           (spacing 25)))
(define duration-pane (new horizontal-pane% (parent control-pane)))
(define duration-field (new number-field% (label "Duration ")
                                          (parent duration-pane)
                                          (min-value 1)
                                          (max-value 600000) ; 10 minutes
                                          (init-value "200")
                                          (min-width 120)))
(define duration-label (new message% (parent duration-pane) (label "ms")))
(define play-button (new button% (parent control-pane)
                                 (label "Play")
                                 (callback generate-tone)))

; Set frequency to specific note

(define (set-note choice event)
  (set-frequency (hash-ref notes (send choice get-string-selection))))
(define note (new choice% (label "♪ ")
                          (choices '("A" "B" "C" "D" "F" "G"))
                          (parent control-pane)
                          (callback set-note)))    


; Display the GUI
(send frame show #t)


