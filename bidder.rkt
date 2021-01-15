#!/usr/bin/env racket
#lang racket/gui

;;;; Program info
(define program-name "Skat bid calculator")
(define program-version "v0.4")

(define version-message
  (format #<<version
~a ~a
https://github.com/erkin/skat-bidder

Copyright (C) 2021 Erkin Batu Altunbas
Each file of this project's source code is subject 
to the terms of the Mozilla Public Licence v2.0
https://mozilla.org/MPL/2.0/
version
          program-name program-version))

(define (quit)
  (custodian-shutdown-all (current-custodian))
  (queue-callback exit #t))

(define (about)
  (message-box
   (string-append "About " program-name)
   version-message frame '(ok no-icon)))

;;; Global variables
(define trump 'Acorns)
(define Null? #f)
(define French? #f)
(define reversed? (make-parameter #f))
(define cards '())

;;;; Card data structure
(struct card (rank suit)
  #:transparent
  #:methods gen:custom-write ;; For debugging
  ((define (write-proc card port mode)
     (when mode (write-string "<" port))
     (fprintf port "~A of ~A" (card-rank card) (card-suit card))
     (when mode (write-string ">" port)))))

;; For drawing
(define (card-colour card)
  (send the-color-database find-color
        (case (card-suit card)
          ((Acorns) (if French? "Black" "DarkGoldenrod"))
          ((Leaves) (if French? "Black" "DarkGreen"))
          ((Hearts) "Red")
          ((Bells)  (if French? "Red" "Orange")))))

(define (card-letter card)
  (let ((rank (card-rank card)))
    (case rank
      ((Deuce) "A")
      ((King) "K")
      ((Ober) (if French? "Q" "O"))
      ((Unter) (if French? "J" "U"))
      (else (number->string rank)))))

(define (suit-symbol suit)
  (case suit
    ((Acorns) "\u2663")
    ((Leaves) "\u2660")
    ((Hearts) "\u2665")
    ((Bells)  "\u2666")))

;; For calculation
(define (suit-value suit)
  (case suit
    ((Acorns) 12)
    ((Leaves) 11)
    ((Hearts) 10)
    ((Bells)   9)
    ((Grand)  24)))

;; For selection
(define (suit-name suit)
  (case suit
    ((Acorns) "Acorns/Clubs")
    ((Leaves) "Leaves/Spades")
    ((Hearts) "Hearts")
    ((Bells)  "Bells/Diamonds")
    ((Grand)  "Grand")))

(define (card-rank-name card)
  (let ((rank (card-rank card)))
    (if (number? rank)
        (number->string rank)
        (case rank
          ((Deuce) "Deuce/Ace")
          ((King) "King")
          ((Ober) "Ober/Queen")
          ((Unter) "Unter/Jack")))))

;;; Sorting logic
(define (get-ranks)
  (if Null?
      '(Deuce King Ober Unter 10 9 8 7)
      '(Deuce 10 King Ober Unter 9 8 7)))
(define default-suits
  '(Acorns Leaves Hearts Bells))
(define (get-suits)
  (if (eq? trump 'Grand)
      default-suits
      (cons trump (remove trump default-suits))))

(define ((>? list accessor) a b)
  (let loop ((lst list))
    (cond ((eq? (accessor a) (car lst)) #t)
          ((eq? (accessor b) (car lst)) #f)
          (else (loop (cdr lst))))))

(define (rank>? a b)
  ((>? (get-ranks) card-rank) a b))
(define (suit>? a b)
  ((>? (get-suits) card-suit) a b))
(define (default-suit>? a b)
  ((>? default-suits card-suit) a b))

(define (same-rank? a b)
  (eq? (card-rank a)
       (card-rank b)))
(define (same-suit? a b)
  (eq? (card-suit a)
       (card-suit b)))

(define (card>? a b)
  ;; Unters/Jacks should be ranked before others
  ;; but not in a null game.
  (define (unter? card)
    (eq? 'Unter (card-rank card)))
  (cond ((and (unter? a) (not Null?))
         (if (unter? b)
             (default-suit>? a b)
             #t))
        ((and (unter? b) (not Null?)) #f)
        ((same-suit? a b) (rank>? a b))
        ((suit>? a b) #t)
        (else #f)))

(define (sort-cards cards)
  ;; Some people prefer it in reverse order
  ;; reversed? is a parameter so that we can dynamically alter it externally.
  (if (reversed?)
      (sort cards (negate card>?))
      (sort cards card>?)))

(define (add-card! card)
  (set! cards (sort-cards (cons card cards))))
(define (remove-card! card)
  (set! cards (remove card cards)))

(define (set-trump! suit)
  (set! trump suit)
  (set! cards (sort-cards cards)))

(define deck
  (for*/list ((rank (in-list (get-ranks)))
              (suit (in-list (get-suits))))
    (card rank suit)))

(define (count-matadors)
  ;; Take only the top four cards if grand is declared, otherwise take them all.
  (define hand
    (parameterize ((reversed? #f))
      (take (sort-cards cards) (if (eq? trump 'Grand) 4 11))))
  ;; Likewise. In the case of grand, trumps contains the unters only.
  ;; Otherwise it contains the eleven trumps: 4 unters and the selected suit, descending.
  (define trumps
    (parameterize ((reversed? #f))
      (take (sort-cards deck) (if (eq? trump 'Grand) 4 11))))
  ;; Playing 'with' or 'without' is determined by the unter of acorns.
  (if (member (card 'Unter 'Acorns) hand)
      (let with ((hand hand) (matadors trumps) (count 0))
        (cond
          ;; Only kicks in if the whole hand is made up of unters and trumps.
          ((null? hand) count)
          ;; Card matches, keep counting.
          ((equal? (car hand) (car matadors))
           (with (cdr hand) (cdr matadors) (add1 count)))
          ;; Missing a card, hit the brake.
          (else count)))
      ;; Return the 'without' count as negative.
      (let without ((matadors trumps) (count 0))
        (cond
          ;; Only kicks in if the hand contains no unters and no trumps.
          ((null? hand) (- count))
          ;; Found a card, hit the brake.
          ((member (car matadors) hand) (- count))
          ;; No trump in hand yet, keep counting.
          (else (without (cdr matadors) (add1 count)))))))

;;; TODO: Maybe a better/functional way to do this?
(define (finalise-value points)
  (define text "")
  (define score points)
  (define-syntax-rule (update-score! new-text new-score)
    (begin
      (set! text (~a text ", " new-text " " (* score new-score)))
      (set! score (* score new-score))))
  (cond
    ((send supra-box get-value)
     (update-score! "supra" 8))
    ((send re-box get-value)
     (update-score! "re" 4))
    ((send kontra-box get-value)
     (update-score! "kontra" 2)))
  (when (send bock-box get-value)
    (update-score! "bock" 2))
  (when (send lost-box get-value)
    (update-score! "lost" -2))
  (values text points))

(define (calculate-value)
  (define matadors (count-matadors))
  (define base-value (suit-value trump))
  (let loop
      ;; Matadors are negative if 'without'.
      ((score (add1 (abs matadors)))
       ;; Get boxes (in this exact order) to iterate.
       (boxes `((,hand-box             . "hand")
                (,schneider-result-box . "schneider")
                (,schneider-box        . "schneider declared")
                (,schwarz-result-box   . "schwarz")
                (,schwarz-box          . "schwarz declared")
                (,ouvert-box           . "ouvert")))
       ;; Prepend the selected trump and its base-value.
       (text (~a (suit-name trump) " " base-value
                 ;; Then determine whether the hand is
                 ;; 'with' or 'without', then append the score.
                 (if (negative? matadors) " without " " with ") (abs matadors)
                 ", play " (add1 (abs matadors)))))
    (cond
      ;; Ran out of boxes.
      ((null? boxes)
       (call-with-values
        (thunk (finalise-value score))
        (λ (additional-text final-score)
          (~a text additional-text ": " (* final-score base-value)))))
      ;; The box in the car is ticked, check its value.
      ((send (caar boxes) get-value)
       ;; Increment the score coefficient, then use the text in the cdr.
       (loop (add1 score) (cdr boxes) (~a text ", " (cdar boxes) " " (add1 score))))
      ;; The box is the car was not ticked.
      (else
       ;; Continue iterating.
       (loop score (cdr boxes) text)))))

(define (calculate-null-value)
  (define hand? (send hand-box get-value))
  (define ouvert? (send ouvert-box get-value))
  (define revolution? (send revolution-box get-value))
  (define lost? (send lost-box get-value))
  ;; Null contracts have fixed values.
  (define contract
    (list-ref
     '(("Revolution" . 92)
       ("Null ouvert hand" . 59)
       ("Null ouvert" . 46)
       ("Null hand" . 35)
       ("Null" . 23))
     (cond (revolution? 0)
           ((and hand? ouvert?) 1)
           (ouvert? 2)
           (hand? 3)
           (else 4))))
  (let ((text (car contract))
        (score (cdr contract)))
    (call-with-values
     (thunk (finalise-value score))
     (λ (additional-text final-score)
       (~a text " " score additional-text)))))

;;; Drawing logic
;; Dimensions of the cards on the table
(define card-x-margin 90)
(define card-y-margin 20)
(define card-x-size 60)
(define card-y-size 100)
(define card-spacing 40)

(define (draw-cards canvas dc)
  (send dc clear)
  ;; Number of cards in the hand is shown at the top left corner.
  (send dc set-text-foreground "Black")
  (send dc draw-text (number->string (length cards)) 0 0)
  ;; Render each card in the hand left to right.
  (do ((step 0 (+ step card-spacing))
       (kards cards (cdr kards)))
      ((null? kards))
    ;; Card border
    (send dc set-brush "Brown" 'solid)
    (send dc draw-rectangle
          (+ step card-x-margin) card-y-margin
          card-x-size card-y-size)
    ;; Card background
    (send dc set-brush "Ivory" 'solid)
    (send dc draw-rectangle
          (+ step (add1 card-x-margin)) (add1 card-y-margin)
          (sub1 card-x-size) (sub1 card-y-size))
    (let ((card (car kards)))
      ;; Card text
      (send dc set-text-foreground (card-colour card))
      (send dc draw-text (card-letter card)
            (+ 5 step card-x-margin) (+ 2 card-y-margin))
      ;; Card symbol
      (send dc draw-text (suit-symbol (card-suit card))
            (+ 3 step card-x-margin) (+ 20 card-y-margin)))))

;;;; Windowing logic
(define frame
  (new frame%
       (label (string-append program-name " " program-version))
       (style '(no-resize-border))
       (width 680)  (min-width 680)
       (height 500) (min-height 500)
       (stretchable-width #f) (stretchable-height #f)))

;;; Contains aesthetic options
(define top-panel
  (new horizontal-panel% (parent frame)
       (alignment '(left center))
       (stretchable-height #f)))

(define canvas
  (new canvas% (parent frame)
       (style '(no-focus))
       (paint-callback draw-cards)))

(define dc
  (send canvas get-dc))

;;;; Rest of the interface from here on
(define bid-message
  (new message% (parent frame)
       (label "Pick twelve cards")
       (enabled #f)
       (horiz-margin 10) (vert-margin 10)
       (auto-resize #t)))

(define options-pane
  (new vertical-pane% (parent frame)
       (alignment '(center center))
       (stretchable-height #f)))

(define (update-value)
  (if (< (length cards) 12)
      (send* bid-message
        (enable #f)
        (set-label "Pick twelve cards"))
      (send* bid-message
        (enable #t)
        (set-label
         (if Null?
             (calculate-null-value)
             (calculate-value))))))

(define trump-selection
  (new radio-box% (parent options-pane)
       (label "Trump:")
       (choices '("Grand" "Acorns/Clubs" "Leaves/Spades" "Hearts" "Bells/Diamonds"))
       (selection 1)
       (style '(horizontal horizontal-label))
       (callback
        (λ (radiobox event)
          (when (eq? 'radio-box (send event get-event-type))
            (set! trump (list-ref '(Grand Acorns Leaves Hearts Bells)
                                  (send radiobox get-selection)))
            (set! cards (sort-cards cards))
            (draw-cards canvas dc))
          (update-value)))))

;;; Announcements before the game begins
(define declaration-panel
  (new horizontal-panel% (parent options-pane)
       (stretchable-width #f)))

(define contract-message
  (new message% (parent declaration-panel)
       (label "Announcement:")))

(define null-box
  (new check-box% (parent declaration-panel)
       (label "Null")
       (callback
        (λ (tickbox event)
          (set! Null? (send tickbox get-value))
          ;; Disable all irrelevant declarations if null is ticked.
          (send trump-selection enable (not Null?))
          (send schneider-box enable (not Null?))
          (send schneider-result-box enable (not Null?))
          (send schwarz-box enable (not Null?))
          (send schwarz-result-box enable (not Null?))
          (send revolution-box enable Null?)
          ;; Sort the cards again.
          (set! cards (sort-cards cards))
          (draw-cards canvas dc)
          (when (not Null?)
            ;; Untick announcement boxes.
            (send schneider-box set-value #f)
            (send schwarz-box set-value #f)
            (send ouvert-box set-value #f)
            (send revolution-box set-value #f))
          (update-value)))))

(define hand-box
  (new check-box% (parent declaration-panel)
       (label "Hand")
       (callback
        (λ (tickbox event)
          (when (and (eq? 'check-box (send event get-event-type))
                     (not (send tickbox get-value)))
            ;; Disable all announcement boxes if this isn't a hand game.
            (if Null?
                (send revolution-box set-value #f)
                (begin
                  (send schneider-box set-value #f)
                  (send schwarz-box set-value #f)
                  (send ouvert-box set-value #f))))
          (update-value)))))

(define schneider-box
  (new check-box% (parent declaration-panel)
       (label "Schneider")
       (callback
        (λ (tickbox event)
          (when (and (not Null?)
                     (eq? 'check-box (send event get-event-type)))
            (cond
              ;; If schneider is ticked, hand also needs to be ticked.
              ((send schneider-box get-value)
               ;; Schneider always comes with the schneider result.
               (send schneider-result-box set-value #t)
               (send hand-box set-value #t))
              ;; If schneider is unticked, schwarz and ouvert
              ;; also get automatically unticked.
              (else
               (send schwarz-box set-value #f)
               (send ouvert-box set-value #f))))
          (update-value)))))

(define schwarz-box
  (new check-box% (parent declaration-panel)
       (label "Schwarz")
       (callback
        (λ (tickbox event)
          (when (and (not Null?)
                     (eq? 'check-box (send event get-event-type)))
            (cond
              ;; If schwarz is ticked, schneider and hand
              ;; also need to be ticked.
              ((send schwarz-box get-value)
               ;; Schwarz always comes with
               ;; the schwarz *and* the schneider results.
               (send schwarz-result-box set-value #t)
               (send schneider-result-box set-value #t)
               (send schneider-box set-value #t)
               (send hand-box set-value #t))
              ;; If schwarz is unticked, ouvert
              ;; also gets automatically unticked.
              (else
               (send ouvert-box set-value #f))))
          (update-value)))))

(define ouvert-box
  (new check-box% (parent declaration-panel)
       (label "Ouvert")
       (callback
        (λ (tickbox event)
          (when (eq? 'check-box (send event get-event-type))
            (cond
              ((and (not Null?) (send tickbox get-value))
               ;; If ouvert is ticked, schwarz, schneider and hand
               ;; also need to be ticked, with their respective result boxes.
               (send schwarz-result-box set-value #t)
               (send schneider-result-box set-value #t)
               (send schwarz-box set-value #t)
               (send schneider-box set-value #t)
               (send hand-box set-value #t))
              ((and Null? (not (send tickbox get-value)))
               (send revolution-box set-value #f))))
          (update-value)))))

;;; Additional declarations
(define misc-panel
  (new horizontal-panel% (parent options-pane)
       (stretchable-width #f)))

(define misc-message
  (new message% (parent misc-panel)
       (label "Custom:")))

(define bock-box
  (new check-box% (parent misc-panel)
       (label "Bock")
       (callback (thunk* (update-value)))))

(define kontra-box
  (new check-box% (parent misc-panel)
       (label "Kontra")
       (callback
        (λ (tickbox event)
          (when (and (eq? 'check-box (send event get-event-type))
                     (not (send tickbox get-value)))
            (send re-box set-value #f)
            (send supra-box set-value #f))
          (update-value)))))

(define re-box
  (new check-box% (parent misc-panel)
       (label "Re")
       (callback
        (λ (tickbox event)
          (when (eq? 'check-box (send event get-event-type))
            (if (send tickbox get-value)
                (send kontra-box set-value #t)
                (send supra-box set-value #f)))
          (update-value)))))

(define supra-box
  (new check-box% (parent misc-panel)
       (label "Supra")
       (callback
        (λ (tickbox event)
          (when (and (eq? 'check-box (send event get-event-type))
                     (send tickbox get-value))
            (send kontra-box set-value #t)
            (send re-box set-value #t)
            (send supra-box set-value #t))
          (update-value)))))

(define revolution-box
  (new check-box% (parent misc-panel)
       (label "Revolution")
       (enabled #f)
       (callback
        (λ (tickbox event)
          (when (and (eq? 'check-box (send event get-event-type))
                     (send tickbox get-value))
            (send null-box set-value #t)
            (send ouvert-box set-value #t)
            (send hand-box set-value #t))
          (update-value)))))

;;; Situation at the end of the game
(define endgame-panel
  (new horizontal-panel% (parent options-pane)
       (stretchable-width #f)))

(define result-message
  (new message% (parent endgame-panel)
      (label "Result:")))

(define lost-box
  (new check-box% (parent endgame-panel)
       (label "Lost")
       (callback (thunk* (update-value)))))

(define schneider-result-box
  (new check-box% (parent endgame-panel)
       (label "Schneider")
       (callback
        (λ (tickbox event)
          (when (and (eq? 'check-box (send event get-event-type))
                     (not (send tickbox get-value)))
            ;; Schneider result is tied to
            ;; schneider, schwarz, schwarz result and ouvert.
            (send schwarz-result-box set-value #f)
            (send schwarz-box set-value #f)
            (send schneider-box set-value #f)
            (send ouvert-box set-value #f))
          (update-value)))))

(define schwarz-result-box
  (new check-box% (parent endgame-panel)
       (label "Schwarz")
       (callback
        (λ (tickbox event)
          (when (and (eq? 'check-box (send event get-event-type)))
            (cond
              ;; Can't win schwarz without winning schneider.
              ((send tickbox get-value)
               (send schneider-result-box set-value #t))
              (else
               ;; Schwarz result is tied to schwarz and ouvert.
               (send schwarz-result-box set-value #f)
               (send schwarz-box set-value #f)
               (send ouvert-box set-value #f))))
          (update-value)))))

(define style-box
 (new radio-box% (parent top-panel)
      (label "Suit")
      (choices '("French" "German"))
      (selection 1)
      (style '(horizontal horizontal-label))
      (callback
       (λ (radiobox event)
         (when (eq? 'radio-box (send event get-event-type))
           (set! French? (zero? (send radiobox get-selection)))
           (draw-cards canvas dc))))))

(define reversed-box
  (new check-box% (parent top-panel)
       (label "Reverse order")
       (callback
        (λ (tickbox event)
          (when (eq? 'check-box (send event get-event-type))
            (reversed? (send tickbox get-value))
            (set! cards (sort-cards cards))
            (draw-cards canvas dc))))))

;;; Picking cards for the hand
(define card-selection
  (new horizontal-pane% (parent frame)
       (alignment '(left top))
       (stretchable-width #f) (stretchable-height #f)))

;;; Generate tickboxes for cards.
(for ((suit (in-list (get-suits))))
  (define panel
    (new group-box-panel% (parent card-selection)
         (label (suit-name suit))
         (alignment '(left center))
         (stretchable-width #f) (stretchable-height #f)))
  (for ((rank (in-list (get-ranks))))
    (define the-card (card rank suit))
    (new check-box% (parent panel)
         (label (card-rank-name (card rank suit)))
         (callback
          (λ (tickbox event)
            (when (eq? 'check-box (send event get-event-type))
              (cond
                ;; Toggled off
                ((not (send tickbox get-value))
                 (remove-card! the-card)
                 (draw-cards canvas dc))
                ;; Toggled on but there are too many cards
                ((>= (length cards) 12)
                 (send tickbox set-value #f))
                ;; Toggled on
                (else
                 (add-card! the-card)
                 (draw-cards canvas dc))))
            (update-value))))))

(define bottom-panel
  (new horizontal-panel% (parent frame)
       (alignment '(right bottom))
       (stretchable-height #f)))

(define about-button
  (new button% (parent bottom-panel)
       (label "About")
       (callback (thunk* (about)))))

(define quit-button
  (new button% (parent bottom-panel)
       (label "Quit")
       (callback (thunk* (quit)))))

(send dc set-background
      (send the-color-database find-color "Forest Green"))

(module+ main
  (application-quit-handler quit)
  (application-about-handler about)
  ;; Show errors as pop-up messages but also log them to stderr.
  (error-display-handler
   (λ (str ex)
     (displayln str (current-error-port))
     (when (exn:fail? ex)
       (message-box "Error" str frame '(stop ok)))))
  (send* frame
    (center)
    (show #t)))
