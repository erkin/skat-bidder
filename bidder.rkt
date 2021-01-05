#lang racket/gui

;;;; Program info
(define program-name "Skat bid calculator")
(define program-version "v0.2")

(define version-message
  (format #<<version
~a ~a
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

(define (card-symbol card)
  (case (card-suit card)
    ((Acorns) "\u2663")
    ((Leaves) "\u2660")
    ((Hearts) "\u2665")
    ((Bells)  "\u2666")))

;; For selection
(define (card-suit-name card)
  (let ((suit (card-suit card)))
    (case suit
      ((Acorns) "Acorns/Clubs")
      ((Leaves) "Leaves/Spades")
      ((Hearts) "Hearts")
      ((Bells)  "Bells/Diamonds"))))

(define (card-rank-name card)
  (let ((rank (card-rank card)))
    (if (number? rank)
        (number->string rank)
        (case rank
          ((Deuce) "Deuce/Ace")
          ((King) "King")
          ((Ober) "Ober/Queen")
          ((Unter) "Unter/Jack")))))

;; For calculation
(define (suit-value suit)
  (cdr (assq suit '((Grand . 24) (Acorns . 12) (Leaves . 11) (Hearts . 10) (Bells . 9)))))

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
  (cond
    ((and (unter? a) (not Null?))
     (if (unter? b)
         (default-suit>? a b)
         #t))
    ((and (unter? b) (not Null?)) #f)
    ((same-suit? a b) (rank>? a b))
    ((suit>? a b) #t)
    (else #f)))

(define (sort-cards cards)
  ;; Some people prefer it in reverse order
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
  (define hand
    (take (sort-cards cards) (if (eq? trump 'Grand) 4 10)))
  (define trumps
    (take (sort-cards deck) (if (eq? trump 'Grand) 4 10)))
  (if (member (card 'Unter 'Acorns) hand)
      (let with ((hand hand) (matadors trumps) (count 0))
        (cond
          ;; Only kicks in if the whole hand is made up of unters and trumps
          ((null? hand) count)
          ;; Card matches, keep counting
          ((equal? (car hand) (car matadors))
           (with (cdr hand) (cdr matadors) (add1 count)))
          ;; Missing a card, hit the brake
          (else count)))
      ;; Return without count as negative
      (let without ((matadors trumps) (count 0))
        (cond
          ;; Only kicks in if the hand contains no unters and no trumps
          ((null? hand) (- count))
          ;; Found a card, hit the brake
          ((member (car matadors) hand) (- count))
          ;; No trump in hand yet, keep counting
          (else (without (cdr matadors) (add1 count)))))))

(define (calculate-value)
  (define-syntax-rule (++ str x)
    (begin (set! x (add1 x)) (string-append str (number->string x))))
  (define hand?             (send hand-box get-value))
  (define schneider?        (send schneider-box get-value))
  (define schneider-result? (send schneider-result-box get-value))
  (define schwarz?          (send schwarz-box get-value))
  (define schwarz-result?   (send schwarz-result-box get-value))
  (define ouvert?           (send ouvert-box get-value))
  (define lost?             (send lost-box get-value))
  (let* ((base-value (suit-value trump))
         (matadors (count-matadors))
         (point (abs matadors)))
    (~a
     (card-suit-name (card 'Deuce trump)) " " base-value
     (if (negative? matadors) " without " " with ")
     point
     (++ ", game " point)
     (if hand? (++ ", hand " point) "")
     (if schneider-result? (++ ", schneider " point) "")
     (if schneider? (++ ", schneider declared " point) "")
     (if schwarz-result? (++ ", schwarz " point) "")
     (if schwarz? (++ ", schwarz declared " point) "")
     (if ouvert? (++ ", ouvert " point) "")
     (cond (lost? (set! point (* -2 point))
                  (string-append ", lost " (number->string point)))
           (else ""))
     ": "
     (* point base-value))))

(define (calculate-null-value)
  (define hand? (send hand-box get-value))
  (define ouvert? (send ouvert-box get-value))
  ;; Fixed values
  (cond ((and hand? ouvert?)
         (format "Null ouvert hand: ~a" 59))
        (ouvert?
         (format "Null ouvert: ~a" 46))
        (hand?
         (format "Null hand: ~a" 35))
        (else
         (format "Null: ~a" 23))))

;;; Drawing logic
;; Card dimensions
(define card-x-margin 120)
(define card-y-margin 20)
(define card-x-size 60)
(define card-y-size 100)
(define card-spacing 40)

(define (draw-cards canvas dc)
  (send dc clear)
  (send dc set-text-foreground "Black")
  (send dc draw-text (number->string (length cards)) 0 0)
  (do ((step 0 (+ step card-spacing))
       (kards cards (cdr kards)))
      ((null? kards))
    ;; Border
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
      (send dc draw-text (card-symbol card)
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

;;; Contains the rest of the interface
(define bid-message
  (new message% (parent frame)
       (label "Pick ten cards")
       (enabled #f)
       (horiz-margin 10) (vert-margin 10)
       (auto-resize #t)))

(define options-pane
  (new vertical-pane% (parent frame)
       (alignment '(center center))
       (stretchable-height #f)))

(define (update-value)
  (if (< (length cards) 10)
      (send* bid-message
        (enable #f)
        (set-label "Pick ten cards"))
      (send* bid-message
        (enable #t)
        (set-label
         (if Null?
             (calculate-null-value)
             (calculate-value))))))

(define trump-selection
  (new radio-box% (parent options-pane)
       (label "Trump")
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

(define declaration-panel
  (new horizontal-panel% (parent options-pane)
       (stretchable-width #f)))

(define contract-message
  (new message% (parent declaration-panel)
       (label "Contract")))

(define null-box
  (new check-box% (parent declaration-panel)
       (label "Null")
       (callback
        (λ (tickbox event)
          (when (eq? 'check-box (send event get-event-type))
            (set! Null? (send tickbox get-value))
            (send trump-selection enable (not Null?))
            (send schneider-box enable (not Null?))
            (send schneider-result-box enable (not Null?))
            (send schwarz-box enable (not Null?))
            (send schwarz-result-box enable (not Null?))
            (set! cards (sort-cards cards))
            (draw-cards canvas dc)
            (when (not Null?)
              (send schneider-box set-value #f)
              (send schwarz-box set-value #f)
              (send ouvert-box set-value #f)))
          (update-value)))))

(define hand-box
  (new check-box% (parent declaration-panel)
       (label "Hand")
       (callback
        (λ (tickbox event)
          (when (and (not Null?)
                     (eq? 'check-box (send event get-event-type))
                     (not (send tickbox get-value)))
            (send schneider-box set-value #f)
            (send schwarz-box set-value #f)
            (send ouvert-box set-value #f))
          (update-value)))))

(define schneider-box
  (new check-box% (parent declaration-panel)
       (label "Schneider")
       (callback
        (λ (tickbox event)
          (when (and (not Null?)
                     (eq? 'check-box (send event get-event-type)))
            (cond ((send schneider-box get-value)
                   (send schneider-result-box set-value #t)
                   (send hand-box set-value #t))
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
            (cond ((send schwarz-box get-value)
                   (send schwarz-result-box set-value #t)
                   (send schneider-result-box set-value #t)
                   (send schneider-box set-value #t)
                   (send hand-box set-value #t))
                  (else
                   (send ouvert-box set-value #f))))
          (update-value)))))

(define ouvert-box
  (new check-box% (parent declaration-panel)
       (label "Ouvert")
       (callback
        (λ (tickbox event)
          (when (and (not Null?)
                     (eq? 'check-box (send event get-event-type))
                     (send tickbox get-value))
            (send schwarz-result-box set-value #t)
            (send schneider-result-box set-value #t)
            (send schwarz-box set-value #t)
            (send schneider-box set-value #t)
            (send hand-box set-value #t))
          (update-value)))))

(define endgame-panel
  (new horizontal-panel% (parent options-pane)
       (stretchable-width #f)))

(define result-message
  (new message% (parent endgame-panel)
      (label "Result")))

(define lost-box
  (new check-box% (parent endgame-panel)
       (label "Lost")
       (callback
        (λ (tickbox event)
          (update-value)))))

(define schneider-result-box
  (new check-box% (parent endgame-panel)
       (label "Schneider")
       (callback
        (λ (tickbox event)
          (when (and (eq? 'check-box (send event get-event-type))
                     (not (send tickbox get-value)))
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
              ((send tickbox get-value)
               (send schneider-result-box set-value #t))
              (else
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

(define card-selection
  (new horizontal-pane% (parent frame)
       (alignment '(left top))
       (stretchable-width #f) (stretchable-height #f)))

;;; Generate tickboxes for cards
(for ((suit (in-list (get-suits))))
  (define panel
    (new group-box-panel% (parent card-selection)
         (label (card-suit-name (card 'Deuce suit)))
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
                ((>= (length cards) 10)
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
  (error-display-handler
   (λ (str ex)
     (displayln str (current-error-port))
     (when (exn:fail? ex)
       (message-box "Error" str frame '(stop ok)))))
  (send* frame
    (center)
    (show #t)))
