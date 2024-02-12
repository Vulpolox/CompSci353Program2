#lang racket

; menu to display to user
(define menu
  "ADD UP TO 3 SEARCH CRITERIA:
   1: GAME TITLE
   2: RELEASE DATE
   3: PUBLISHER
   4: REGION
   5: GENRE
   6: DONE")

; hash table mapping a number representing the user's choice to a string
(define menu-hash
  (make-immutable-hash
   '((1 . "GAME TITLE")
     (2 . "RELEASE DATE")
     (3 . "PUBLISHER")
     (4 . "REGION")
     (5 . "GENRE")
     (6 . "DONE"))))

; pre  -- doesn't take any parameters
; post -- returns a list containing 1-3 search criteria
(define (get-user-input [input-list '()])
  (begin
    (display menu)
    (displayln "")
    (define current-input (read-line))
    (displayln current-input)
    ))
    

(get-user-input)
  