#lang racket

; menu to display to user
(define menu
  "ADD UP TO 3 SEARCH CRITERIA:
   A: GAME TITLE
   B: RELEASE DATE
   C: PUBLISHER
   D: REGION
   E: GENRE
   F: DONE")

; hash table mapping a number representing the user's choice to a string
(define menu-hash
  (make-immutable-hash
   '((#\A . "GAME TITLE")
     (#\B . "RELEASE DATE")
     (#\C . "PUBLISHER")
     (#\D . "REGION")
     (#\E . "GENRE")
     (#\F . "DONE"))))

; pre  -- takes a list and an element to search the list for
; post -- returns true if the element is in the list; else false
(define (contains? lst element)
  (cond
    [(empty? lst)
     #f]
    [(eq? lst element)
     #t]
    [else
     (contains? (cdr lst) element)]
    ))

  
; pre  -- takes a list of menu choices that have previously been chosen
; post -- returns a valid character representing a menu option that hasn't already been chosen
(define (get-valid-choice [already-chosen '()] [num-left 0])
    
  (if (not (eq? num-left 0))
      (begin
        (display menu)
        (displayln ""))
      (begin
        (displayln "Enter any value to continue")))
          
  (define choice (read-line))
  (define casted-choice (string-ref (string-upcase choice) 0))
    
  (cond
    [(not (hash-has-key? menu-hash
                         casted-choice))
     (begin
       (displayln "Invalid input; input is not an option on menu")
       (get-valid-choice already-chosen num-left))]
      
    [(member casted-choice already-chosen)
     (begin
       (displayln "Invalid input; input has already been chosen")
       (get-valid-choice already-chosen num-left))]
      
    [(eq? num-left 0)
     #\F]
       
    [else
     casted-choice]))

; pre  -- takes a character representing an option from the menu
; post -- returns a function for handling said option
(define (get-input-handler choice)
  (if (eq? choice #\B)
      handle-date-range
      handle-string))

; pre  -- takes no parameters
; post -- returns a pair of integers representing the upper
;         and lower bounds of a date search range
(define (handle-date-range)
  "TODO1")

; pre  -- takes no parameters
; post -- returns a string representing search data
(define (handle-string)
  "TODO2")

; pre  -- takes a list of pairs where each pair contains a search category
;         and the associated search data obtained from user input; also takes
;         a list of categories the user has already selected
; post -- returns a list of up to three pairs
(define (get-search-data [output-list '()] [already-chosen'()] [num-left 3])
  
  (define current-choice (get-valid-choice already-chosen num-left))      
  (define current-category (hash-ref menu-hash current-choice))
  
  (define search-data ((get-input-handler current-choice)))
  
  (define pair-to-add (cons search-data current-category))
  (define updated-output-list (cons pair-to-add output-list))
  (define updated-already-chosen (cons current-choice output-list))

  (cond
    [(eq? current-category "DONE")
     output-list]
    
    [else
     (get-search-data updated-output-list updated-already-chosen (- num-left 1) )]))

(get-search-data)