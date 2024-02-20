#lang racket

(provide get-search-data)
(provide get-sorting-option)

; menu to display to user for categories
(define category-menu
  "ADD UP TO 3 SEARCH CRITERIA:
   A: GAME TITLE
   B: RELEASE DATE
   C: PUBLISHER
   D: REGION
   E: GENRE
   F: DONE")

; menu to display to the user for regions
(define region-menu
  "CHOOSE A REGION:
   A: UNITED STATES
   B: JAPAN
   C: EUROPE
   D: REST OF WORLD
   E: GLOBAL")

; menu for sorting options
(define sort-menu
  "CHOOSE A SORTING OPTION
   A: SALES
   B: RATING")

; hash table mapping a letter representing search category to a string
(define category-menu-hash
  (make-immutable-hash
   '((#\A . "GAME TITLE")
     (#\B . "RELEASE DATE")
     (#\C . "PUBLISHER")
     (#\D . "REGION")
     (#\E . "GENRE")
     (#\F . "DONE"))))

; hash table mapping letter representing region to a string
(define region-menu-hash
  (make-immutable-hash
   '((#\A . "UNITED STATES")
     (#\B . "JAPAN")
     (#\C . "EUROPE")
     (#\D . "REST OF WORLD")
     (#\E . "GLOBAL"))))

; hash table mapping letter representing sorting option to string
(define sort-menu-hash
  (make-immutable-hash
   '((#\A . "SALES")
     (#\B . "RATING"))))

; pre  -- takes a list of menu options that have previously been chosen, a menu to display to the user, and a hash table
; post -- returns a valid character representing a menu option that hasn't already been chosen
(define (get-valid-choice menu menu-hash [already-chosen '()] [num-left 0])
    
  (if (not (eq? num-left 0))
      (begin
        (display menu)                                    ; display the menu if the user can still enter more categories
        (displayln ""))
      (begin
        (displayln "Enter any value to continue")))
          
  (define choice (read-line))                                   ; raw user input
  (define casted-choice (string-ref (string-upcase choice) 0))  ; the user's input casted to a capitalized char
    
  (cond
    [(not (hash-has-key? menu-hash                        ; if the received input is not on the menu
                         casted-choice))
     (begin
       (displayln "Invalid input; input is not an option on menu")
       (get-valid-choice menu menu-hash already-chosen num-left))]
      
    [(member casted-choice already-chosen)                ; if the user has already chosen the menu option
     (begin
       (displayln "Invalid input; input has already been chosen")
       (get-valid-choice menu menu-hash already-chosen num-left))]
      
    [(eq? num-left 0)                                     ; if the user has already picked num-left options from the menu
     #\F]
       
    [else                                                 ; if the user's input is valid, return it
     casted-choice]))

; pre  -- takes a character representing an option from the menu
; post -- returns a function for handling said option
(define (get-input-handler choice)
  (cond
    [(eq? choice #\B)
      handle-range]
    
     [(eq? choice #\D)
       handle-region]

     [(eq? choice #\F)
      skip]
     
     [else
      handle-string]))

; pre  -- takes no parameters
; post -- returns a pair of integers representing the upper
;         and lower bounds of a date search range
(define (handle-range)

  (define lower (get-num "Enter lower bound"))
  (define upper (get-num "Enter upper bound"))

  (cond
    
    [(< lower upper)
     (cons lower upper)]

    [(> lower upper)
     (cons upper lower)]

    [else
     (cons lower upper)]))

    

; pre  -- takes no parameters
; post -- returns a string representing search data
(define (handle-string)
  (begin
    (displayln "Enter search term")
    (read-line)))

; pre  -- takes no parameters
; post -- returns a pair containing a region and a sales range
(define (handle-region)
  (define region (hash-ref region-menu-hash
                           (get-valid-choice region-menu region-menu-hash '() 1)))
  
  (cons region (handle-range)))

; pre  -- takes no parameters
; post -- does nothing
(define (skip)
  "")

; pre  -- takes a string to display to the user
; post -- gets a string from user input; if the string is numeric, returns the string
;         type-casted to an integer, else recursively calls itself
(define (get-num message)

  (begin
    (displayln message))
  
  (define num (read-line))
  (define input-list (string->list num))
  (define num-list (filter char-numeric? input-list))

  (cond
    
    [(eq? (length input-list)
          (length num-list))
     (string->number num)]

    [else
     (begin
       (displayln "Invalid input; enter a number")
       (get-num message))]))
  

; pre  -- takes a list of pairs where each pair contains a search category
;         and the associated search data obtained from user input; also takes
;         a list of categories the user has already selected and a counter
; post -- returns a list of up to three pairs
(define (get-search-data [output-list '()] [already-chosen'()] [num-left 3])
  
  (define current-choice (get-valid-choice category-menu category-menu-hash already-chosen num-left))            ; binds a valid menu option from the user   
  (define current-category (hash-ref category-menu-hash current-choice))                                         ; binds what said menu option maps to in the menu-hash
  
  (define search-data ((get-input-handler current-choice)))                     ; evaluates different functions based on current-choice and binds what they return
  
  (define pair-to-add (cons search-data current-category))                      ; creates a pair to add to the output list
  (define updated-output-list (cons pair-to-add output-list))                   ; updates output list
  (define updated-already-chosen (cons current-choice output-list))             ; updates list of menu options already chosen

  (cond
    [(eq? current-category "DONE")                                              ; base case is evaluated if current-choice is #\F or the user has entered 3 categories
     output-list]
    
    [else
     (get-search-data updated-output-list updated-already-chosen (- num-left 1) )]))

; pre  -- takes a menu and a hash table representing sorting options
; post -- returns a string representing whether to sort by sales or reviews
(define (get-sorting-option)
  (hash-ref sort-menu-hash
            (get-valid-choice sort-menu sort-menu-hash '() 1)))
