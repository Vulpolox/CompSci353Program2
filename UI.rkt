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

; pre  -- takes no parameters
; post -- returns a string from user input
(define (get-str)
  (begin
    (displayln "Enter search terms")
    (define input-search-terms (read-line))
    input-search-terms))
  

; pre  -- doesn't take any parameters
; post -- returns a list containing 1-3 search criteria paired with strings to search
(define (get-user-input [input-list '()])
  (define (input-validator user-input)                                     ; nested input validator function
    (define converted-input (string-ref (string-upcase user-input)       ; converted input
                                        0))
    (cond
      [(eq? (string-length user-input)
            0)
       (begin
         (displayln "Input is invalid; empty string was entered")
         (get-user-input input-list))]
      [(not (hash-has-key? menu-hash
                           converted-input))
       (begin
         (displayln "Input is invalid; choose an option from the menu")
         (get-user-input input-list))]
      [(contains? input-list
                  (hash-ref menu-hash
                            converted-input))
       (begin
         (displayln "Input is invalid; option already selected")
         (get-user-input input-list))]
      [else
       converted-input]
      ))
  (begin
    (display menu)
    (displayln "")
    (define current-input (read-line))                          
    (define validated-input (input-validator current-input)))
  
    (cond
      [(eq? #\F validated-input)
       input-list]
      [(eq? (length input-list)
                    3)
       input-list]
      [else
       (begin    
         (define current-category (hash-ref menu-hash                
                                       validated-input))
         (define search-terms (get-str))
         (define pair-to-add (cons current-category search-terms))
         (define updated-list (cons pair-to-add input-list))
         (get-user-input updated-list))]
      ))
    
    
    
    

(get-user-input)
  