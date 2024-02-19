#lang racket
(require "FileReader.rkt") ; provides "data" which contains all the contents from "Video Games Sales.csv"
(require "UI.rkt")         ; provides "get-search-data" and "get-sorting-option" functions for getting user input

; hash table for mapping user input to an index of a list from "data"
(define category->index-hash
  (make-immutable-hash
   '(("GAME TITLE" . 2)
     ("RELEASE DATE" . 4)
     ("PUBLISHER" . 6)
     ("GENRE" . 5)
     ("NORTH AMERICA" . 7)
     ("EUROPE" . 8)
     ("JAPAN" . 9)
     ("REST OF WORLD" . 10)
     ("GLOBAL" . 11))))

; pre  -- takes a list
; post -- displays the list nicely to the console
(define (print-list lst)
  (for-each
   (lambda(sub-list) (displayln sub-list))
   lst))

; pre  -- takes a search category
; post -- returns a filter function
(define (filter-selector category)

  (define str-list '("GAME TITLE" "PUBLISHER" "GENRE"))
  (define num-list '("RELEASE DATE" "GENRE"))

  (cond
    [(member category str-list)
     string-filter]
    
    [(member category num-list)
     number-filter]
    
    [else
     "Error"]))

; pre  -- takes a list of lists, a string, and an index
; post -- filters the list of lists to only contain lists that
;         contain the given string at the given index
(define (string-filter data search-string index)

  (define (predicate lst)
    (define pattern (regexp (string-downcase search-string)))
    (if (eq? #f (regexp-match pattern (string-downcase (list-ref lst index))))
        #f
        #t))

  (filter predicate (cdr data)))
    
; pre  -- takes a list of lists, an upper and lower bounds, and an index
; post -- filters the given list of lists to only have entries
;         that contain numbers between lower and upper at the given index
(define (number-filter data lower upper index)

  (define (predicate lst)
    (define comparison-number (string->number (list-ref lst index)))
    (and (>= comparison-number lower) (<= comparison-number upper)))

  (filter predicate (cdr data)))
    
;(define reg (regexp "hello"))
;(regexp-match reg "world")
;(regexp-match reg "hellowrd")

(number-filter data 1 50 0)