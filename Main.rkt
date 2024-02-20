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
     ("GLOBAL" . 11)
     ("SALES" . 1)
     ("RATING" . 12)
     )))

; pre  -- takes a list
; post -- displays the list nicely to the console
(define (print-list lst)
  (for-each
   (lambda(sub-list) (displayln sub-list))
   lst))

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

; pre  -- takes a list
; post -- performs car or cdr on list with the addition of
;         returning an empty list if the passed list is empty
(define (mod-cdr lst)
  (if (empty? lst) '() (cdr lst)))
(define (mod-car lst)
  (if (empty? lst) '() (car lst)))


; pre  -- takes data and search-filters to apply to said data
; post -- filters data based off of search filters
(define (search data search-filters)

  (define current-filter (mod-car search-filters))     ; e.g. ((2000 . 2020) . "RELEASE DATE")
  (define current-category (mod-cdr current-filter))   ; e.g. "RELEASE DATE"

  (define str-list '("GAME TITLE" "PUBLISHER" "GENRE")) 

  (define (handle-num-range current-term) ; current-term must be in format ((lower . upper) . "CATEGORY NAME")
    (define index (hash-ref category->index-hash (cdr current-term)))
    (define numbers (car current-term))
    (define upper-bound (cdr numbers))
    (define lower-bound (car numbers))
    
    (define updated-data (number-filter data lower-bound upper-bound index))

    (search updated-data (cdr search-filters)))

  
  (define (handle-string current-term) ; current-term must be in format ("STRING" . "CATEGORY NAME")
    (define index (hash-ref category->index-hash (cdr current-term)))
    (define search-string (car current-term))

    (define updated-data (string-filter data search-string index))

    (search updated-data (cdr search-filters)))

  
  (cond
    [(empty? data)
     '("NO RESULTS; TRY USING FEWER FILTERS")]

    [(empty? search-filters)
     data]

    [(member current-category str-list)
     (handle-string current-filter)]

    [(equal? current-category "RELEASE DATE")
     (handle-num-range current-filter)]

    [(equal? current-category "REGION")
     (handle-num-range (car current-filter))]))

; pre  -- takes a list with the csv file contents and a sorting criterion 
; post -- returns a sorted list
(define (sort-list lst criterion)

  (define index (hash-ref category->index-hash criterion))

  (define (compare current next)
    (> (list-ref current index) (list-ref next index)))

  (sort lst compare))

    



  
    
(define sample '(("mario" . "GAME TITLE") ((2000 . 2020) . "RELEASE DATE") (((20 . 40) . "GLOBAL") . "REGION")))
(search data sample)

