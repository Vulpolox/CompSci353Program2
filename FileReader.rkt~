#lang racket

(provide refined-lines)

; pre  -- takes a string and a char to remove from the string
; post -- returns said string without any instances of char
(define (strip-char str char)
  (define char-list (string->list str))
  (define refined-list (filter
                        (lambda(c) (not (char=? c char)))
                        char-list))
  (define string-list (map string refined-list))
  (string-join string-list ""))

; pre  -- takes a list of strings
; post -- returns a list of lists of strings delineated by commas
;         and without quotation marks
(define (get-refined-lines lines)
  (define (strip-quotes str) ; use currying to simplify strip-char function
    (strip-char str #\"))
  (define (comma-split str)
    (string-split str ","))
  (define stripped-lines (map strip-quotes lines))
  (map comma-split stripped-lines))
  
  

(define raw-lines (file->lines "Video Games Sales.csv"))
(define refined-lines (get-refined-lines raw-lines))

(first refined-lines)


