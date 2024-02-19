#lang racket

(provide data)

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

; pre  -- takes a list of strings
; post -- fixes elements that were improperly split (e.g. if there
;         was a comma in the title)
(define (fix-line lst)
  (if (not (eq? (length lst) 13))
      
      (let ([complete-title (string-append (third lst) "," (fourth lst))])
        (append (take lst 2) (list complete-title) (drop lst 4)))

      lst))


(define raw-lines (file->lines "Video Games Sales.csv"))
(define refined-lines (get-refined-lines raw-lines))
(define data (map fix-line refined-lines))