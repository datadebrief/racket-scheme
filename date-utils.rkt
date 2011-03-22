#lang racket

(require racket/date)

(define day-secs (* 24 60 60))

(define (make-day dd mm yyyy) (seconds->date (find-seconds 0 0 0 dd mm yyyy)))

(define (date-diff-days date1 date2)  (/ (- (date->seconds date1)
                                            (date->seconds date2))
                                         day-secs))
(define (LeapYear? yyyy)
  (cond
    [(= 0 (modulo yyyy 400)) #t]
    [(= 0 (modulo yyyy 100)) #f]
    [(= 0 (modulo yyyy 4)) #t]
    [else #f]))

(define (add-days start-date n)
  (seconds->date (+ (* n day-secs) (date->seconds start-date))))


; Examples
; Example 1 "difference in days between dates"
(display "example 1 - difference in days between dates") (newline)
(define d1 (make-day 20 3 2011))
(define d2 (make-day 20 3 2010))
(display (string-append (number->string (date-diff-days d1 d2)) " days"))(newline)

; Example 2 "list of leap years"
(display "example 2 - list of leap years") (newline)
(define (enum-leap-years start n m leap-years)
  (cond
    [(= n m) (flatten leap-years)]
    [else (enum-leap-years start n (+ m 1) (if (LeapYear? (+ start m))
                                               (cons leap-years (+ start m))
                                               leap-years ))]))
(let ((x (enum-leap-years 1970 (- 2070 1970) 0 '())))
  (display "number of leap years - ") (display (length x)) (newline)
  x)

; Example 3 "date of add n number of days to date"
(display "example 3 - date of add n number of days to date") (newline)
(add-days (make-day 22 2 2011) 7)
(add-days (make-day 1 3 2011) -7)