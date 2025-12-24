;;; day02.el --- Advent of Code Day 2 -*- lexical-binding: t; -*-
(require 'ert)

;; invalid is: any ID which is made only of some sequence of digits repeated twice
;; None of the numbers have leading zeroes

;; (defun is-mirrored-over-i (str i)
;;   (string= (substring str 0 i) (substring str i)))

;; (ert-deftest mirrored-over-i ()
;;   ;; theirs
;;   (should (is-mirrored-over-i "11" 1))
;;   (should (is-mirrored-over-i "22" 1))
;;   (should (is-mirrored-over-i "99" 1))
;;   (should (is-mirrored-over-i "1010" 2))
;;   (should (is-mirrored-over-i "1188511885" 5))
;;   ;; mine
;;   (should (is-mirrored-over-i "1414" 2))
;;   (should-not (is-mirrored-over-i "1414" 3))
;;   (should-not (is-mirrored-over-i "1415" 2)))

;; (defun valid-id-p (number)
;;   "Check if id is well formed."
;;   (let ((str (number-to-string number)))
;;     (not (is-mirrored-over-i str (/ (length str) 2)))))

;; FIXME do not allow i<=1?
(defun repeated-i-times (n i)
  (let* ((len (1+ (floor (log n 10))))
         (part-len (/ len i))
         (divisor (expt 10 part-len))
         (repeater (mod n divisor)))
    (and
     ;; evenly divisible
     (zerop (mod len i))
     ;; parts all match?
     (cl-loop with v = n
              while (> v 0)
              always (prog1
                         (= (mod v divisor) repeater)
                       (setq v (/ v divisor)))))))

(repeated-i-times 12345 2)

(defun valid-id-p (n)
  (not (repeated-i-times n 2)))

(defun valid-id-p2-p (n)
  (not
   (let ((len (1+ (floor (log n 10)))))
     ;; cannot be repeated 1 time, then 12345 is the first element of 12345, and
     ;; there's none left to check against
     (cl-loop for i from 2 to len
              thereis (repeated-i-times n i)))))

(ert-deftest valid-id ()
  (should-not (valid-id-p 11))
  (should-not (valid-id-p 1188511885))
  (should-not (valid-id-p 38593859))
  (should-not (valid-id-p 1111))
  (should (valid-id-p 824824824)) ;; different!
  (should (valid-id-p 12345)))

(ert-deftest valid-id-p2 ()
  (should-not (valid-id-p2-p 67676767))
  (should-not (valid-id-p2-p 111111))
  (should-not (valid-id-p2-p 824824824))
  (should (valid-id-p2-p 12345)))

(defun check-range (st ed)
  (cl-loop for id from st to ed
           unless (valid-id-p id)
           sum id))

(defun check-range-p2 (st ed)
  (cl-loop for id from st to ed
           unless (valid-id-p2-p id)
           sum id))

(defun parse-range (s)
  (let* ((parts (string-split s "-"))
         (st-str (nth 0 parts))
         (ed-str (nth 1 parts)))
    (cons (string-to-number st-str) (string-to-number ed-str))))

(aoc-expect 'day02-part1 "1227775554")
(defun day02-part1 (input)
  (number-to-string
   (seq-reduce
    (lambda (acc c) (+ acc (check-range (car c) (cdr c))))
    (seq-map 'parse-range (string-split input ","))
    0)))

(aoc-expect 'day02-part2 "4174379265")
(defun day02-part2 (input)
  (number-to-string
   (seq-reduce
    (lambda (acc c) (+ acc (check-range-p2 (car c) (cdr c))))
    (seq-map 'parse-range (string-split input ","))
    0)))
