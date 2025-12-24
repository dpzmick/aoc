;;; day03.el --- Advent of Code Day 3 -*- lexical-binding: t; -*-
(require 'ert)

;; each line is a bank
;; in each bank, turn on exactly 2 batteries
;; the joltage of the bank is eq to the number formed by the digits that are turned on

;; could be a mask?
(defun bank-val (bank d1 d2)
  (string-to-number
   ;; could skip allocation and just extract these from raw digit using math?
   (format "%s%s"
           (substring bank d1 (1+ d1))
           (substring bank d2 (1+ d2)))))

;; indexing in the doc is 1 based; I'm doing zero indexing
(ert-deftest bank-val ()
  (should (eq (bank-val "12345" 1 3) 24)))

;; find the largest possible joltage each bank can produce
;; total output is the sum of the max from each bank
;; 
;; notice, the pick the highest leading digit first, then highest after?
;; this will always be the highest b.c. the expression is x*10 + y, and y<10

;; to find max, either:
;; - it is the first 9 we found (shortcut? or iterate down the list)
;; - it is the first occurance of the max val in the string
;;
;; what's longer, 1-9 or the strings?
;; the banks are fairly long, so searching them over and over again isn't ideal
;; so we want first occureance of largest number we've found so far

;; vectors are strings so this is fine
(defun argmax-first (vec)
  (cl-loop for i from 0 below (length vec)
           with maxidx = nil
           do (when
                  (or (not maxidx) 
                      (> (aref vec i) (aref vec maxidx)))
                (setq maxidx i))
           finally return maxidx))

;; bank is a string, so indexing is fine
(defun max-bank (bank)
  (let* ((bank-str (number-to-string bank))
         ;; cannot select max char because we need at least one char to make up the second char
         (first-best-idx (argmax-first (substring bank-str 0 (1- (length bank-str)))))
         ;; find the best of the rest, the argmax is over a substring, so must account for that
         (second-best-idx (+
                           (1+ first-best-idx)
                           (argmax-first (substring bank-str (1+ first-best-idx))))))
    (string-to-number (format "%c%c"
                              (aref bank-str first-best-idx)
                              (aref bank-str second-best-idx)))))

;; cannot quite just sort them, because of the 81819 -> 89 case

(ert-deftest max-bank ()
  (should (eq (max-bank 1111111) 11))
  (should (eq (max-bank 1111119) 19))
  (should (eq (max-bank 81819) 89))
  (should (eq (max-bank 987654321111111) 98))
  (should (eq (max-bank 811111111111119) 89))
  (should (eq (max-bank 234234234234278) 78)) ;; max number is last char
  (should (eq (max-bank 818181911112111) 92)))

(aoc-expect 'day3-part1 "357")
;; I guess once I get part1, I know the value and could use it for part2
;; then could refactor it, so maybe I should add the type back to expect
(defun day3-part1 (input)
  (number-to-string
   (seq-reduce
    (lambda (acc line) (+ acc (max-bank (string-to-number line))))
    (string-lines input)
    0)))

;; part2 is the max subject to there being 12 left over, so just do it 12 times?

(defun argmax-inbounds (seq st ed)
  (+ st
     (argmax-first (substring seq st (1+ ed)))))

(defun max-bank-p2-helper (bank-str cnt)
  (let ((len (length bank-str)))
    (cl-loop for i from 0 below cnt
             for window-size = (- cnt i)
             for start = 0 then (1+ selected)
             for end = (- len (- cnt i))
             for selected = (argmax-inbounds bank-str start end)
             collect selected)))

(max-bank-p2-helper "81819" 3); 889 / (0, 2, 4)

(defun max-bank-p2-cnt (bank cnt)
  (let* ((bank-str (number-to-string bank))
         (idx-list (max-bank-p2-helper bank-str cnt)))
    (string-to-number
     (mapconcat (lambda (i) (string (aref bank-str i))) idx-list))))

(defun max-bank-p2 (bank)
  (max-bank-p2-cnt bank 12))

(ert-deftest max-bank-p2 ()
  (should (eq (max-bank-p2-cnt 81819 3) 889))
  (should (eq (max-bank-p2 987654321111111) 987654321111))
  (should (eq (max-bank-p2 811111111111119) 811111111119))
  (should (eq (max-bank-p2 234234234234278) 434234234278))
  (should (eq (max-bank-p2 818181911112111) 888911112111)))

(aoc-expect 'day3-part2 "3121910778619")
(defun day3-part2 (input)
  (number-to-string
   (seq-reduce
    (lambda (acc line) (+ acc (max-bank-p2 (string-to-number line))))
    (string-lines input)
    0)))


;; question:
;; - are any banks 0? (no)
;; - should avoid using strings to speed anything up?
