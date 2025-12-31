;;; day01.el --- Advent of Code Day 1 -*- lexical-binding: t; -*-

(require 'aoc)

(defun new-val (val direction amount)
  (pcase-exhaustive direction
      ("L" (mod (- val amount) 100))
      ("R" (mod (+ val amount) 100))))

;; (defun zero-crossings (val direction amount)
;;   (pcase-exhaustive direction
;;     ("L" (let ((first-zero (if (zerop val) 100 val)))
;;            (if (< amount first-zero)
;;                0
;;              (1+ (/ (- amount first-zero) 100)))))
;;     ("R" (/ (+ val amount) 100))))

(defun bval (v) (if v 1 0))
(defun zero-crossings (val direction amount)
  (+ (/ amount 100)
     (pcase-exhaustive direction
       ("L" (bval (< val (new-val val direction amount))))
       ("R" (bval (> val (new-val val direction amount)))))))

(zero-crossings 50 "L" 1000)
(zero-crossings 99 "R" 1)

;; 1/100 = 0
;; val=99 new-val=0, dir=R
;; (> 99 0)

(defun solve (lines)
  (let ((dial 50)
        (zeros 0))
    (dolist (line lines)
      (let ((direction (substring line 0 1))
            (amount (string-to-number (substring line 1))))
        (setq dial (new-val dial direction amount))
        (if (= dial 0)
            (setq zeros (+ 1 zeros)))))
    zeros))

(defun solve-zero-crossings (lines)
  (let ((dial 50)
        (zeros 0))
    (dolist (line lines)
      (let ((direction (substring line 0 1))
            (amount (string-to-number (substring line 1))))
        (setq zeros (+ zeros (zero-crossings dial direction amount)))
        (setq dial (new-val dial direction amount))))
    zeros))

(+
 (solve (string-lines (aoc-read-input 1)))
 (solve-zero-crossings (string-lines (aoc-read-input 1))))


;; FIXME need to do part2
;; count number of times we cross zero during `new-val' call
;; can probalby just loop again?
