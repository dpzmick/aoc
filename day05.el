;;; day05.el --- Advent of Code Day 5 -*- lexical-binding: t; -*-
(require 'ert)

;; input is ranges and list of ids
;; want to count the ids in the list that are contained in the ranges
;; there are more _ranges_ than input ids, not sure about the range values though

;; lets gather basic stats
(defun process-ranges (ranges-section) 
  (mapcar (lambda (line)
            (mapcar #'string-to-number (split-string line "-")))
          (split-string ranges-section "\n" t)))

(defun process-ids (idx-section)
  (mapcar #'string-to-number 
          (split-string idx-section "\n" t)))

;; FIXME should probably validate the ranges but AOC input is usually clean
(defun process-input (input)
  (let* ((sections (split-string input "\n\n" t))
         (ranges (process-ranges (car sections)))
         (numbers (process-ids (cadr sections))))
    `(,ranges . ,numbers)))

(defun sort-ranges (ranges)
  (sort ranges
        (lambda (x y)
          (or (< (car x) (car y))
              (and (= (car x) (car y))
                   (< (cadr x) (cadr y)))))))

(ert-deftest sort-ranges ()
  (should (equal
           (sort-ranges '((1 1) (1 2)))
           '((1 1) (1 2))))
  (should (equal
           (sort-ranges '((2 1) (1 2)))
           '((1 2) (2 1))))
  (should (equal
           (sort-ranges '((1 2) (1 1)))
           '((1 1) (1 2)))))

;; assumes sorted
;; keep going until the end of the current range
(defun merge-ranges (ranges)
  (let ((curr-st nil)
        (curr-ed nil)
        (lst nil))
    (dolist (item ranges)
      (let ((item-st (car item))
            (item-ed (cadr item)))
        (progn ;; FIXME required?
          (when (not curr-st)
            (setq curr-st (car item)
                  curr-ed (cadr item)))
          ;; cases:
          ;; fully-contained -> ignore
          ;; item-st > curr-ed -> new range
          ;; starts inside, but extends past end -> extend range
          (cond
           ;; new element, put at front, do this check first
           ((< curr-ed item-st)
            (setq lst (cons (cons curr-st (cons curr-ed nil)) lst)
                  curr-st item-st
                  curr-ed item-ed))

           ;; we know st <= curr-st already from sort
           ((< curr-ed item-ed)
            (setq curr-ed item-ed))))))
    (nreverse
     ;; add whatever is left over
     (cons (cons curr-st (cons curr-ed nil)) lst))))

(ert-deftest merge-ranges ()
  (should (equal
           (merge-ranges '((1 10) (2 4)))
           '((1 10))))

  (should (equal
           (merge-ranges '((1 10) (14 17)))
           '((1 10) (14 17))))

  (should (equal
           (merge-ranges '((1 10) (2 4) (14 17)))
           '((1 10) (14 17))))

  (should (equal
           (merge-ranges '((1 10) (2 12) (14 17)))
           '((1 12) (14 17))))

  (should (equal
           (merge-ranges '((1 10) (2 14) (14 17)))
           '((1 17)))))

(defun range-direction (range value)
  (let ((st (car range))
        (ed (cadr range)))
    (cond
     ((and (<= st value) (<= value ed)) 'contained)
     ((< value st) 'lower)
     ((< ed value) 'higher))))

;; this should not be a cons list, convert to vector
;; but it was fast enough anyway.. oh well
(defun is-contained (ranges value)
  (cl-loop
   with idx-min = 0
   with idx-max = (1- (length ranges))
   while (<= idx-min idx-max)
   for idx-mid = (+ idx-min (/ (- idx-max idx-min) 2))
   for mid-range = (nth idx-mid ranges)
   do (pcase-exhaustive (range-direction mid-range value)
        ('contained (cl-return t))
        ('lower (setq idx-max (- idx-mid 1)))
        ('higher (setq idx-min (+ idx-mid 1))))
   finally return nil))

(aoc-expect 'day5-part1 "3")
(defun day5-part1 (input)
  (let* ((p (process-input input))
         (ranges (merge-ranges (sort-ranges (car p)))) ;; sort destroys (car p)
         (values (cdr p)))
    (number-to-string
     (cl-loop for value in values
              count (is-contained ranges value)))))

(aoc-expect 'day5-part2 "14")
(defun day5-part2 (input)
  (let* ((p (process-input input))
         (ranges (merge-ranges (sort-ranges (car p)))))
    (number-to-string
     (cl-loop for range in ranges
              sum (1+ (- (cadr range) (car range)))))))


;; thought about using an interval tree but figured it was not needed b.c. I do
;; not need all of the ranges. This is the same rough speed (other than the
;; lists being used)
