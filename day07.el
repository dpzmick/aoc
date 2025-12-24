;;; day07.el --- Advent of Code Day 7 -*- lexical-binding: t; -*-

(cl-defstruct vec2d data rows cols)

(defun vec2d-create (rows cols &optional initial-value)
  (let* ((size (* rows cols))
         (data (make-vector size initial-value)))
    (make-vec2d :data data :rows rows :cols cols)))

(defun vec2d-index (v x y)
  (when (not
         (and
          (>= x 0)
          (>= y 0)
          (< x (vec2d-rows v))
          (< y (vec2d-cols v))))
    (error "out of bounds"))
   (+ (* x (vec2d-cols v)) y))

(defun vec2d-ref (v x y) (aref (vec2d-data v) (vec2d-index v x y)))

(defun vec2d-set (v x y newval)
  (aset (vec2d-data v)
        (vec2d-index v x y)
        newval))

(defun vec2d-row (v x)
  (seq-subseq
   (vec2d-data v)
   (* x (vec2d-cols v))
   (+
    (* x (vec2d-cols v))
    (vec2d-cols v))))

(defun get-char (row col lines)
  (pcase-exhaustive (aref (aref lines row) col)
    (?. nil)
    (?S 'start)
    (?^ 'splitter)))

(defun process-input (input)
  (let* ((lines (vconcat (split-string input)))
         (rows (length lines))
         (cols (length (aref lines 0)))
         (vec (vec2d-create rows cols)))
    (cl-loop for row from 0 below rows
             do (cl-loop for col from 0 below cols
                         do (vec2d-set vec row col (get-char row col lines))))
    vec))

(defun find-start (vec)
  (cl-loop for row from 0 below (vec2d-rows vec)
           thereis (cl-loop for col from 0 below (vec2d-cols vec)
                       thereis (when (equal 'start (vec2d-ref vec row col))
                                 (cons row col)))))


;; state:     ...t...
;; next row:  ...^...
;; output:    ..t.t..
(defun next-row (this-row-lazers next-row)
  (let ((output (make-vector (length next-row) nil))
        (cnt 0))
    (cl-loop for i from 0 below (length next-row)
             do (when (aref this-row-lazers i)
                    (if (eq (aref next-row i) 'splitter)
                        (progn
                          (cl-incf cnt)
                          (when (>= (1- i) 0)
                            (aset output (1- i) t))
                          (when (< (1+ i) (length output))
                            (aset output (1+ i) t)))
                      (aset output i t))))
    (cons output cnt)))

;; the final count is the number of times a split occurs
;; not the final number of lasers
;;
;; this is the annoyingest value to track, naturally.
;; does a split count if it goes off the board?
;;
;; need to count the number of times we encounter a splitter
;; not total number of lasers before/after (which is less?)
;;
;; this is the number of internal nodes

(aoc-expect 'day7-part1 "21")
(defun day7-part1 (input)
  (number-to-string
  (let* ((vec (process-input input))
         (rows (vec2d-rows vec))
         (cols (vec2d-cols vec))
         (start-y (cdr (find-start vec)))
         (lasers (make-vector cols nil)))
    (aset lasers start-y t)
    (cl-loop for row from 1 below rows
             sum (let* ((res (next-row lasers (vec2d-row vec row)))
                        (split-count (cdr res))
                        (next-lasers (car res)))
                    (setq lasers next-lasers)
                    split-count)))))

;; its a dag, not a binary tree, consider
;; ..S..   ->      |
;; ..^..          | |
;; .^.^.         | | |
;;
;; and we counted the splitters. But, if we redefine "node" we might still get
;; binary tree. What if we count the _splits_ themselves?
;; ..S..   ->      |        -> |        
;; ..^..          | |         | |
;; .^...         | ||        | | |
;; ^..^.        | || |      |
;;
;; but tracking the actual leaf count isn't dramatically easier than just
;; counting because the structure isn't known. We can just mathematically
;; transform from splitter count into this tree.

;; same next row but use counts instead of just t/f
;; do not need output count
(defun next-row-counting (this-row-lazers next-row)
  (let ((output (make-vector (length next-row) 0)))
    (cl-loop for i from 0 below (length next-row)
             for cnt = (aref this-row-lazers i)
             do (when (> cnt 0)
                    (if (eq (aref next-row i) 'splitter)
                        (progn
                          (when (>= (1- i) 0)
                            (cl-incf (aref output (1- i)) cnt)
                          (when (< (1+ i) (length output))
                            (cl-incf (aref output (1+ i)) cnt))))
                      (cl-incf (aref output i) cnt))))
    output))

(aoc-expect 'day7-part2 "40")
(defun day7-part2 (input)
  (number-to-string
   (let* ((vec (process-input input))
          (rows (vec2d-rows vec))
          (cols (vec2d-cols vec))
          (start-y (cdr (find-start vec)))
          (lasers (make-vector cols 0)))
     (aset lasers start-y 1)
     (cl-loop for row from 1 below rows
              do (setq lasers (next-row-counting lasers (vec2d-row vec row))))
     (cl-loop for c across lasers sum c))))

(day7-part2 (aoc-read-sample 7))
