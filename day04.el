;;; day04.el --- Advent of Code Day 4 -*- lexical-binding: t; -*-

(cl-defstruct vec2d data rows cols)

(defun vec2d-create (rows cols &optional initial-value)
  (let* ((size (* rows cols))
         (data (make-vector size initial-value)))
    (make-vec2d :data data :rows rows :cols cols)))

;; (defun vec2d-index (v x y)
;;   (when (>= x (vec2d-rows v))
;;     (error "out of range, x(%d) > rows(%d)" x (vec2d-rows v)))
;;   (when (>= y (vec2d-cols v))
;;     (error "out of range, y(%d) > cols(%d)" y (vec2d-cols v)))
;;   (+ (* y (vec2d-cols v)) x)) ;; FIXME wrong indexing

;; return nil for out of bounds, those are treated as empty
(defun vec2d-index (v x y)
  (and
   (>= x 0)
   (>= y 0)
   (< x (vec2d-rows v))
   (< y (vec2d-cols v))
   (+ (* x (vec2d-cols v)) y)))

;; allow reading out of bounds
(defun vec2d-ref (v x y)
  (let ((idx (vec2d-index v x y)))
    (when idx
      (aref (vec2d-data v) idx))))

;; this one should error when nil
(defun vec2d-set (v x y newval)
  "Set value at (X,Y) in vec2d V to NEWVAL."
  (aset (vec2d-data v)
        (vec2d-index v x y)
        newval))

(defun make-vec-from-input (lines)
  (let* ((rows (length lines))
         (cols (length (nth 0 lines)))
         (vec (vec2d-create rows cols)))
    (cl-loop for row from 0 below rows
             do (let ((row-str (pop lines)))
                  (cl-loop for col from 0 below cols
                           do (when (char-equal (aref row-str col) ?@)
                                (vec2d-set vec row col t)))))
    vec))

(defun count-occupied-neighbors (v x y)
  (cl-loop for dx in '(-1 0 1)
           sum (cl-loop for dy in '(-1 0 1)
                        count (and (not (and (zerop dx) (zerop dy)))
                                   (vec2d-ref v (+ x dx) (+ y dy))))))

(defun count-accessible (v)
  (cl-loop for x from 0 below (vec2d-rows v)
           sum (cl-loop for y from 0 below (vec2d-cols v)
                        count (when (vec2d-ref v x y)
                                (< (count-occupied-neighbors v x y) 4)))))

;; I think I can remove as I go rather than working in waves
;; but still need to do multiple passes (probably max of 2? think about that)
(defun remove-accessible (v)
  (cl-loop for x from 0 below (vec2d-rows v)
           sum (cl-loop for y from 0 below (vec2d-cols v)
                        when (and (vec2d-ref v x y)
                                  (< (count-occupied-neighbors v x y) 4))
                        do (vec2d-set v x y nil)
                        and count t)))

(defun make-vec (input)
  (make-vec-from-input (string-lines input)))

;; could probably do some sort of fill algo rather than checking them all?
(aoc-expect 'day4-part1 "13")
(defun day4-part1 (input)
  (number-to-string
   (count-accessible (make-vec input)))) ;; have to count since single pass

(aoc-expect 'day4-part2 "43")
(defun day4-part2 (input)
  (let* ((v (make-vec input))
         (r (remove-accessible v))
         (s 0))
    (while (> r 0)
      (message "r was %d" r)
      (setq s (+ s r))
      (setq r (remove-accessible v)))
    (number-to-string s)))


;; questions:
;; - does out of bounds count as empty? yes
