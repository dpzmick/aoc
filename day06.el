;;; day06.el --- Advent of Code Day 6 -*- lexical-binding: t; -*-

;; for some reason always getting an extra nil, just trim it off
(defun process-input (input)
  (butlast (cl-loop for line in (string-split input "\n")
                    collect (vconcat (string-split line)))))


(aoc-expect 'day6-part1 "4277556")
(defun day6-part1-inner (input)
  (let* ((p (process-input input))
         (numbers (vconcat (butlast p)))
         (ops (vconcat (car (last p)))))
    (cl-loop for op-idx from 0 below (length ops)
             sum (cl-loop for line from 1 below (length numbers)
                          with acc = (string-to-number (aref (aref numbers 0) op-idx))
                          do (let ((op (aref ops op-idx))
                                   (val (string-to-number (aref (aref numbers line) op-idx))))
                               (pcase-exhaustive op
                                 ("*" (setq acc (* acc val)))
                                 ("+" (setq acc (+ acc val)))))
                          finally (return acc)))))

(defun day6-part1 (input)
  (number-to-string (day6-part1-inner input)))

;; part2 is stupid
;; for some reason always getting an extra nil, just trim it off
;; only split at every 5th space

;; just clean it up first, this input format is insane
;; 
;; $ awk '{
;;   for (i = 1; i <= NF; i++) {
;;     printf "%-5s", $i
;;   }
;;   printf "\n"
;; }' inputs/day06.txt | less > inputs/day06_clean.txt

;; not really named correctly; each chunk is length n but there is a trailing
;; space in each, which we skip
;;
;; this is wrong, we should actually split on whitespace but then _assume_ each
;; chunk has (up to) 4 numbers in it, but do the cleanup first
(defun split-into-chunks (str n)
  (let ((n-chunks (/ (length str) n)))
    (cl-loop for chunk-idx from 0 to n-chunks
             ;; starts at n*idx, n-1
             collect (substring str
                                (* n chunk-idx)
                                (min 
                                 (length str)
                                 (1- (* n (1+ chunk-idx))))))))

(ert-deftest split-into-chunks-test ()
  (should (equal
           (split-into-chunks "xxxxYxxxxYxxxx" 5)
           '("xxxx" "xxxx" "xxxx"))))

(defun process-input2 (input)
  (butlast
   (cl-loop for line in (string-split input "\n")
            collect (vconcat (split-into-chunks line 5)))))

(process-input2 "xxxx xxxx xxxx\nyyyy yyyy yyyy\n*    -    *    \n")

(defun get-number (op-idx number-idx numbers)
  "Read a vertical number from character-column NUMBER-IDX (0..3) inside chunk OP-IDX."
  (let ((digit-idx (- 3 number-idx))
        (acc 0)
        (saw-digit nil))
    (cl-loop for row across numbers
             for cell = (aref row op-idx)  ;; a 4-char string
             for ch = (if (< digit-idx (length cell))
                          (aref cell digit-idx)
                        ?\s)
             when (and (>= ch ?0) (<= ch ?9))
             do (setq saw-digit t
                      acc (+ (* acc 10) (- ch ?0))))
    (when saw-digit acc)))

(defun vflat (a)
  (vconcat
   (cl-loop for x in a
            collect (vconcat x))))

;; (get-number 1 3
;;             (vflat
;;              '(("1234" "1234" "1234")
;;                ("   1" "2   " "    ")
;;                ("    " "3   " "    "))))

;; 4 digits per column
;; 4 rows
;;
;; so op-idx 0 is the 0th column
;; there are 4 numbers in this column

(defun get-number2 (op-idx number-idx numbers op)
  (or
   (get-number op-idx number-idx numbers)
   (pcase-exhaustive op
     ("*" 1)
     ("+" 0))))

;;(aoc-expect 'day6-part2 "3263827") ;; sample needs different constants
(defun day6-part2 (input)
  (let* ((p (process-input input))
         (numbers (vconcat (butlast p)))
         (ops (vconcat (car (last p)))))
    (cl-loop for op-idx from 0 below (length ops)
             for op = (string-trim (aref ops op-idx))
             for vals = (cl-loop for number-idx from 0 below 4
                                 collect (get-number2 op-idx number-idx numbers op))
             sum (pcase-exhaustive op
                   ("*" (apply #'* vals))
                   ("+" (apply #'+ vals))))))

(kill-new (number-to-string (day6-part2 (aoc-read-input 6))))
