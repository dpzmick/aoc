;;; day08.el --- Advent of Code Day 8 -*- lexical-binding: t; -*-

;; always find and connect the smallest distance coordinates
;; at least one of the selected ones must be unconnected
;; once all boxes are connected _to something_, we are done

;; structures are:
;; - all the coordinates (1000 of them)
;; - circuit assignment
;;
;; I think just find all pairs (500k of them) and compute distance
;; sort by distance
;;
;; iterate in that order assigning boxes to ciruits; we can probably get away
;; with it, the list isn't that large.

(cl-defstruct point x y z)

(defun distance (a b)
  (sqrt
   (+
    (expt (- (point-x a) (point-x b)) 2)
    (expt (- (point-y a) (point-y b)) 2)
    (expt (- (point-z a) (point-z b)) 2))))

(defun process-input (input)
  (cl-loop for line in (split-string input)
           collect (let ((parts (split-string line ",")))
                     (make-point
                      :x (string-to-number (nth 0 parts))
                      :y (string-to-number (nth 1 parts))
                      :z (string-to-number (nth 2 parts))))))


(defun mag (c)
  (distance (make-point :x 0 :y 0 :z 0) c))

;; make list (vec?) of all pairs
(defun all-pairs (lst)
  (cl-loop for a in lst
           nconc (cl-loop for b in lst
                          when (and
                                (not (equal a b))
                                (> (mag a) (mag b)))
                          collect (cons a b))))
                            

(defun sorted-by-distance (pairs)
  (sort pairs :key
        (lambda (p)
          (distance
           (car p)
           (cdr p)))))

;; this is rather inefficient, should do union-find structure here
;; the 10/1000 conection limit means this never gets big enough for us to care..
;; boring!
;; even for the larger p2 problem this is still adequately fast, annoying
(defun merge-circuits (ac bc circuits)
  ;; sketchy, but only modifying values
  ;; can I get a place in maphash? NO
  (maphash
   (lambda (k v)
     (when (equal v bc)
       (puthash k ac circuits)))
   circuits))

(defun connect (a b circuits)
  (let* ((ac (gethash a circuits))
         (bc (gethash b circuits))
         (new-id (hash-table-count circuits)))
    (cond
     ;; both already in something
     ((and ac bc)
      (when (not (= ac bc))
        (merge-circuits ac bc circuits)))
     ;; cases where only one is already in something
     (ac ;; implies (not bc)
      (puthash b ac circuits))
     (bc ;; implies (not ac)
      (puthash a bc circuits))
     ;; neither in a circuit, make new one and put both in it
     (t
      (progn
        (puthash a new-id circuits)
        (puthash b new-id circuits))))))

(defun do-connects (input)
  (let ((data (take 1000 ;; NOTE have to change this for real data
                    (sorted-by-distance
                     (all-pairs
                      (process-input input)))))
        (circuits (make-hash-table)))
    (cl-loop for pair in data
             do (let* ((a (car pair))
                       (b (cdr pair)))
                  (connect a b circuits))
             finally return circuits)))

(defun inchash (k h)
  (puthash k (1+ (gethash k h 0)) h))

(defun counts (circuits)
  (let ((counts (make-hash-table)))
    (maphash
     (lambda (k v) (inchash v counts))
     circuits)
    counts))

(aoc-expect 'day8-part1 "40")
(defun day8-part1 (input)
  (number-to-string
   (apply #'*
          (take 3
                (sort (hash-table-values
                       (counts (do-connects input)))
                      :reverse t)))))

(defun make-init-table (data)
  (cl-loop with circuits = (make-hash-table)
           for pair in data
           for i from 0
           do (puthash (car pair) i circuits)
           finally return circuits))

;; keep going until all in the same circuit
;; 
;; cannot use the same hash table nil thing I used before, so have to also init
;; the table with one group per point, then do the merges
(defun do-connects-p2 (input)
  (let* ((data (sorted-by-distance
                (all-pairs
                 (process-input input))))
        (circuits (make-init-table data)))
    (cl-loop for pair in data
             do (let* ((a (car pair))
                       (b (cdr pair)))
                  (connect a b circuits)
                  (when (= 1 (hash-table-count (counts circuits)))
                    (cl-return pair))))))

(aoc-expect 'day8-part2 "25272")
(defun day8-part2 (input)
  (let ((pair (do-connects-p2 input)))
    (message "last two are %s" pair)
    (number-to-string
     (*
      (point-x (car pair))
      (point-x (cdr pair))))))
