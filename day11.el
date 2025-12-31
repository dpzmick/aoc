;;; day11.el --- Advent of Code Day 11 -*- lexical-binding: t; -*-

;; structure will be adjacency matrix
;; I guess its time for matrix struct again

(cl-defstruct mat data rows cols)

(defun mat-create (rows cols &optional initial-value)
  (let* ((size (* rows cols))
         (data (make-vector size initial-value)))
    (make-mat :data data :rows rows :cols cols)))

(defun mat-from (rows cols vec-of-vec)
  (cl-loop with ret = (mat-create rows cols)
           for r from 0 below rows
           do (cl-loop for c from 0 below cols
                       do (mat-set ret r c
                                   (aref (aref vec-of-vec r) c)))
           finally return ret))

(defun mat-inbounds (m x y)
  (and
   (>= x 0)
   (>= y 0)
   (< x (mat-rows m))
   (< y (mat-cols m))))

;; col major, didn't think hard about this
;; (defun mat-index (m x y)
;;   (when (not (mat-inbounds m x y))
;;     (error "out of bounds"))
;;    (+
;;     x
;;     (* y (mat-rows m))))
(defun mat-index (m x y)
  (+ (* x (mat-cols m)) y))  ; index = x * cols + y (ROW-MAJOR)

(defun mat-ref (mat x y)
  (aref (mat-data mat) (mat-index mat x y)))

(defun mat-set (m x y newval)
  (aset (mat-data m)
        (mat-index m x y)
        newval))

(defun show-mat (m)
  (with-current-buffer (get-buffer-create "*mat*")
    (erase-buffer)
    (cl-loop for r from 0 below (mat-rows m) do
             (cl-loop for c from 0 below (mat-cols m) do
                      (insert (format "%2d" (mat-ref m r c))))
             (insert "\n"))
    (goto-char (point-min))
    (pop-to-buffer (current-buffer))))

;; two steps for processing
;; 1. get all the node names and assign them ids; figure out how many there are
;; 2. make matrix

(defun parse-line (line)
  (let* ((parts (string-split line ":"))
         (src (nth 0 parts))
         (dsts (string-split (string-trim (nth 1 parts)) " ")))
    (list src dsts)))

(defun intern-name (name table)
  (unless (hash-table-contains-p name table)
    (puthash name (hash-table-count table) table)))

(defun make-node-table (input)
  (cl-loop
   with ret = (make-hash-table :test 'equal)
   for line in (string-split input "\n" t)
   do (seq-let (src dsts) (parse-line line)
        (intern-name src ret)
        (mapc (lambda (e) (intern-name e ret)) dsts))
   finally return ret))

(defun fill-mat-line (line adj table)
  (seq-let (src dsts) (parse-line line)
    (cl-loop
     with row = (gethash src table)
     for dst in dsts
     for col = (gethash dst table)
     do (mat-set adj row col 1))))

;; I will need the table, at least for "you" and "out" node names
(defun process-input (input)
  (cl-loop
   with table = (make-node-table input)
   with adj = (mat-create (hash-table-count table) (hash-table-count table) 0)
   for line in (string-split input "\n" t)
   do (fill-mat-line line adj table)
   finally return (cons table adj)))

;; okay, now we need all unique paths. That's a DFS ideally so we can return the
;; total at the end
;;
;; I do not think there can be any loops b.c. the problem would be ill formed if
;; there were (there would be infnite paths)
;;
;; going to try writing recursively, to do it iteratively I'd need a stack anyway

(defun dfs (from to adj)
  (if (= from to)
      ;; one path from self to dst
      1
    ;; sum of paths from children
    (cl-loop for i from 0 below (mat-cols adj)
           when (= 1 (mat-ref adj from i))
           sum (dfs i to adj))))

(aoc-expect 'day11-part1 "5")
(defun day11-part1 (input)
  (let*
      ((d (process-input input))
       (table (car d))
       (adj (cdr d))
       (you (gethash "you" table))
       (out (gethash "out" table)))
      (number-to-string (dfs you out adj))))

;;(day11-part1 (aoc-read-sample 11))

;; I am a bit disappointed that this worked for p1
;; at least p2 didn't work trivially

(defun dfs-p2 (from to dac-id fft-id adj)
  (let ((memo (make-hash-table :test 'equal)))
    (dfs-p2--inner from to dac-id fft-id nil nil adj memo)))

(defun dfs-p2--inner (from to dac-id fft-id seen-dac seen-fft adj memo)
  (let* ((seen-dac (or seen-dac (= from dac-id)))
         (seen-fft (or seen-fft (= from fft-id)))
         (key (list from seen-dac seen-fft))
         (cached (gethash key memo)))
    (if cached
        cached
      (puthash key
               (if (= from to)
                   (if (and seen-dac seen-fft) 1 0)
                 (cl-loop for i from 0 below (mat-cols adj)
                          when (= 1 (mat-ref adj from i))
                          sum (dfs-p2--inner i to dac-id fft-id
                                             seen-dac seen-fft adj memo)))
               memo))))

(aoc-expect 'day11-part2 "2")
(defun day11-part2 (input)
  (let*
      ((d (process-input input))
       (table (car d))
       (adj (cdr d))
       (svr (gethash "svr" table))
       (out (gethash "out" table))
       (dac (gethash "dac" table))
       (fft (gethash "fft" table)))
    (number-to-string (dfs-p2 svr out dac fft adj))))

(day11-part2 (aoc-read-sample 11))
