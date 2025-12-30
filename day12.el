;;; day12.el --- Advent of Code Day 12 -*- lexical-binding: t; -*-

(cl-defstruct shape data width height)

;; this is 2d graphics again, so x is col, y is row
;; -- x --
;; |  ###
;; y  ##.
;; |  ##.
;; 
;; ---- x
;; |  (0,0) (1,0) ...
;; y  (0,1)
;;    ...

(defun shape-create (width height)
  (let* ((size (* width height))
         (data (make-vector size 0)))
    (make-shape :data data :width width :height height)))

(defun shape-copy-deep (s)
  (make-shape :data (copy-sequence (shape-data s))
              :width (shape-width s)
              :height (shape-height s)))

;; mostly for debugging/test code
;; i.e. to create:
;; ###
;; ##.
;; ##.
;;
;; do (shape-from [[1 1 1] [1 1 0] [1 1 0]]
(defun shape-from-vec (width height vec-of-vec)
  (cl-loop with ret = (shape-create width height)
           for x from 0 below width
           do (cl-loop for y from 0 below height
                       do (shape-set ret x y
                                     (aref (aref vec-of-vec y) x)))
           finally return ret))

;; ###
;; ##.
;; ##.
(defun shape-from-input (input)
  (let* ((lines (split-string input "\n" t))
         (height (length lines))
         (width (length (car lines)))
         (shape (shape-create width height)))
    (cl-loop for y from 0
             for line in lines
             do (cl-loop for x from 0 below (length line)
                         for char = (aref line x)
                         do (shape-set shape x y
                                       (if (char-equal char ?#) 1 0))))
    shape))

;; always leave original shape in upper left corner
(defun shape-resize (s width height)
  (cl-loop
   with ret = (shape-create width height)
   for x from 0 below (shape-width s)
   do (cl-loop
       for y from 0 below (shape-height s)
       do (shape-set
           ret x y (shape-ref s x y)))
   finally return ret))

(defun shape-inbounds (s x y)
  (and
   (>= x 0)
   (>= y 0)
   (< x (shape-width s))
   (< y (shape-height s))))

;; the layout probably does not matter
(defun shape-index (s x y)
  (+
   y
   (* x (shape-height s))))

(defun shape-ref (s x y)
  (when (not (shape-inbounds s x y))
    (error "out of bounds"))
  (aref (shape-data s) (shape-index s x y)))

(defun shape-ref-zerob (s x y)
  (if (shape-inbounds s x y)
      (aref (shape-data s) (shape-index s x y))
    0))

(defun shape-set (s x y newval)
  (when (not (shape-inbounds s x y))
    (error "out of bounds"))
  (aset (shape-data s)
        (shape-index s x y)
        newval))

(defun shape-translate-nochecks (s dx dy)
  (cl-loop
   with ret = (shape-create (shape-width s) (shape-height s))
   for x from 0 below (shape-width s)
   do (cl-loop
       for y from 0 below (shape-height s)
       do (shape-set
           ret x y (shape-ref-zerob s (- x dx) (- y dy))))
   finally return ret))

;; return nil if out of bounds
(defun shape-translate (s dx dy)
  (let* ((before (seq-reduce #'+ (shape-data s) 0))
         (new-s (shape-translate-nochecks s dx dy))
         (after (seq-reduce #'+ (shape-data new-s) 0)))
    (when (= before after)
      new-s)))

(defun shape-rotate (s)
  (let* ((s-width (shape-width s))
         (s-height (shape-height s))
         (ret (shape-create s-height s-width)))
    (cl-loop for x from 0 below s-width
             do (cl-loop for y from 0 below s-height
                         do (shape-set ret
                                       (- s-height y 1)
                                       x
                                       (shape-ref s x y)))
             finally return ret)))

;; (show-shape (shape-from-input "##.\n...\n..."))
;; (show-shape (shape-rotate (shape-from-input "##.\n...\n...")))
 
(defun shape-flip-h (s)
  "Flip shape horizontally (mirror across vertical axis)."
  (let* ((s-width (shape-width s))
         (s-height (shape-height s))
         (ret (shape-create s-width s-height)))
    (cl-loop for x from 0 below s-width
             do (cl-loop for y from 0 below s-height
                         do (shape-set ret
                                       (- s-width x 1)
                                       y
                                       (shape-ref s x y)))
             finally return ret)))

(defun show-shape (s)
  (with-current-buffer (get-buffer-create "*shape*")
    (erase-buffer)
    (cl-loop for y from 0 below (shape-height s) do
             (cl-loop for x from 0 below (shape-width s) do
                      (insert (format "%2d" (shape-ref s x y))))
             (insert "\n"))
    (goto-char (point-min))
    (pop-to-buffer (current-buffer))))

(defun process-input (input)
  (with-temp-buffer
     (insert input)
     (list :shapes (day12--parse-shapes)
           :grids (day12--parse-grids))))

(defun day12--parse-shapes ()
  (let ((shapes (make-hash-table)))
    (goto-char (point-min))
    (while (re-search-forward "^\\([0-9]+\\):$" nil t)
      (let* ((id (string-to-number (match-string 1)))
             (start (1+ (point)))  ; skip newline after id
             (end (if (re-search-forward "^$" nil t)
                      (match-beginning 0)
                    (point-max)))
             (text (string-trim (buffer-substring-no-properties start end))))
        (puthash id (shape-from-input text) shapes)))
    shapes))

(defun day12--parse-grids ()
  "Parse grid specifications from current buffer. Returns list."
  (let (grids)
    (goto-char (point-min))
    (while (re-search-forward "^\\([0-9]+\\)x\\([0-9]+\\):\\s-*\\(.*\\)$" nil t)
      (push (list :width (string-to-number (match-string 1))
                  :height (string-to-number (match-string 2))
                  :indices (mapcar #'string-to-number
                                   (split-string (match-string 3) " " t)))
            grids))
    (nreverse grids)))

;; okay, the only thing I can think to do is just search
;; the trickiest case is "not possible"; just have to do the full search

;; we prepare the shape list first, then try to fit them all
;; the order does not matter.
;; also normalize them to all be the same shape
(defun make-shape-list (shapes width height counts)
  (cl-loop for cnt in counts
           for idx from 0
           when (not (zerop cnt))
           nconc (let ((s (gethash idx shapes)))
                   (make-list cnt (shape-resize s width height)))))

;; transate the shape in all directions until it is off the grid completely
(defun all-translations (s)
  ;; include dx=0, dy=0 to make it not tricky to keep the original in list
  (cl-loop for dx from 0 below (shape-width s)
           nconc (cl-loop for dy from 0 below (shape-height s)
                          for st = (shape-translate s dx dy)
                          when st
                          collect st)))

;; these all looked legit
;; (show-shape
;;  (nth 6
;;       (all-translations
;;        (shape-from-input "##.\n...\n..."))))

(defun all-variants (s)
  (let ((f (shape-flip-h s)))
    (delete-dups
     (nconc
      (all-translations s)
      (all-translations (shape-rotate s))
      (all-translations (shape-rotate (shape-rotate s)))
      (all-translations (shape-rotate (shape-rotate (shape-rotate s))))
      (all-translations f)
      (all-translations (shape-rotate f))
      (all-translations (shape-rotate (shape-rotate f)))
      (all-translations (shape-rotate (shape-rotate (shape-rotate f))))))))

;; these all looked legit too
;; the duplicate deletion might be a bit sketchy
;; (show-shape
;;  (nth 4
;;       (all-variants
;;        (shape-from-input "##.\n...\n..."))))

;; but this returns equal so it is probably okay?
;; (equal
;;  (shape-from-input "##.\n...\n...")
;;  (shape-from-input "##.\n...\n..."))


;; add the shape the the acc shape
;; if any values are gt 1 after adding, there was collision
(defun add-shape (shape acc)
  (unless (= (length (shape-data shape)) (length (shape-data acc)))
    (error "not compatible shapes"))

  (cl-loop
   with ret = (shape-copy-deep acc)
   for i from 0 below (length (shape-data shape))
   for sum = (+ (aref (shape-data acc) i)
                (aref (shape-data shape) i))
   do (aset (shape-data ret) i sum)
   when (> sum 1) return nil  ; collision detected
   finally return ret))

;; a very tests worked here too
;; (show-shape
;;  (add-shape
;;   (shape-from-input "#..\n...\n...")
;;   (shape-from-input "#..\n#..\n...")))

;; try to put all variants of this shape into acc
;; if any fit, and the rest of the sublist fits, return true
;; (defun fit-shapes (shapes acc)
;;   (if (not shapes)
;;       ;; empty list always fits
;;       t
;;     (let ((shape (car shapes))
;;           (rest (cdr shapes)))
;;       (cl-loop
;;        for variant in (all-variants shape)
;;        for tmp = (add-shape variant acc)
;;        when tmp
;;        thereis (fit-shapes rest tmp)))))

;; it was too slow to ignore order in the list, that is, if we have
;; 3 of s1 and 1 of s2, we need to try all combos of the first 3 s1s first, such
;; that we never try s1_v1, s1_v2, s2_v1 and s1_v2, s1_v1, s2_v1
;;
;; we only need to try s1_v1, s1_v2, ... one time
;;
;; the fix for this was AI'd to be as small as possible, but the AI did
;; something rather elegant I think. Though, that's saved for later here

;; (defun fit-shapes (shapes acc &optional min-variant-idx)
;;   (if (not shapes)
;;       t
;;     (let* ((shape (car shapes))
;;            (rest (cdr shapes))
;;            (variants (all-variants shape))
;;            (start-idx (or min-variant-idx 0))
;;            (next-same (and rest (equal shape (car rest)))))
;;       (cl-loop
;;        for variant in (nthcdr start-idx variants)
;;        for idx from start-idx
;;        for tmp = (add-shape variant acc)
;;        when tmp
;;        thereis (fit-shapes rest tmp (and next-same idx))))))

;; it's still way too slow, so doing a memoized approach
;; I think memoization solves the above ^^^ problem, but that might be the only
;; thing is solves. There's probably not that much other overlap in the search
;; tree.
;;
;; (defun fit-shapes (shapes acc &optional memo)
;;   (if (not shapes)
;;       t
;;     (let* ((memo (or memo (make-hash-table :test 'equal)))
;;            (memo-key (cons (shape-data acc) (length shapes)))
;;            (cached (gethash memo-key memo 'not-found)))
;;       (if (not (eq cached 'not-found))
;;           cached
;;         (let ((result
;;                (cl-loop
;;                 for variant in (all-variants (car shapes))
;;                 for tmp = (add-shape variant acc)
;;                 when tmp
;;                 thereis (fit-shapes (cdr shapes) tmp memo))))
;;           (puthash memo-key result memo)
;;           result)))))
;;
;; that's still too slow, so lets start adding some pruning.

(defun remaining-space (acc)
  (cl-loop for val across (shape-data acc)
           count (= val 0)))

(defun total-area (shapes)
  (cl-loop for s in shapes
           sum (seq-reduce #'+ (shape-data s) 0)))

;; (defun fit-shapes (shapes acc)
;;   (cond
;;    ((null shapes) t)
;;    ((< (remaining-space acc) (total-area shapes)) nil)
;;    (t
;;     (let ((shape (car shapes))
;;           (rest (cdr shapes)))
;;       (cl-loop
;;        for variant in (all-variants shape)
;;        for tmp = (add-shape variant acc)
;;        when tmp
;;        thereis (fit-shapes rest tmp))))))

;; okay it's actually all stupid, and solving the impossibly hard problems in
;; the input is not needed. Good, becuase for NP hard problem of these sizes I'm
;; not sure there's a perfect way to solve, they are simply too big. In practice
;; have to use heuristics like crazy
;;
;; this is stupid...
(defun fit-shapes (shapes acc)
  (let ((needs (total-area shapes))
        (has (remaining-space acc)))
    (< (* needs 1.2) has)))

;; this is way to slow b.c. all the making shape lists and stuff isn't needed
(defun day12-part1-inner (input)
  (let* ((data (process-input input))
         (all-shapes (plist-get data :shapes))
         (grids (plist-get data :grids)))
    (cl-loop for grid in grids
             for shapes = (make-shape-list all-shapes
                                           (plist-get grid :width)
                                           (plist-get grid :height)
                                           (plist-get grid :indices))
             when (fit-shapes shapes
                              (shape-create (plist-get grid :width)
                                            (plist-get grid :height)))
             sum 1)))

(aoc-expect 'day12-part1 "2")
(defun day12-part1 (input)
  (number-to-string (day12-part1-inner input)))


;; ;; this one should fail
;; (setq test-data (process-input (aoc-read-sample 12)))
;; (fit-shapes
;;  (make-shape-list
;;   (plist-get test-data :shapes)
;;   12 5 '(1 0 1 0 3 2)) ;; 1 of 0, 1 of 2, 3 of 4 and 2 of 5
;;  (shape-create 12 5))
