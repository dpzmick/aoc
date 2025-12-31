;;; day09.el --- Advent of Code Day 9 -*- lexical-binding: t; -*-


(cl-defstruct point x y)

(defun point-incx (p)
  (make-point :x (1+ (point-x p))
              :y (point-y p)))

(defun point-decx (p)
  (make-point :x (1- (point-x p))
              :y (point-y p)))

(defun point-incy (p)
  (make-point :x (point-x p)
              :y (1+ (point-y p))))

(defun point-decy (p)
  (make-point :x (point-x p)
              :y (1- (point-y p))))

;; feels like there should be a clever way to do this
;; i thought at first I could sort by magnitude but
;; min/max of sqrt(x^2 + y^2) does not imply (x1-x2) and (y1-y2) are maximized
;; simultaneously
;;
;; could find min/max X coord, and min/max Y coord, but minX/minY isn't for sure
;; a coordinate so not sure this helps very much.

(defun process-input (input)
  (cl-loop for line in (split-string input)
           collect (let ((parts (split-string line ",")))
                     (make-point
                      :x (string-to-number (nth 0 parts))
                      :y (string-to-number (nth 1 parts))))))

(defun area-between (a b)
  (* (1+ (abs (- (point-x a) (point-x b))))
     (1+ (abs (- (point-y a) (point-y b))))))

;; just brute force it again

(aoc-expect 'day9-part1 "50")
(defun day9-part1 (input)
  (let* ((pts (vconcat (process-input input)))
         (n (length pts))
         (best 0))
    (cl-loop for i from 0 below n do
             (let ((a (aref pts i)))
               (cl-loop for j from (1+ i) below n do
                        (let* ((b (aref pts j))
                               (area (area-between a b)))
                          (when (> area best)
                            (setq best area))))))
    (number-to-string best)))


;; part2 is "a bit" harder
;; it's a filled sructure, loops around so need to figure out the looping I guess
;;
;; we'll have to build the structure, then check (almost) all pairs of red cells
;; we might be able to skip some red cells
;; and can probably compress the structure a bit
;;
;; to check connected we just need a prefix sum somehow?
;; example
;;
;; ..............
;; .......#XXX#..
;; .......XXXXX..
;; ..ROOOOOOOXX.. -- start scanning here
;; ..OOOOOOOOXX..
;; ..OOOOOOORXX.. -- targetting here
;; .........XXX..
;; .........#X#..
;; ..............
;;
;; We can easily check these points
;; 
;; ..............
;; .......#XXX#..
;; .......XXXXX..
;; ..ROOOOOOOXX.. -- start scanning here
;; ..OOOOOOOOXX..
;; ..********XX.. -- scan down, if all have value Y+(dY), its filled
;; .........XXX..
;; .........#X#..
;; ..............
;;
;; the output is largest triangle area again. So do the compression thing then
;; just look up at the end the resulting coords

;; compression is just taking the x-range of [a, b, c] -> [0, 1, 2], regardless
;; of what x was. Same for y. to get back into (x,y) from (x_c, y_c) we need a
;; lookup table of points, basically
;;
;; I did end up getting a fair amount of AI help on all this indexing
;; correctness

(defun compressed-axis (points getter)
  (sort ;; okay to destruct the brand new sequence, doens't need comp for int
   (seq-uniq ;; doesn't need operator for int
    (seq-map getter points))))

(defun make-index-map (sorted-values)
  (let ((ht (make-hash-table)) ;; integer keys, just x/y coords
        (i 0))
    (dolist (v sorted-values)
      (puthash v i ht)
      (setq i (1+ i))) ;; implictly compressing again here
    ht))

(defun compress-points (points)
  (let* ((xs (compressed-axis points #'point-x))
         (ys (compressed-axis points #'point-y))
         (x-map (make-index-map xs))
         (y-map (make-index-map ys)))
    (mapcar
     (lambda (p)
       (make-point
        :x (gethash (point-x p) x-map)
        :y (gethash (point-y p) y-map)))
     points)))

;; FIXME inline?
(defun add-padding (cpoints)
  (mapcar (lambda (p)
            (make-point :x (1+ (point-x p))
                        :y (1+ (point-y p))))
          cpoints))

(defun remove-padding (p)
  (make-point :x (1- (point-x p))
              :y (1- (point-y p))))

;; uncompress a single point given the original points
;; expensive as it recomputes the internal strutures
(defun uncompress-point (p points)
  (let ((xs (compressed-axis points #'point-x))
        (ys (compressed-axis points #'point-y))
        (xc (point-x p))
        (yc (point-y p)))
    (make-point
     :x (nth xc xs) ;; no point in converting to vector (linear) as we only look once
     :y (nth yc ys))))

;; vector accessed using points

(cl-defstruct vec2d data rows cols)

(defun vec2d-create (rows cols &optional initial-value)
  (let* ((size (* rows cols))
         (data (make-vector size initial-value)))
    (make-vec2d :data data :rows rows :cols cols)))

(defun vec2d-inbounds (v p)
  (and
   (>= (point-x p) 0)
   (>= (point-y p) 0)
   (< (point-x p) (vec2d-cols v))  ; x is column
   (< (point-y p) (vec2d-rows v)))) ; y is row

;; (x, y) where x=column, y=row, origin at top-left
;; y increases downward, x increases rightward
;;
;; for nice display in a grid wiht (0,0) as upper left corner

(defun vec2d-index (v p)
  (when (not (vec2d-inbounds v p))
    (error "out of bounds"))
  ;; row-major: index = row * num_cols + col = y * cols + x
  (+
   (* (point-y p) (vec2d-cols v))
   (point-x p)))

;; actual accessors

(defun vec2d-ref (v p)
  (aref (vec2d-data v) (vec2d-index v p)))

(defun vec2d-set (v p newval)
  (aset (vec2d-data v)
        (vec2d-index v p)
        newval))

(defun show-grid (v)
  (with-current-buffer (get-buffer-create "*grid*")
    (erase-buffer)
    (cl-loop for y from 0 below (vec2d-rows v) do
             (cl-loop for x from 0 below (vec2d-cols v) do
                      (let ((val (vec2d-ref v (make-point :x x :y y))))
                        ;;(insert (if (and val (= val 1)) "#" "."))))
                        (insert (format "%5d" val))))
             (insert "\n"))
    (goto-char (point-min))
    (pop-to-buffer (current-buffer))))

;; for the input data, first drop all the red points into the compressed grid
;; two things going on:
;; 1. 1+ on max coord so the coord value fits in the grid
;; 2. 1+ again to pad along the left and bottom edge with empty space, so we can flood fill
;; 
;; this messes up our point compression but we can undo it later
(defun make-init-grid (cpoints)
  (let* ((max-x (seq-max (seq-map #'point-x cpoints)))
         (max-y (seq-max (seq-map #'point-y cpoints)))
         (v     (vec2d-create (+ 2 max-y) (+ 2 max-x))))
    (cl-loop for cp in cpoints
             do (vec2d-set v cp 1)
             finally return v)))

;; if p1 and p2 can be connected, draw edge between them
;; FIXME should check validity?
(defun draw-edge (v p1 p2)
  (let ((x1 (point-x p1)) (y1 (point-y p1))
        (x2 (point-x p2)) (y2 (point-y p2)))
    (cond
     ((= x1 x2) ; vertical line
      (cl-loop for y from (min y1 y2) to (max y1 y2)
               do (vec2d-set v (make-point :x x1 :y y) 1)))
     ((= y1 y2) ; horizontal line
      (cl-loop for x from (min x1 x2) to (max x1 x2)
               do (vec2d-set v (make-point :x x :y y1) 1))))))

(defun draw-all-edges (v cpoints)
  (let ((pts (vconcat cpoints)))
    (cl-loop for i from 0 below (length pts)
             do (draw-edge v
                           (aref pts i)
                           (aref pts (mod (1+ i) (length pts)))))))

;; FIXME this is broken b.c. it can't get everywhere from that one corner
;; I need to add the padding? think through the wrapping carefully
(defun flood-fill-exterior (v)
  ;; confirmed init point is exterior in both inputs
  (let ((queue (list (make-point :x 0 :y 0))))
    (while queue
      (let ((p (pop queue)))
        (when
            (and
             ;; cell valid
             (vec2d-inbounds v p)
             ;; cell not set to anything; when we find "wall" stop
             (null (vec2d-ref v p)))
          (vec2d-set v p 0) ;; exterior point
          (push (point-incx p) queue)
          (push (point-decx p) queue)
          (push (point-incy p) queue)
          (push (point-decy p) queue))))))

(defun fill-rest (v)
  (cl-loop for x from 0 below (vec2d-cols v)
           do (cl-loop for y from 0 below (vec2d-rows v)
                       do (let ((p (make-point :x x :y y)))
                            (when (null (vec2d-ref v p))
                              (vec2d-set v p 1))))))

;; now that we can compress and fill the grid, we need pick a scan direction and a sum direction
;; then we can scan our points and check if we can fill in the stuff
;;
;; 0 and 1 for the two values is nice, we can just prefix sum down any direction

;; what direction do I want to scan in? board is stored with x dim as fastest
;; moving, so should sweep x at the most expensive step?

;; x is stored as the fastest dimenion, so fill that way
;; this is flipped from the diagrams above but that's okay
(defun prefix-sum (v)
  (cl-loop for y from 0 below (vec2d-rows v)
           do (let ((s 0))
                (cl-loop for x from 0 below (vec2d-cols v)
                         do (let ((p (make-point :x x :y y)))
                              (setq s (+ s (vec2d-ref v p)))
                              (vec2d-set v p s))))))

;; we now have something like this (with red points in ())
;; 
;;    0    0    0    0    0    0 
;;    0    0   (1)   2   (3)   3
;;    0   (1)  (2)   3    4    4
;;    0   (1)   2   (3)   4    4
;;    0    0    0   (1)  (2)   2
;;    0    0    0    0    0    0
;;
;;
;; pick a pair of red points [] and check if they make rectangle
;;
;;    0    0    0    0    0    0 
;;    0    0   (1)   2   (3)   3
;;    0   [1]  (2)   3    4    4
;;    0   (1)   2   (3)   4    4
;;    0    0    0   (1)  [2]   2
;;    0    0    0    0    0    0
;;
;; we build the prefix sums by sweeping x
;; so now we need to sweep y
;; 
;; for y from ay to by
;;   vec[bx] = vec[ax] + (bx-ax)     must have bx be greater one?
;;
;;
;; the "largest" must be determined by the area from original coordinate system
;; so I actually lied when I said it was a one time thing.
;; I guess its probably still okay

;; in compressed point space
;; could maybe have both prefix sums precomputed?
(defun pair-forms-rect (a b v)
  (let* ((ax (point-x a))
         (ay (point-y a))
         (bx (point-x b))
         (by (point-y b))
         (min-x (min ax bx))
         (max-x (max ax bx))
         (dx (- max-x min-x))
         (min-y (min ay by))
         (max-y (max ay by)))
    (cl-loop for y from min-y to max-y
             always (= (- (vec2d-ref v (make-point :x max-x :y y))
                          (vec2d-ref v (make-point :x (1- min-x) :y y)))
                       (1+ dx)))))  ; dx+1 cells should all be 1

(defun all-valid-rects (cpoints v)
  (let ((pts (vconcat cpoints)))
    (cl-loop for i from 0 below (length pts)
             nconc
             (cl-loop for j from (1+ i) below (length pts)
                      when (pair-forms-rect (aref pts i) (aref pts j) v)
                      collect (cons (aref pts i) (aref pts j))))))


(defun my-seq-max (seq mag)
  (cl-reduce
   (lambda (best x)
     (if (> (funcall mag x)
            (funcall mag best))
         x
       best))
   seq))

(defun largest-rect (all-rects points)
  (let* ((rects-uncompressed 
          (seq-map (lambda (pair)
                     (cons
                      (uncompress-point
                       (remove-padding (car pair))
                       points)
                      (uncompress-point
                       (remove-padding (cdr pair))
                       points)))
                   all-rects)))
    (my-seq-max
     rects-uncompressed
     (lambda (pair)
       (area-between (car pair) (cdr pair))))))

(aoc-expect 'day9-part2 "24")
(defun day9-part2 (input)
  (let* ((points (process-input input))
         (cpoints (add-padding (compress-points points)))
         (v (make-init-grid cpoints)))
    (draw-all-edges v cpoints)
    (flood-fill-exterior v)
    (fill-rest v)
    (prefix-sum v)
    ;;(show-grid v)
    (let* ((all (all-valid-rects cpoints v))
           (best (largest-rect all points)))
      (number-to-string
       (area-between (car best) (cdr best))))))

;; was very fast with native compilation, less than a second on the full input
;; 
;; I will admit I did look up solutions to this one! I'd not have come up with
;; the prefix sum; at least not quickly
