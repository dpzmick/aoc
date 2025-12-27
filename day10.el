;;; day10.el --- Advent of Code Day 10 -*- lexical-binding: t; -*-

;; can be made with cons cells, but is sort of a pain
(require 'queue)
(require 'cl-lib)

;; using rx to express the regex becuase it looks nifty and I'd like to try
(defconst
 d10-line-regex
 (rx
  line-start
  "["
  (group (+ (any ".#"))) ;; goal switch states, example: [.##.]
  "] "
  (group (*? any)) ;; toggles, just grab them all
  " {"
  (group (+ (any "0-9,")))
  "}"))

(defun extract-line-parts (line)
  (save-match-data ;; unhinged
    (string-match d10-line-regex line)
    (list
     (match-string 1 line)
     (match-string 2 line)
     (match-string 3 line))))

(defun parse-goal (goal-str)
  (cl-loop for c across goal-str
           for i from 0
           ;; sum is really `or` here in this context; no carry
           sum (if (= c ?#) (ash 1 i) 0)))

(defconst d10-toggle-regex
  (rx "(" (group (+ (any "0-9,"))) ")"))

;; toggles are a bit mask
;; 0,3 inverts bits 0 and 3
;; invert is xor
(defun parse-toggle (positions)
  (cl-loop for pos in positions
           ;; sum is really `or` here in this context; no carry
           sum (ash 1 pos)))

(defun parse-toggles (toggles-str)
  (cl-loop with start = 0
           while (string-match d10-toggle-regex toggles-str start)
           do (setq start (match-end 0))
           collect (parse-toggle
                    (seq-map #'string-to-number
                             (string-split (match-string 1 toggles-str) ",")))))

(defun apply-toggle (state toggle)
  (logxor state toggle))

(cl-defstruct d10-line len goal toggles joltage-goal)

(defun parse-line (line)
  (seq-let (goal toggles jolts) (extract-line-parts line)
    (make-d10-line
     :len (length goal)
     :goal (parse-goal goal)
     :toggles (parse-toggles toggles)
     :joltage-goal (vconcat (seq-map #'string-to-number (string-split jolts ","))))))
  

;; a line is like:
;; [.###.#] (0,1,2,3,4) (0,3,4) (0,1,2,4,5) (1,2) {10,11,11,5,10,5}
(defun process-input (input)
  (cl-loop for line in (split-string input "\n" t)
           collect (parse-line line)))

;;(process-input (aoc-read-sample 10))

;; Each combo of lights is a node in a graph
;; each button is a state transition that we can take (from any node)
;; then find shortest path through the graph from the node for "all off"
;;
;; our nodes and edges are well defined:
;; - nodes, all numbers from 0, 2^(len-1)
;; - edges, only exist when there's a toggle that goes from X -> Y
;;      - all toggles apply to all edges
;;
;; I think the graph can be completely implicit?
;;
;; always start from 0 and try to reach goal

(defun solve-line-bfs (line)
  ;; queue of node, distance
  (let ((q (queue-create))
        (visited (make-hash-table))
        (goal (d10-line-goal line))
        (toggles (d10-line-toggles line)))
    (queue-enqueue q (cons 0 0))
    (while (not (queue-empty q))
      (let* ((p (queue-dequeue q))
             (node (car p))
             (dist (cdr p)))

        ;; check if we found it and report how far
        (when (= node goal)
            (throw 'exit dist))

        (when (not (gethash node visited ))
          (puthash node t visited)
          ;; we didn't, keep going
          (cl-loop for toggle in toggles
                   do (queue-enqueue
                       q
                       (cons
                        (apply-toggle node toggle)
                        (1+ dist)))))))))

(defun solve-line (line)
  (catch 'exit (solve-line-bfs line)))

;; (solve-line
;;  (parse-line
;;   "[.###.#] (0,1,2,3,4) (0,3,4) (0,1,2,4,5) (1,2) {10,11,11,5,10,5}"))

(defun debug-log (msg &rest args)
  (with-current-buffer (get-buffer-create "*day10-debug*")
    (goto-char (point-max))
    (insert (apply #'format msg args) "\n")))

(aoc-expect 'day10-part1 "7")
(defun day10-part1 (input)
  (with-current-buffer (get-buffer-create "*day10-debug*")
    (erase-buffer))
  (number-to-string
   (cl-loop for line in (process-input input)
            for line-num from 0
            for result = (solve-line line)
            do (debug-log "P1 Line %3d result=%s"
                          line-num
                          result)
            sum result)))
                 
;; oh, lame, part 2 is just part1 with a different goal and slightly different
;; toggle function?
;;
;; now the states are vector, can't do masks anymore
;; and we increment each state. Just going to copy paste my code tbh

;; (defun add-jolts (toggle jolts-vec)
;;   (vconcat
;;    (cl-loop for i from 0 below (length jolts-vec)
;;             collect (if (zerop (logand toggle (ash 1 i)))
;;                         (aref jolts-vec i)
;;                       (1+ (aref jolts-vec i))))))

;; (defun too-far (curr goal)
;;   (cl-loop for i from 0 below (length goal)
;;            thereis (> (aref curr i) (aref goal i))))

;; ;; this is too large of state space I think
;; ;; not terminating
;; ;; maybe need A*, or solve a different way
;; (defun solve-line-bfs-p2 (line)
;;   ;; queue of node, distance
;;   (let ((q (queue-create))
;;         (visited (make-hash-table :test 'equal))
;;         (goal (d10-line-joltage-goal line))
;;         (toggles (d10-line-toggles line)))
;;     (queue-enqueue
;;      q
;;      (cons (make-vector (length goal) 0) 0))
;;     (while (not (queue-empty q))
;;       (let* ((p (queue-dequeue q))
;;              (node (car p))
;;              (dist (cdr p)))

;;         ;; check if we found it and report how far
;;         (when (equal node goal)
;;             (throw 'exit dist))

;;         (when (not (gethash node visited))
;;           (puthash node t visited)
;;           ;; we didn't, keep going
;;           (cl-loop for toggle in toggles
;;                    for next-node = (add-jolts toggle node)
;;                    unless (too-far next-node goal)
;;                    do (queue-enqueue q (cons next-node (1+ dist)))))))))


;; (defun solve-line-p2 (line)
;;   (catch 'exit (solve-line-bfs-p2 line)))

;; (solve-line-p2
;;  (parse-line
;;   "[.###.#] (0,1,2,3,4) (0,3,4) (0,1,2,4,5) (1,2) {10,11,11,5,10,5}"))

;; (aoc-expect 'day10-part2 "33")
;; (defun day10-part2 (input)
;;   (number-to-string
;;    (cl-loop for line in (process-input input)
;;             sum (solve-line-p2 line))))

;; okay.. so different way


;; this is a linear system
;; We need to find linear combination of "presses" of the input buttons that
;; creates the sum we want
;;
;; that is, for example
;; [.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}
;;
;; each of these toggles is a column
;;
;; (3)  (1,3)   (2)
;;  0     0      0
;;  0     1      0  ....   = A
;;  0     0      1
;;  1     1      0
;;
;; A is not square, it has 4 rows and 6 columns.
;;
;; and we want to find vector x with x_i = "presses" of ith "button"
;;
;; such that
;;
;; Ax = (3 5 4 7).T
;;
;; This system is underdetermined though, so there are multiple solutions.
;; How to solve that?
;;
;; Once we have found a solution we want it to be the "minimum" solution, in
;; that the value of all xs is as small as possible, but I need an example of
;; how this would work with a larger solution for same result..
;;
;; t1 = (0)  t4 = (0,1)   goal = 2,2,2
;; t2 = (1)  t5 = (1,2)
;; t3 = (2)  t6 = (0,2)
;;
;;
;; A = 1 0 0 1 0 1     b = 2
;;     0 1 0 1 1 0         2
;;     0 0 1 0 1 1         2
;;
;;
;; So, I can do
;;  - 2*t1 + 2*t2 + 2*t3 (6 total clicks)
;;  - 2*t4 + 2*t2        (3 total clicks)
;;  - 1*t4 + 1*t5 + 1*t6 (3 total clicks)
;;
;; This almost seems like a "least norm" problem, which has a nice closed form I
;; found, but the least norm solution assumes L2 norm, which has nicer
;; properties. Also we need integers
;;
;; L1 norm is \sum x_i
;; L2 norm is sqrt \sum x_i^2
;;
;; is x guaranteed minimum though?
;; if unique solution, yes, there's only one
;; unique when A is square and invertible, even in the example above it's not
;; square though.
;;
;; if multiple solutions, then have to do another pass to minimize
;; this is now an integer linear program
;;
;; min \sum x_i
;; st Ax = b
;;
;; Around here I'm sort of lost, we can't solve Ax=b really.
;; Especially not minimal solution.
;; 
;; there's a reddit post about reducing part2 to part1 which seems interesting
;; too: https://old.reddit.com/r/adventofcode/comments/1pk87hl/2025_day_10_part_2_bifurcate_your_way_to_victory/
;;
;; After some googling and AIing, a solution here is something like:
;; 
;; I can state that (for the problem above)
;; 
;; x1 + x4 + x6 = b1
;; x2 + x4 + x5 = b2
;; x3 + x4 + x5 = b3
;; 
;; or
;; 
;; x1 = b1 - x4 - x6
;; x2 = b2 - x4 - x5
;; x3 = b3 - x4 - x5
;;
;; Then I can brute force all values of x4,x5,x6 (they are pretty small max
;; val), now that I have a slightly simpler problem. I can search this space
;; smarter perhaps (is that simplex? i canot remember)
;;
;;
;; actually, this I might be able to exrpess part1 as the same system; it's just
;; in GF(2)?  I think? Revisit


;; okay, so lets actuall do that. I'll need to convert the problem into a matrix
;; repr and I'll need a small matrix library. We can reuse my vec2d from the
;; past again but with the dimensions flipped around to be sensible

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

;; row major
(defun mat-index (m x y)
  (when (not (mat-inbounds m x y))
    (error "out of bounds"))
   (+ (* y (mat-rows m)) x))

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
                      (insert (format "%50d" (mat-ref m r c))))
             (insert "\n"))
    (goto-char (point-min))
    (pop-to-buffer (current-buffer))))


;; matrix of Ax=b, do it as A|b
;; I want to get things in the form
;;
;; I can't remember gausian elimination.. so lets rederive that too
;;
;; Suppose
;;
;; a1 x1 + a2 x2 = b1
;; a3 x1 + a4 x2 = b2
;;
;;
;; I think the trick is that I only ever add whole equations to each other
;; So lets get first row into form
;;
;; 1 x1 + ? x2 = ?
;;
;; I cannot do that simply by scaling it (unless a2 is 0)
;;
;; So do it by adding some multiple of the second row
;;
;;     a1 x1 +   a2 x2 =   b1
;; + N a3 x1 + N a4 x2 = N b2  .. Make N a4 = -a2, or N = - a2/a4
;; --------------------------
;; (a1 - a2/a4 + a3) x1 = -a2/a4 b2 + b1
;;
;; Now divide by (a1 - a2/a4 + a3), and we have x1 = some constant
;;
;; 1 x1 = S1
;;
;;      a3 x1 + a4 x2 = b2
;; +  N    x1 + 0     = N S1  .. N = -a3
;; -------------------------
;;       0     a4 x2   = b2 - a3 S1
;;
;; x2 = b2 - a3 S1
;;
;; For my problem I probably need to stay away from floats, is that possible?
;; I don't really need to get into 1 x1 = C, it could be any coefficient I think

; mutate m?
(defun swap-rows (m r1 r2)
  (when (not (= r1 r2))
    (cl-loop for c from 0 below (mat-cols m)
             for tmp = (mat-ref m r1 c)
             do (mat-set m r1 c (mat-ref m r2 c ))
             do (mat-set m r2 c tmp))))

;; different factor for each row
(defun add-rows (m r1 r2 factor1 factor2)
  (cl-loop for c from 0 below (mat-cols m)
           for r1-val = (mat-ref m r1 c)
           for r2-val = (mat-ref m r2 c)
           do (mat-set m r2 c
                       (+ (* r1-val factor1)
                          (* r2-val factor2)))))

;; starting from st-row (where st-row,c is pivot element)
;; elimiate all values below
;;
;; to keep everything in integer, we have to scale both rows to gcm
;; which might blow up all the coefs? bareiss algo maybe helpful but lets just
;; try it
(defun eliminate-down (m pivot-r c)
  (cl-loop for r from (1+ pivot-r) below (mat-rows m)
           for factor1 = (mat-ref m r c) ;; scale by our val
           for factor2 = (* -1 (mat-ref m pivot-r c)) ;; scale by pivot
           do (add-rows m pivot-r r factor1 factor2)))

;; try to eliminate all _other_ values in the column going up.
;; run this once we have the matrix in upper triangular form
(defun eliminate-up (m pivot-r c)
  (cl-loop for r from 0 below pivot-r
           for target-val = (mat-ref m r c)
           for pivot-val = (mat-ref m pivot-r c)
           when (not (zerop target-val))
           ;; r = pivot-val * r - target-val * pivot-row
           do (add-rows m pivot-r r (- target-val) pivot-val)))

(defun nonzero-row-idx-for-col (m c &optional st)
  (cl-loop for r from (or st 0) below (mat-rows m)
           when (not (zerop (mat-ref m r c)))
           do (cl-return r)))

(defun eliminate (m)
  (let ((pivot-row 0)
        (pivot-cols))
    (cl-loop for c from 0 below (mat-cols m)
             for tgt-r = (nonzero-row-idx-for-col m c pivot-row)
             when tgt-r
             do (progn
                  (swap-rows m pivot-row tgt-r)
                  (eliminate-down m pivot-row c)
                  (push (cons pivot-row c) pivot-cols)
                  (setq pivot-row (1+ pivot-row))))

    ;; go back up in pivot order
    (cl-loop for (pr . pc) in pivot-cols
             do (eliminate-up m pr pc))

    ;; return the pivot cols, we need them later
    pivot-cols))

;; given just free vars I can compute the entire solution.
;; where free vars is all the ones that /aren't/ pivot vars
;;
;; in matrix form, I "set" the free vars by subtracting them off of b
;;
;; that is
;;   x1  y1    f1    b1
;;   x2  y2 .. f2 =  b2
;;   x3  y3    f3    b3
;;
;; if we know the values for x_f , with coefs f, then we just
;;
;;  x1 y1 ... = b1 - f1*x_f
;;  x2 y2 ... = b2 - f1*x_f
;;  ...
;;
;; if we start with reduced m, this means each row is just C*x_n = b_n
;; so we just divide b_n / C and we should be in good shape.
;;
;; It's a little more complicated than that of course
;;
;; It'll have to result in integer else the problem doesn't work ;p

(defun rhs-col (m)
  (1- (mat-cols m)))

(defun num-vars (m)
  (1- (mat-cols m)))

(defun pivot-col-set (pivot-cols)
  (mapcar #'cdr pivot-cols))

(defun free-var-cols (m pivot-cols)
  (let ((pcols (pivot-col-set pivot-cols)))
    (cl-loop for c from 0 below (num-vars m)
             unless (member c pcols)
             collect c)))

;; plug in free vars and get the rhs after adjusting accordingly
(defun pivot-row-effective-rhs (m pivot-row free-vals)
  (-
   ;; current rhs
   (mat-ref m pivot-row (rhs-col m))
   ;; sum of adjustments
   (cl-loop for (fc . fv) in free-vals
            sum (* (mat-ref m pivot-row fc) fv))))

(defun solve-pivot-var (m pivot-row pivot-col free-vals)
  (/
   (pivot-row-effective-rhs m pivot-row free-vals)
   (mat-ref m pivot-row pivot-col)))

(defun solve-system (m pivot-cols free-vals)
  (append free-vals
          (cl-loop for (pr . pc) in pivot-cols
                   collect (cons pc (solve-pivot-var m pr pc free-vals)))))

;; (setq test-mat
;;       (mat-from 4 6 [[2 5 3 2 1 2]
;;                      [3 7 4 2 1 2]
;;                      [5 1 1 1 1 2]
;;                      [9 1 2 3 1 2]]))

;; (setq test-pivots
;;       (eliminate test-mat))

;; (show-mat test-mat)

;; (solve-system test-mat test-pivots '((4 . 5)))


(defun parse-toggles-p2 (toggles-str)
  (cl-loop with start = 0
           while (string-match d10-toggle-regex toggles-str start)
           do (setq start (match-end 0))
           collect (seq-map #'string-to-number
                            (string-split (match-string 1 toggles-str) ","))))

(cl-defstruct d10-line-p2 toggles joltage-goal)

(defun parse-line-p2 (line)
  (seq-let (goal toggles jolts) (extract-line-parts line)
    (make-d10-line-p2
     :toggles (parse-toggles-p2 toggles)
     :joltage-goal (vconcat (seq-map #'string-to-number (string-split jolts ","))))))

(defun make-toggles-mat (toggles jolts)
  ;; A|b where:
  ;;   rows = counters (one equation per counter)
  ;;   cols = buttons + 1 (one variable per button, +1 for RHS)
  (let* ((num-counters (length jolts))
         (num-buttons (length toggles))
         (rows num-counters)
         (cols (1+ num-buttons))
         (m (mat-create rows cols 0)))

    ;; For each button, mark which counters it affects
    (cl-loop for button-idx from 0 below num-buttons
             do (cl-loop for counter-idx in (aref toggles button-idx)
                         do (mat-set m counter-idx button-idx 1)))

    ;; RHS column (target joltages)
    (cl-loop for counter-idx from 0 below num-counters
             do (mat-set m counter-idx num-buttons (aref jolts counter-idx)))

    m))

;; this is AI need to think about it I guess
(defun all-combinations (ranges)
  (cl-reduce
   (lambda (r acc)  ; note: swapped!
     (cl-loop for i from 0 to r
              nconc (mapcar (lambda (c) (cons i c)) acc)))
   ranges
   :initial-value '(())
   :from-end t))

;; (all-combinations '(2 2))


;; find max value for each var-idx
;; the target is the max
(defun get-button-max (button-idx toggles jolts)
  "Max presses for button is min joltage of counters it affects.
   If button affects no counters (free variable), use max joltage as upper bound."
  (let ((affected-counters (aref toggles button-idx)))
    (if affected-counters
        (apply #'min (mapcar (lambda (c) (aref jolts c)) affected-counters))
      (apply #'max (cl-coerce jolts 'list)))))

(defun get-maxes (free-cols toggles jolts)
  (mapcar (lambda (fc) (get-button-max fc toggles jolts)) free-cols))

(defun solution-sum (solution)
  (cl-loop for (_ . v) in solution sum v))

(defun zip (a b)
  (cl-loop for i from 0 below (length a)
           collect (cons (nth i a) (nth i b))))


(defun solution-non-negative-p (sol)
  (cl-loop for (_ . v) in sol always (>= v 0)))

(defun integer-solution-p (m pivot-cols free-vals)
  "Check if free-vals yields an integer solution."
  (cl-loop for (pr . pc) in pivot-cols
           for pivot-val = (mat-ref m pr pc)
           for eff-rhs = (pivot-row-effective-rhs m pr free-vals)
           always (zerop (mod eff-rhs pivot-val))))

(defun solve-line-p2 (line)
  (let* ((toggles (vconcat (d10-line-p2-toggles line)))
         (jolts (d10-line-p2-joltage-goal line))
         (m (make-toggles-mat toggles jolts))
         (pivots (eliminate m)) ;; (r . c)
         (fvc (free-var-cols m pivots))  ;; just c
         (fv-maxes (get-maxes fvc toggles jolts))
         (all-fv-values (all-combinations fv-maxes)))
    (message "pivots %s fvc %s" pivots fvc)
    (show-mat m)
    (cl-loop for comb in all-fv-values
             for fv-vals = (zip fvc comb)
             when (integer-solution-p m pivots fv-vals)  ;; ADD THIS, FIXME understand this trick
             for sol = (solve-system m pivots fv-vals) ;; make sure not modifying
             for total = (solution-sum sol)
             when (solution-non-negative-p sol)
             minimize total)))

(solve-line-p2
 (parse-line-p2
  "[.###.#] (0,1,2,3,4) (0,3,4) (0,1,2,4,5) (1,2) {10,11,11,5,10,5}"))

(solve-line-p2
 (parse-line-p2
"[##.....#.] (0,1,5,8) (1,6,7) (3,6,8) (1,3,6,7) (0,1,2,6,7) (1,2,3,5,7) (0,1,3,4,5,6,7) (1,2,4,5,7,8) (0,2,5,7,8) (1,2,3,5,7,8) {53,78,43,44,33,73,46,81,60}"
))

(aoc-expect 'day10-part2 "33")
(defun day10-part2 (input)
  (number-to-string
   (cl-loop for line in (string-split input "\n" t)
            for line-num from 0
            for parsed = (parse-line-p2 line)
            for result = (solve-line-p2 parsed)
            do (debug-log "P2 Line %3d result=%s"
                          line-num
                          result)
            sum result)))
