;;; benchmark_elisp.el --- Benchmark harness for elisp solutions -*- lexical-binding: t; -*-

;; Add current directory to load path
(add-to-list 'load-path default-directory)

(require 'aoc)
(require 'json)
(require 'cl-lib)

(defun benchmark-day-part (day part &optional warmup iterations)
  "Benchmark a single day/part combination."
  (let* ((warmup (or warmup 5))
         (iterations (or iterations 20))
         (fn (intern (format "day%d-part%d" day part)))  ; No leading zero!
         (file (aoc-solution-path day))
         (input (aoc-read-input day))
         (times nil)
         (slow-threshold-ms 1000.0)  ; If iteration takes >1s, reduce iterations
         (min-iterations 3))

    ;; Ensure native compiled
    (message "Native compiling day %d..." day)
    (load (native-compile file))

    ;; Warm-up runs - check if first warmup is too slow
    (let ((first-warmup-time nil))
      (dotimes (i warmup)
        (let ((start (float-time)))
          (funcall fn input)
          (when (= i 0)
            (setq first-warmup-time (* 1000.0 (- (float-time) start)))
            (when (> first-warmup-time slow-threshold-ms)
              (setq iterations min-iterations)
              (message "  First warmup took %.2fms, reducing iterations to %d"
                      first-warmup-time iterations))))))

    ;; Timed runs (in milliseconds)
    (dotimes (i iterations)
      (let ((start (float-time)))
        (funcall fn input)
        (let ((elapsed (* 1000.0 (- (float-time) start))))
          (push elapsed times)
          ;; After first timed run, check if we should reduce iterations
          (when (and (= i 0)
                    (> elapsed slow-threshold-ms)
                    (> iterations min-iterations))
            (setq iterations min-iterations)
            (message "  First iteration took %.2fms, reducing remaining iterations to %d"
                    elapsed (1- iterations))))))

    (setq times (nreverse times))
    (let* ((mean (/ (apply #'+ times) (length times)))
           (sorted (sort (copy-sequence times) #'<))
           (median (nth (/ (length times) 2) sorted))
           (variance (/ (apply #'+
                               (mapcar (lambda (x) (expt (- x mean) 2))
                                       times))
                        (length times)))
           (std-dev (sqrt variance)))
      (list :day day
            :part part
            :mean_ms mean
            :median_ms median
            :std_dev_ms std-dev
            :min_ms (car sorted)
            :max_ms (car (last sorted))
            :iterations iterations))))

(defun benchmark-all-days (days &optional warmup iterations)
  "Benchmark specified days."
  (let ((results nil))
    (dolist (day days)
      (message "Benchmarking day %02d..." day)
      (condition-case err
          (dolist (part '(1 2))
            (let ((result (benchmark-day-part day part warmup iterations)))
              (push result results)
              (message "  Part %d: %.2fms (±%.2fms)"
                      part
                      (plist-get result :mean_ms)
                      (plist-get result :std_dev_ms))))
        (error (message "  Error: %s" (error-message-string err)))))
    (nreverse results)))

(defun save-results-json (results filename)
  "Save results to JSON file."
  (with-temp-file filename
    (insert "[\n")
    (let ((first t))
      (dolist (r results)
        (unless first (insert ",\n"))
        (setq first nil)
        (insert (json-encode-plist r))))
    (insert "\n]")))

(defun run-elisp-benchmarks ()
  "Run all benchmarks and save results."
  (interactive)
  (let* ((days '(9 10 11))
         (warmup 5)
         (iterations 20)
         (results (benchmark-all-days days warmup iterations)))
    (save-results-json results "elisp_results.json")
    (message "Benchmarked %d solutions, results saved to elisp_results.json"
             (length results))))

;; When run as batch script
(when noninteractive
  (run-elisp-benchmarks))
