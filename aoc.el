;;; aoc.el --- Advent of Code utilities -*- lexical-binding: t; -*-

(defvar aoc-root "~/programming/elisp-aoc/")

(defun aoc-local-input-path (day)
  (format "%sinputs/day%02d.txt" aoc-root day))

(defun aoc-local-sample-path (day)
  (format "%sinputs/day%02d_sample.txt" aoc-root day))

(defun aoc-solution-path (day)
  (format "%s/day%02d.el" aoc-root day))

;; lurked it out of the session in browser
(defconst aoc-cookie "session=53616c7465645f5ff3ea07a35b9ee4921f558250d6c0d19ed262ad555d9a457d3a1dc73f92bf95b6ff607be29d3edf24c9bb12948c45542ca94de29f5e4e06d4")

(defun get-aoc-input-url (day)
  (format "https://adventofcode.com/2025/day/%d/input" day))

(defun fetch-with-headers (url headers)
  "Fetch a URL with alist of HEADERS."
  (let ((url-request-extra-headers headers))
    (with-current-buffer (url-retrieve-synchronously url)
      (goto-char (point-min))
      (re-search-forward "\n\n")
      (prog1 (buffer-substring (point) (point-max))
        (kill-buffer)))))

(defun aoc-fetch-day (day)
  (fetch-with-headers
   (get-aoc-input-url day)
   `(("Cookie" . ,aoc-cookie))))

(defun aoc-fetch-input (day)
  (interactive "nDay: ")
  (let ((path (aoc-local-input-path day)))
    (make-directory (file-name-directory path) t)
    (with-temp-file path
      (insert (aoc-fetch-day day)))
    (message "Fetched input for day %d to %s" day path)))

(defun aoc-work-on (day)
  (interactive "nDay: ")
  (let ((path (aoc-solution-path day)))
    (unless (file-exists-p path)
      (with-temp-file path
        (insert (format ";;; day%02d.el --- Advent of Code Day %d -*- lexical-binding: t; -*-\n\n"
                        day day))))
    (find-file path)))

(defun aoc-edit-sample (day)
  (interactive "nDay: ")
  (let ((path (aoc-local-sample-path day)))
    (find-file path)))

(defun aoc-read-input (day)
  "Read input for DAY as string."
  (with-temp-buffer
    (insert-file-contents (aoc-local-input-path day))
    (buffer-string)))

(defun aoc-read-sample (day)
  "Read input for DAY as string."
  (with-temp-buffer
    (insert-file-contents (aoc-local-sample-path day))
    (buffer-string)))

(defvar aoc-sample-expected '()
  "Alist of (fn . expected-type) for samples.")

(defun aoc-expect (fn expected)
  "Register expected result for FN with INPUT-TYPE."
  (setf (alist-get fn aoc-sample-expected) expected))

(defun aoc-solve (fn &optional input-type)
  (let* ((input-type (or input-type 'real))
         (name (symbol-name fn))
         (day (string-to-number
               (replace-regexp-in-string "day0*\\([0-9]+\\)-part[12]" "\\1" name)))
         (file (aoc-solution-path day))
         (input (if (eq input-type 'sample)
                    (aoc-read-sample day)
                  (aoc-read-input day))))
    (load (native-compile file))
    (let* ((result (funcall fn input))
           (expected (alist-get fn aoc-sample-expected)))
      (kill-new result)
      (pcase input-type
        ('sample
         (cond
          ((null expected)
           (message "%s [sample]: %s (no expected)" fn result))
          ((string= result expected)
           (message "✓ %s [sample]: %s" fn result))
          (t
           (error "✗ %s [sample]: got %s, expected %s" fn result expected))))
        ('real
         (message "%s: %s (copied)" fn result)))
      result)))

(provide 'aoc)
