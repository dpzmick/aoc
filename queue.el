;;; queue.el --- Simple queue implementation -*- lexical-binding: t; -*-

(defun queue-create ()
  "Create an empty queue."
  (cons nil nil))

(defun queue-enqueue (queue item)
  "Add ITEM to the end of QUEUE."
  (let ((new-tail (list item)))
    (if (null (car queue))
        ;; Empty queue
        (progn
          (setcar queue new-tail)
          (setcdr queue new-tail))
      ;; Non-empty: append to tail
      (setcdr (cdr queue) new-tail)
      (setcdr queue new-tail))))

(defun queue-dequeue (queue)
  "Remove and return the first item from QUEUE."
  (when (null (car queue))
    (error "Queue is empty"))
  (let ((item (car (car queue))))
    (setcar queue (cdr (car queue)))
    (when (null (car queue))
      (setcdr queue nil))
    item))

(defun queue-empty (queue)
  "Return t if QUEUE is empty."
  (null (car queue)))

(provide 'queue)
