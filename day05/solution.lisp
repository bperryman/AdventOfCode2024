(defvar *data-readtable* (copy-readtable))
(set-syntax-from-char #\| #\space *data-readtable*)
(set-syntax-from-char #\, #\space *data-readtable*)

(defun load-data (file)
  (let ((*readtable* *data-readtable*))
    (aoc:read-data-file file :line-processor #'aoc:process-line-as-list)))

(defun get-print-rules (data)
  (let* ((end-of-rules (position nil data))
         (rules (subseq data 0 end-of-rules))
         (processed-rules (make-hash-table)))
    (loop for (before after) in rules
          do (push after (gethash before processed-rules))
          finally (return processed-rules))))

(defun get-print-runs (data)
  (let ((end-of-rules (position nil data)))
    (subseq data (1+ end-of-rules))))

(defun middle-page-number (data)
  (elt data (1- (ceiling (length data) 2))))

(defun valid-print-run-p (rules print-run)
  (let ((printed ()))
    (dolist (page print-run t)
      (let ((after-list (gethash page rules)))
        (if (not (null (intersection printed after-list :test #'=)))
            (return nil)
            (push page printed))))))

(defun solution-1 (file)
  (let* ((data (load-data file))
         (rules (get-print-rules data))
         (runs (get-print-runs data)))
    (loop for run in runs
          when (valid-print-run-p rules run)
            sum (middle-page-number run))))

;; Some of this could probably be simplified but that's for later
(defun insert-before (lst number before)
  "Insert number into lst before before in the list, or at the end if
not already there."
  (cond
    ((null lst)
     (list number))
    ((and (numberp before) (= before (car lst)))
     (cons number lst))
    (t
     (cons (car lst)
           (insert-before (cdr lst) number before)))))

(defun insert-before-what (current-list constraints)
  (let ((overlap (intersection current-list constraints :test #'=)))
    (when overlap
      (first (sort overlap #'< 
                   :key #'(lambda (x) (position x current-list)))))))


(defun correct-print-order (rules run)
  "Return a corrected print order for the run"
  (let ((result ()))
    (dolist (page run result)
      (setf result
            (insert-before result
                           page
                           (insert-before-what result (gethash page rules)))))))

(defun solution-2 (file)
  (let* ((data (load-data file))
         (rules (get-print-rules data))
         (runs (get-print-runs data)))
    (loop for run in runs
          unless (valid-print-run-p rules run)
            sum (middle-page-number (correct-print-order rules run)))))


