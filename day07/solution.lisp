(defvar *data-readtable* (copy-readtable))
(set-syntax-from-char #\: #\space *data-readtable*)

(defun load-data (file)
  (let ((*readtable* *data-readtable*))
    (aoc:read-data-file file :line-processor #'aoc:process-line-as-list)))

(defun valid-equation-p (equation operators)
  (let ((target (first equation))
        (initial-value (second equation))
        (remaining-parameters (rest (rest equation))))
    (labels ((solvable (current remaining)
               (if (null remaining)
                 (= target current)
                 (some #'(lambda (op)
                           (solvable (funcall op current (first remaining)) (rest remaining)))
                       operators))))
      (solvable initial-value remaining-parameters))))

(defun solution (file operators)
  (let ((data (load-data file)))
    (loop for equation in data
          when (valid-equation-p equation operators)
          sum (first equation))))

(defun solution-1 (file)
  (solution file (list #'+ #'*)))

(defun solution-2 (file)
  (solution file (list #'+ #'* #'(lambda (a b) (parse-integer (format nil "~d~d" a b))))))
