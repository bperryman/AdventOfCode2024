;; Day 3 solution file
;; NOTE: Today we require the following additional loader:
;; (ql:quickload :cl-ppcre

(defun load-data (file)
  (aoc:read-data-file file))

(defun perform-operation (string)
  (let* ((comma (position #\, string))
         (first-num (parse-integer string :start 4 :end comma))
         (second-num (parse-integer string :start (1+ comma) :junk-allowed t)))
    (* first-num second-num)))

(defun solution-1 (file)
  (let ((data (load-data file))
        (sum 0))
    (dolist (entry data sum)
      (cl-ppcre:do-matches-as-strings (str "mul\\(\\d{1,3},\\d{1,3}\\)" entry)
        (incf sum (perform-operation str))))))

(defun operation-type (str)
  (cond
    ((string-equal "mul" str :end2 3) :operation)
    ((string-equal "do(" str :end2 3) :do)
    ((string-equal "don't(" str :end2 6) :do-not)
    (t :unknown)))

(defun solution-2 (file)
  (let ((data (load-data file))
        (sum 0)
        (process-next t))
    (dolist (entry data sum)
      (cl-ppcre:do-matches-as-strings (str "do\\(\\)|don't\\(\\)|mul\\(\\d{1,3},\\d{1,3}\\)" entry)
        (case (operation-type str)
          (:do (setf process-next t))
          (:do-not (setf process-next nil))
          (:operation (when process-next 
                       (incf sum (perform-operation str)))))))))
