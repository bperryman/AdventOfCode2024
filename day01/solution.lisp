(defun read-data (file)
  (aoc:read-data-file file :line-processor #'aoc:process-line-as-list))

(defun solution-1 (input)
  (let* ((data (read-data input))
         (first-list (sort (mapcar #'first data) #'>))
         (second-list (sort (mapcar #'second data) #'>)))
    (reduce #'+ (mapcar #'(lambda (a b) (abs (- a b))) first-list second-list))))

;; This is actually horribly in-effecient.
;; However on a raspbery pi 5 it still takes only 0.016 seconds to run
(defun solution-2 (input)
  (let* ((data (read-data input))
         (first-list (sort (mapcar #'first data) #'>))
         (second-list (sort (mapcar #'second data) #'>)))
    (reduce #'+ (mapcar #'(lambda (a) (* a (count a second-list))) first-list))))
