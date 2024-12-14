(defun load-data (file)
  (flet ((to-nums (str)
           (map 'list #'(lambda (c) (- (char-code c) (char-code #\0))) str)))
    (aoc:process-dataset-to-matrix (aoc:read-data-file file :line-processor #'to-nums) :element-type 'fixnum)))

(defun trail-head-locations (data)
  "Return a collection of positions for each trail head in the data."
  (aoc:positions-for data 0 :test #'=))


(defun trails-from (pt data)
  "Returns a list of the end point of the trails that start from pt"
  (let ((ends ()))
    (labels ((walk (step expected)
               ; (format t "walk ~a ~a~%" step expected)
               (cond
                 ((or (not (aoc:dataset-contains-point-p data step))
                      (not (= (aoc:data-at data step) expected)))
                  ())
                 ((= expected 9 (aoc:data-at data step))
                  (setf ends (adjoin step ends :test #'equalp)))
                 (t (dolist (next-step (aoc:surrounding-points step))
                      (walk next-step (1+ expected)))))))
      (walk pt 0)
      ends)))


(defun solution-1 (file)
  (let ((data (load-data file)))
    (loop for head in (trail-head-locations data)
          sum (length (trails-from head data)))))

(defun count-trails-from (pt data)
  "Returns a list of the end point of the trails that start from pt"
  (let ((count 0))
    (labels ((walk (step expected)
               ; (format t "walk ~a ~a~%" step expected)
               (cond
                 ((or (not (aoc:dataset-contains-point-p data step))
                      (not (= (aoc:data-at data step) expected)))
                  ())
                 ((= expected 9 (aoc:data-at data step))
                  (incf count))
                 (t (dolist (next-step (aoc:surrounding-points step))
                      (walk next-step (1+ expected)))))))
      (walk pt 0)
      count)))

(defun solution-2 (file)
  (let ((data (load-data file)))
    (loop for head in (trail-head-locations data)
          sum (count-trails-from head data))))
