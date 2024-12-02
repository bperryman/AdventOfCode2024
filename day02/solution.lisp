(defun load-data (file)
  (aoc:read-data-file file :line-processor #'aoc:process-line-as-list))

(defun safe-delta (delta)
  (< 0 (abs delta) 4))

(defun delta-list (data)
  (mapcar #'- data (cdr data)))

(defun safe-readings (data)
  (and (or (apply #'> data)
           (apply #'< data))
       (every #'safe-delta (delta-list data))))

(defun solution-1 (file)
  (let ((data (load-data file)))
    (count-if #'safe-readings data)))

(defun safe-element-p (previous element)
  (or (null previous)
      (and (= (signum previous) (signum element))
           (safe-delta (- previous element)))))


(defun missing-an-element (lst)
  "Return a list of lists that contain the contents of lst without a single element"
  (loop for i from 0 below (length lst)
        collect (append (subseq lst 0 i) (subseq lst (1+ i)))))

(defun safe-dampened-readings (data)
  (or (safe-readings data)
      ; This is a bit brute force. We create every possible list without a single element
      ; and see if that is the results contain a safe reading
      (some #'safe-readings (missing-an-element data))))


(defun solution-2 (file)
  (let ((data (load-data file)))
    (count-if #'safe-dampened-readings data)))
