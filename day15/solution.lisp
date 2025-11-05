(defun load-data (file)
  "Load the map and movement test data, return mulitple values of the map
and the test data"
  (let* ((data (aoc:read-data-file file))
         (grid-end (position "" data :test #'string=)))
    (values (make-array (list grid-end (length (first data)))
                        :initial-contents (subseq data 0 grid-end))
            (apply #'concatenate 'string (subseq data (1+ grid-end))))))
 
(defun robot-location (overview-map)
  "Find the robot in the overview map and return the location as a column
major list (row column)"
  (let ((dims (array-dimensions overview-map)))
    (loop for row from 0 below (first dims)
          do (loop for column from 0 below (second dims)
                   when (char= #\@ (aref overview-map row column))
                     do (return-from robot-location (aoc:create-point column row))))))

(defparameter *direction-map* `((#\^ . ,#@(0 -1))
                                (#\v . ,#@(0 1))
                                (#\< . ,#@(-1 0))
                                (#\> . ,#@(1 0))))

(defun lookup-direction (direction-character)
  (or (cdr (assoc direction-character *direction-map* :test #'char=))
      #@(0 0)))

(defun swap-data (data pt1 pt2)
  "Swap the contents of points pt1 and pt2 in the data"
  (psetf (aoc:data-at data pt1) (aoc:data-at data pt2)
         (aoc:data-at data pt2) (aoc:data-at data pt1)))

(defun move-robot (overview-map direction location)
  (let ((next-location (aoc:add-points location direction)))
    (case (aoc:data-at overview-map next-location)
      (#\. (swap-data overview-map location next-location) t)
      (#\# nil)
      (#\O (when (move-robot overview-map direction next-location)
             (swap-data overview-map location next-location)
             t)))))

(defun print-map (map)
  (let ((dims (array-dimensions map)))
    (loop for row from 0 below (first dims)
          do (progn
               (loop for column from 0 below (second dims)
                     do (format t "~c" (aref map row column)))
               (terpri)))))

(defun gps-box-score (map)
  (let ((dims (array-dimensions map)))
    (do ((total 0)
         (x 0 (1+ x)))
        ((= x (second dims)) total)
      (do ((y 0 (1+ y)))
          ((= y (first dims)))
        (when (char= #\O (aref map y x))
          (incf total (+ (* y 100) x)))))))
 
(defun solution-1 (file)
  (multiple-value-bind (map movements) (load-data file)
    (loop for direction across movements
          do (progn
               (move-robot map (lookup-direction direction) (robot-location map)))
          finally (return (gps-box-score map)))))
