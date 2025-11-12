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
        (when (or (char= #\O (aref map y x))
                  (char= #\[ (aref map y x)))
          (incf total (+ (* y 100) x)))))))
 
(defun solution-1 (file)
  (multiple-value-bind (map movements) (load-data file)
    (loop for direction across movements
          do (progn
               (move-robot map (lookup-direction direction) (robot-location map)))
          finally (return (gps-box-score map)))))

;; part 2
(defun upscaled-map-entry (entry)
  (case entry
    (#\. '(#\. #\.))
    (#\# '(#\# #\#))
    (#\O '(#\[ #\]))
    (#\@ '(#\@ #\.))
    (t   '(#\- #\-))))

(defun upscale-map (map)
  (let* ((dims (array-dimensions map))
         (data (make-array (list (first dims) (* 2 (second dims)))
                           :element-type 'base-char)))
    (loop for row from 0 below (first dims)
          do (loop for column from 0 below (second dims)
                   for replacement = (upscaled-map-entry (aref map row column))
                   do (setf (aref data row (* 2 column)) (first replacement)
                            (aref data row (+ 1 (* 2 column))) (second replacement)))
          finally (return data))))

(defun is-vertical-movement-p (direction)
  (not (zerop (aoc:point-y direction))))


(defun move-robot-2 (overview-map direction location)
  (let* ((next-location (aoc:add-points location direction))
         (next-location-entry (aoc:data-at overview-map next-location)))
    (cond
      ((char= next-location-entry #\.)
       (swap-data overview-map location next-location)
       t)
      ((char= next-location-entry #\#)
       nil)
      ((and (not (is-vertical-movement-p direction))
            (or (char= next-location-entry #\[)
                (char= next-location-entry #\]))
            (move-robot-2 overview-map direction next-location))
       (swap-data overview-map location next-location)
       t)
      ((and (is-vertical-movement-p direction)
            (char= next-location-entry #\[)
            (move-robot-2 overview-map direction next-location)
            (move-robot-2 overview-map direction (aoc:add-points next-location #@(1 0))))
       (swap-data overview-map
                  location
                  next-location)
       (swap-data overview-map
                  (aoc:add-points location #@(1 0))
                  (aoc:add-points next-location #@(1 0)))
       t)
      ((and (is-vertical-movement-p direction)
            (char= next-location-entry #\])
            (move-robot-2 overview-map direction next-location)
            (move-robot-2 overview-map direction (aoc:add-points next-location #@(-1 0))))
       (swap-data overview-map
                  location
                  next-location)
       (swap-data overview-map
                  (aoc:add-points location #@(-1 0))
                  (aoc:add-points next-location #@(-1 0)))
       t))))

(defun solution-2 (file)
  (multiple-value-bind (map movements) (load-data file)
    (setf map (upscale-map map))
    (format t "Initial state~%")
    (print-map map)
    (loop for direction across movements
          do (progn
               (move-robot-2 map (lookup-direction direction) (robot-location map))
               (format t "Move ~c:~%" direction)
               (print-map map))
          finally (return (gps-box-score map)))))
