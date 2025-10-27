;; Solution for day 12
(defun load-data (file)
  "Load the problem data"
  (aoc:read-data-file file :dataset-processor #'aoc:process-dataset-to-matrix))

(defun visited-p (visited-map r c)
  (= 1 (aref visited-map r c)))

(defun set-visited (visited-map r c)
  (setf (aref visited-map r c) 1))

(defun create-visited-map (data)
  "Creates a 2d array binary bits the same size as the data"
  (let ((size (array-dimensions data)))
    (make-array size
                :element-type '(unsigned-byte 1)
                :initial-element 0)))

(defun number-of-locations-visited (data)
  (reduce #'+ (make-array (apply #'* (array-dimensions data))
                          :displaced-to data
                          :element-type '(unsigned-byte 1))))

(defun merge-visited (visited-1 visited-2)
  "Create a unified visited map."
  (let ((return-data (create-visited-map visited-1))
        (size (array-dimensions visited-1)))
    (loop for r from 0 below (first size)
          do (loop for c from 0 below (second size)
                   do (setf (aref return-data r c)
                            (max (aref visited-1 r c)
                                 (aref visited-2 r c))))
          finally (return return-data))))

(defun next-unvisited (visited-array)
  "Returns the location of the next un-visited location"
  (let ((dims (array-dimensions visited-array)))
    (loop for r from 0 below (first dims)
          do (loop for c from 0 below (second dims)
                   when (zerop (aref visited-array r c))
                   do (return-from next-unvisited (list r c)))
          finally (return nil))))

(defun fencing-around (data r c)
  "Uses the data and the visited data to mark off locations visited"
  (let ((dimensions (array-dimensions data))
        (visited-map (create-visited-map data))
        (current-cell (aref data r c)))
    (labels ((visit-similar (r c)
               (cond
                 ((or (< r 0) (< c 0) (>= r (first dimensions)) (>= c (second dimensions))) 1)
                 ((visited-p visited-map r c) 0)
                 ((not (char= current-cell (aref data r c))) 1)
                 (t 
                  (set-visited visited-map r c)
                  (+ (visit-similar (- r 1) c)
                     (visit-similar r (- c 1))
                     (visit-similar r (+ c 1))
                     (visit-similar (+ r 1) c))))))
      (values (visit-similar r c) visited-map))))

(defun solution-1 (file-name)
  (let* ((data (load-data file-name))
         (visited (create-visited-map data))
         (result 0))
    (do ((next-start '(0 0) (next-unvisited visited)))
        ((null next-start) result)
      (multiple-value-bind (fencing area-map) (fencing-around data
                                                              (first next-start)
                                                              (second next-start))
        (let ((area (number-of-locations-visited area-map)))
          (format t "Location ~a containing ~c with perimeter ~d and area ~a~%"
                  next-start (apply #'aref data next-start) fencing area)
          (incf result (* fencing area))
          (setf visited (merge-visited visited area-map)))))))
