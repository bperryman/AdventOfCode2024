;; Solution for day 12
(defun load-data (file)
  "Load the problem data"
  (aoc:read-data-file file :dataset-processor #'aoc:process-dataset-to-matrix))

#|
(defun visited-p (visited-map pt)
  (= 1 (apply #'aref visited-map pt)))

(defun make-visited-map (data)
  "Creates a 2d array binary bits the same size as the data"
  (let ((size (array-dimensions data)))
    (make-array size
                :element-type '(unsigned-byte 1)
                :initial-element 0)))

(defun next-unvisited (visited-array)
  "Returns the location of the next un-visited location"
  (let ((dims (array-dimensions visited-array)))
    (loop for r from 0 below (first dims)
          do (loop for c from 0 below (second dims)
                   when (zerop (aref visited-array r c))
                   do (return-from next-unvisited (list r c)))
          finally (return nil))))
|#

(defun fencing-around (data visited pt)
  "Uses the data and the visited data to mark off locations visited"
  :todo)

