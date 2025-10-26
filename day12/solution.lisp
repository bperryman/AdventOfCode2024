;; Solution for day 12
(defun load-data (file)
  "Load the problem data"
  (aoc:read-data-file file :dataset-processor #'aoc:process-dataset-to-matrix))

(defun visited-p (visited-map pt)
  (= 1 (apply #'aref visited-map pt)))

(defun create-visited-map (data)
  "Creates a 2d array binary bits the same size as the data"
  (let ((size (array-dimensions data)))
    (make-array size
                :element-type '(unsigned-byte 1)
                :initial-element 0)))

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

(defun fencing-around (data r c current-cell)
  "Uses the data and the visited data to mark off locations visited"
  (let ((dimensions (array-dimensions data)))
    (cond
      ((or (< r 0) (< c 0) (>= r (first dimensions)) (>= c (second dimensions))) 1)
      ((null (aref data r c)) 0)
      ((not (char= current-cell (aref data r c))) 1)
      (t 
       (setf (aref data r c) nil)
       (+ (fencing-around data (- r 1) c current-cell)
          (fencing-around data r (- c 1) current-cell)
          (fencing-around data r (+ c 1) current-cell)
          (fencing-around data (+ r 1) c current-cell))))))

