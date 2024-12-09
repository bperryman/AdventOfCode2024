(defun load-data (file)
  (aoc:read-data-file file :dataset-processor #'aoc:process-dataset-to-matrix))

(defun pairs (list)
  "Return a list of all the pairs combinations that can be made from the list"
  (apply #'append
         (maplist #'(lambda (lst)
                      (mapcar #'(lambda (item) (list (first lst) item)) (rest lst)))
                  list)))

(defun determine-beacons (data)
  "Return a list of the unique beacon frequencies"
  (flet ((beacon-p (val) (alphanumericp val)))
    (let ((result nil))
      (aoc:do-all-data 
        #'(lambda (val)
            (when (beacon-p val)
              (setf result (adjoin val result :test #'char=))))
        data)
      result)))

(defun resonant-locations (b1 b2)
  "Returns a list of resonant locations for beacons b1 and b2"
  (let ((delta (aoc:subtract-points b2 b1)))
    (list (aoc:subtract-points b1 delta)
          (aoc:add-points b2 delta))))

(defun beacon-resonant-locations (beacon data)
  (let ((locations (aoc:positions-for data beacon :test #'char=)))
    (mapcan #'(lambda (pts) (apply #'resonant-locations pts))
            (pairs locations))))

(defun valid-resonant-locations (data resonances)
  (remove-if-not #'(lambda (pt) (aoc:dataset-contains-point-p data pt)) resonances))

(defun solution-1 (file)
  (let* ((data (load-data file))
         (beacons (determine-beacons data))
         (locations nil))
    (dolist (b beacons (length locations))
      (dolist (loc (valid-resonant-locations data (beacon-resonant-locations b data)))
        (setf locations (adjoin loc locations :test #'equalp))))))


(defun resonant-locations-within-dataset (data b1 b2)
  "Returns a list of all resonant locations for beacons b1 and b2"
  (let ((delta (aoc:subtract-points b2 b1)))
    (labels ((all-locations (start delta)
               (if (aoc:dataset-contains-point-p data start)
                   (cons start 
                         (all-locations (aoc:add-points start delta) delta))
                   ())))
      (append (all-locations b1 (aoc:negate-point delta))
              (all-locations b2 delta)))))

(defun all-beacon-resonant-locations (beacon data)
  (let ((locations (aoc:positions-for data beacon :test #'char=)))
    (mapcan #'(lambda (pts) (apply #'resonant-locations-within-dataset data pts))
            (pairs locations))))


(defun solution-2 (file)
  (let* ((data (load-data file))
         (beacons (determine-beacons data))
         (locations nil))
    (dolist (b beacons (length locations))
      (dolist (loc (valid-resonant-locations data (all-beacon-resonant-locations b data)))
        (setf locations (adjoin loc locations :test #'equalp))))))
