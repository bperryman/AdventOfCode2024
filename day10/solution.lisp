(defun load-data (file)
  (aoc:process-dataset-to-matrix (aoc:read-data-file file) :element-type 'character))

(defvar *trail-path* (list #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))

(defun trail-head-locations (data)
  "Return a collection of positions for each trail head in the data."
  (aoc:positions-for data #\0 :test #'char=))

(defun trails-from (pt data path)
  (declare (optimize (debug 3)))
  (labels ((walk-path (pt remaining-path)
             (cond
               ((null remaining-path) (list pt))
               ((not (aoc:dataset-contains-point-p data pt)) ())
               ((char= (aoc:data-at data pt) (first remaining-path))
                (apply #'append (mapcar #'(lambda (pt)
                                            (walk-path pt (rest remaining-path)))
                                         (aoc:surrounding-points pt))))
               (t nil))))
    (walk-path pt path)))

(defun solution-1 (file)
  (let ((data (load-data file)))
    (loop for head in (trail-head-locations data)
          collect (trails-from head data *trail-path*) into all-trails
          finally (return (remove-duplicates (apply #'append all-trails) :test #'equalp)))))
