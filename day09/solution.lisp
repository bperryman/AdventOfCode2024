(defun load-data (file)
  (with-open-file (ifp file :direction :input)
    (read-line ifp)))

(defun decode-disk-map (disk-map)
  "Return a decoded disk map"
  (loop with maplen = (length disk-map)
        for i from 0 below maplen by 2
        collect (list (parse-integer disk-map :start i :end (1+ i))
                      (if (= (1+ i) maplen)
                          0
                          (parse-integer disk-map :start (1+ i) :end (+ i 2))))))

(defun total-disk-blocks (decoded)
  (loop for (used free) in decoded
        sum (+ used free)))

(defun expand-disk-map (disk-map)
  (loop with decoded = (decode-disk-map disk-map)
        with expanded = (make-array (total-disk-blocks decoded) :initial-element nil)
        with i = 0
        for (used free) in decoded
        for id-number from 0
        do
        (loop for j from 1 to used
              do 
              (setf (aref expanded i) id-number)
              (incf i))
        (incf i free)
        finally (return expanded)))

(defun array-swap (array a b)
  "Exchange the elements at indexes a and b of the array"
  (psetf (aref array a) (aref array b)
         (aref array b) (aref array a)))

(defun fragment (disk-map)
  (loop with expanded = (expand-disk-map disk-map)
        for start = (position-if #'null expanded) then (position-if #'null expanded :start start)
        for end = (position-if-not #'null expanded :from-end t) then (position-if-not #'null expanded :from-end t :end end)
        while (< start end)
        do
        (array-swap expanded start end)
        finally (return expanded)))

(defun calculate-disk-map-score (processed-disk-map)
  (loop for i from 0
          for num across processed-disk-map
          until (null num)
          sum (* i num)))

(defun solution-1 (file)
  (let* ((disk-map (load-data file))
         (fragmented (fragment disk-map)))
    (calculate-disk-map-score fragmented)))


(defun solution-2 (file)
  (let ((disk-map (load-data file)))
    ; TOD: summon up enthusiam to do some stuff with it
    )
  )
