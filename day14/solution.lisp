(defvar *data-reader* (copy-readtable))
(set-syntax-from-char #\, #\space *data-reader*)
(set-syntax-from-char #\= #\space *data-reader*)

(defun parse-line-entry (line)
  (let ((line-data (aoc:process-line-as-list line)))
    (list (aoc:create-point (second line-data) (third line-data))
          (aoc:create-point (fifth line-data) (sixth line-data)))))

(defun load-data (file)
  "Load in the data file"
  (let ((*readtable* *data-reader*))
    (aoc:read-data-file file :line-processor #'parse-line-entry)))

;; TODO: Generalize this a fair bit more
(defun wrap-points-within (position board-size)
  (aoc:create-point (mod (aoc:point-x position) (aoc:point-x board-size))
                    (mod (aoc:point-y position) (aoc:point-y board-size))))

;; TODO: Extract this one out too
(defun point-with-rectange (pt top-left bottom-right)
  (let ((pt-x (aoc:point-x pt))
        (pt-y (aoc:point-y pt))
        (tl-x (aoc:point-x top-left))
        (tl-y (aoc:point-y top-left))
        (br-x (aoc:point-x bottom-right))
        (br-y (aoc:point-y bottom-right)))
    (and (<= tl-x pt-x br-x)
         (<= tl-y pt-y br-y))))

(defun run-simulation (position velocity board-size steps)
  (if (zerop steps)
      position
      (run-simulation (wrap-points-within (aoc:add-points position velocity)
                                          board-size)
                      velocity
                      board-size
                      (1- steps))))

(defun quadrants (board-size)
  (let* ((board-x (aoc:point-x board-size))
         (board-y (aoc:point-y board-size))
         (mid-x-column (floor board-x 2))
         (mid-y-row (floor board-y 2)))
    (list (list (aoc:create-point 0 0)
                (aoc:create-point (1- mid-x-column) (1- mid-y-row)))
          (list (aoc:create-point (1+ mid-x-column) 0)
                (aoc:create-point board-x (1- mid-y-row)))
          (list (aoc:create-point 0 (1+ mid-y-row))
                (aoc:create-point (1- mid-x-column) board-y))
          (list (aoc:create-point (1+ mid-x-column) (1+ mid-y-row))
                board-size))))

(defun solution-1 (file board-size)
  (let* ((data (load-data file))
         (quads (quadrants board-size))
         (post-run (loop for (pt v) in data
                         collect (run-simulation pt v board-size 100))))
    (reduce #'*
            (loop for (tl br) in quads
                  collect (count-if #'(lambda (pt)
                                        (point-with-rectange pt tl br))
                                    post-run))
            :initial-value 1)))

;; OK, let's draw
(defun display-board (robots board-size)
  (let* ((sz-x (aoc:point-x board-size))
         (sz-y (aoc:point-y board-size))
         (board (make-array (list sz-y sz-x)
                            :element-type 'character
                            :initial-element #\.)))
    (loop for pt in robots
          do (setf (aref board (aoc:point-y pt) (aoc:point-x pt)) #\*))
    (loop for row from 0 below sz-y
          do
             (loop for col from 0 below sz-x
                   do (format t "~c" (aref board row col)))
             (terpri))))

(defun solution-2 (file board-size iters)
  (let ((data (load-data file)))
    (loop for i from 1 to iters
          do
          (format t "Step ~d~%" i)
          (display-board (loop for (pt v) in data
                               collect (run-simulation pt v board-size i))
                         board-size))))

