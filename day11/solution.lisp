(defun load-data (file)
  "Load in the data file"
  (first (aoc:read-data-file file :line-processor #'aoc:process-line-as-list)))

(defun number-digits (num)
  "Returns the number of digits required to display the printed representation of the number"
  (1+ (floor (log num 10))))

;; solution for day 11
(defun blink (stone blinks)
  "How many stones are generated when the user blinks"
  (cond
    ((zerop blinks) 1)
    ((zerop stone) (blink 1 (1- blinks)))
    ((evenp (number-digits stone))
     (multiple-value-bind (left right) (floor stone (expt 10 (/ (number-digits stone) 2)))
       (+ (blink left (1- blinks))
          (blink right (1- blinks)))))
    (t (blink (* stone 2024) (1- blinks)))))

(defun solution-1 (file)
  (let ((data (load-data file)))
    (loop for stone in data
          sum (blink stone 25))))

(fmemo:memoize 'blink)

(defun solution-2 (file)
  (let ((data (load-data file)))
    (loop for stone in data
          sum (blink stone 75))))