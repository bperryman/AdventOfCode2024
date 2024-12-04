; Day 4

(defun load-data (file)
  "Some sort of load mapping here!"
  (aoc:read-data-file file))

(defun reverse-board-direction (board)
  (loop for line in board
        collect (reverse line)))

(defun rotate-board-90 (board)
  "Rotate the board by 90 degrees so that things that would have read
top to bottom now read left to right."
  (let ((line-length (length board)))
    (loop for line-number from 0 below (length (first board))
          collect (make-array line-length
                              :element-type 'character
                              :initial-contents (loop for l in board
                                                      collect (char l line-number))))))

;; Fold this into rotate-board-45?
(defun diagonal (board start-col start-row)
  "Return a diagonal across the board from the start column and start row"
  (let ((dims (array-dimensions board)))
    (loop for c from start-col below (second dims)
          for r from start-row downto 0
          collect (aref board r c) into chars
          finally (return (make-array (length chars)
                                      :element-type 'character
                                      :initial-contents chars)))))

(defun rotate-board-45 (board)
  (let* ((matrix (aoc:process-dataset-to-matrix board))
         (dims (array-dimensions matrix))
         (num-rows (first dims))
         (num-cols (second dims)))
    (append (loop for r from 0 below num-rows
                  collect (diagonal matrix 0 r))
            (loop for c from 1 below num-cols
                  collect (diagonal matrix c (1- num-rows))))))

;; TODO: Candidate for abstraction and libraryfication!
(defun count-xmases (str)
  "Returns a count of the number of times xmas appears in the string"
  (let ((len (length str)))
    (do ((pos (search "XMAS" str) (search "XMAS" str :start2 (min (1+ pos) len)))
         (count 0 (1+ count)))
        ((null pos) count))))

(defun count-xmases-in-board (board)
  (flet ((total-for-board (board)
           (loop for line in board
                 sum (count-xmases line))))
    (+ (total-for-board board)
       (total-for-board (reverse-board-direction board)))))

(defun solution-1 (file)
  (let ((data (load-data file)))
    (+ (count-xmases-in-board data)
       (count-xmases-in-board (rotate-board-90 data))
       (count-xmases-in-board (rotate-board-45 data))
       (count-xmases-in-board (rotate-board-45 (reverse-board-direction data))))))

;; Solution 2 has a different approach
;; * convert the board to a matrix
;; * iterate through the board in looking for an 'a'
;; * grab the diagonals for that point
;; * check that both diagonals contain mas or sam

(defun x-mas-at (data row col)
  "Are there two 'mas' formed in an X at row, col in the data"
  ; Should probably do a check on the row and col indexes!
  (flet ((is-m-s-or-s-m (c1 c2)
           (or (and (char= #\M c1)
                    (char= #\S c2))
               (and (char= #\S c1)
                    (char= #\M c2)))))
    (when (char= #\A (aref data row col))
      (let ((tl (aref data (1- row) (1- col)))
            (br (aref data (1+ row) (1+ col)))
            (tr (aref data (1- row) (1+ col)))
            (bl (aref data (1+ row) (1- col))))
        (and (is-m-s-or-s-m tl br)
             (is-m-s-or-s-m tr bl))))))

(defun solution-2 (file)
  (let* ((data (aoc:process-dataset-to-matrix (load-data file)))
         (dims (array-dimensions data)))
    (loop for row-num from 1 to (- (first dims) 2)
          sum (loop for col-num from 1 to (- (second dims) 2)
                    count (x-mas-at data row-num col-num)))))
