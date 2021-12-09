(defconstant data-width 100)
(defconstant data-height 100)

(defun parse-file (file)
  (with-open-file (in file)
    (loop with data = (make-array `(,data-width ,data-height)
                                  :element-type '(integer 0 10)
                                  :initial-element 0)
          for line = (read-line in nil) while line
          for i from 0
          do (loop for chr across line
                   for j from 0
                   do (setf (aref data i j) (digit-char-p chr)))
          finally (return data))))

(defun neighbor-indices (i j)
  (let ((result nil))
    (when (/= i 0)
      (push (cons (1- i) j) result))
    (when (/= j 0)
      (push (cons i (1- j)) result))
    (when (/= i (1- data-width))
      (push (cons (1+ i) j) result))
    (when (/= j (1- data-height))
      (push (cons i (1+ j)) result))
    result))

(defun get-neighbors (data i j)
  (let ((result nil))
    (when (/= i 0)
      (push (aref data (1- i) j) result))
    (when (/= j 0)
      (push (aref data i (1- j)) result))
    (when (/= i (1- data-width))
      (push (aref data (1+ i) j) result))
    (when (/= j (1- data-height))
      (push (aref data i (1+ j)) result))
    result))

(defun sum-neighbors (data)
  (loop for i from 0 to (1- data-width)
        sum (loop for j from 0 to (1- data-height)
                  for datum = (aref data i j)
                  for neighbors = (get-neighbors data i j)
                  if (every (lambda (x) (< datum x)) neighbors)
                    sum (1+ datum))))

(defun find-basins (data)
  (loop with seen = (make-hash-table :test #'equal)
        for i from 0 to (1- data-width)
        append (loop for j from 0 to (1- data-height)
                     for size = 0
                     for queue = nil
                     if (and (not (gethash (cons i j) seen)) (/= (aref data i j) 9))
                       do (push (cons i j) queue)
                          (loop while queue
                                for (a . b) = (pop queue)
                                unless (gethash (cons a b) seen)
                                  do (setf (gethash (cons a b) seen) t)
                                     (incf size)
                                     (loop for neighbor in (neighbor-indices a b)
                                           unless (= (aref data (car neighbor) (cdr neighbor)) 9)
                                             do (push neighbor queue)))
                       and collect size)))

(defun main ()
  (let ((basins (sort (find-basins (parse-file "../day9/input.txt")) #'>)))
    (* (nth 0 basins) (nth 1 basins) (nth 2 basins))))
