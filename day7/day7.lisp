(defun parse-file (file)
  (with-open-file (in file)
    (loop with string = (read-line in)
          for beg = 0 then (1+ end)
          for end = (position #\, string :start beg)
          collect (read-from-string (subseq string beg (or end (length string))))
          while end)))

(defun median (list)
  (nth (/ (length list) 2) (sort (copy-list list) #'<)))

(defun mean (list)
  (/ (reduce #'+ list) (length list)))

(defun min-fuel (crabs)
  (loop with position = (median crabs)
        for crab in crabs
        sum (abs (- crab position))))

(defun triangular-fuel (crabs pos)
  (loop for crab in crabs
        for dist = (abs (- crab pos))
        sum (/ (* dist (1+ dist)) 2)))

(defun min-triangular-fuel (crabs)
  (let ((mean (mean crabs)))
    (if (integerp mean)
        (triangular-fuel crabs mean)
        (min (triangular-fuel crabs (ceiling mean))
             (triangular-fuel crabs (floor mean))))))

(defun main ()
  (min-triangular-fuel (parse-file "./input.txt")))
