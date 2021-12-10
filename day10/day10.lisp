(define-modify-macro multf (&optional (number 1)) *)

(defun load-file (file)
  (with-open-file (in file)
    (loop for line = (read-line in nil) while line
          collect line)))

(defun autocomplete-stack (line)
  (loop with stack = ()
        for char across line
        do (ecase char
             ((#\( #\[ #\{ #\<) (push char stack))
             (#\) (unless (char= (pop stack) #\()
                    (return nil)))
             (#\] (unless (char= (pop stack) #\[)
                    (return nil)))
             (#\} (unless (char= (pop stack) #\{)
                    (return nil)))
             (#\> (unless (char= (pop stack) #\<)
                    (return nil))))
        finally (return stack)))

(defun autocomplete-score (remaining)
  (loop with sum = 0
        for char in remaining
        do (multf sum 5)
           (incf sum
                 (ecase char
                   (#\( 1)
                   (#\[ 2)
                   (#\{ 3)
                   (#\< 4)))
        finally (return sum)))

(defun autocomplete-scores (lines)
  (mapcar #'autocomplete-score
          (remove nil (mapcar #'autocomplete-stack lines))))

(defun error-score (line)
  (loop with stack = ()
        for char across line
        do (ecase char
             ((#\( #\[ #\{ #\<) (push char stack))
             (#\) (unless (char= (pop stack) #\()
                    (return 3)))
             (#\] (unless (char= (pop stack) #\[)
                    (return 57)))
             (#\} (unless (char= (pop stack) #\{)
                    (return 1197)))
             (#\> (unless (char= (pop stack) #\<)
                    (return 25137))))
        finally (return 0)))

(defun sum-errors (lines)
  (apply #'+ (mapcar #'error-score lines)))

(defun main ()
  (let* ((sums (autocomplete-scores (load-file "../day10/input.txt")))
         (sorted-sums (sort sums #'>)))
    (nth (floor (/ (length sorted-sums) 2)) sorted-sums)))
