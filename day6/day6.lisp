(defun read-file (file)
  (with-open-file (in file)
    (loop for c across (read-line in)
          ; Initial ages are all single-digit, so this works
          for digit = (digit-char-p c)
          if digit
            collect digit)))

(defun generation-table (pop)
  (loop with next-gen = (make-hash-table)
        for key being the hash-keys of pop using (hash-value val)
        if (zerop key)
          do (incf (gethash 6 next-gen 0) val)
             (incf (gethash 8 next-gen 0) val)
        else
          do (incf (gethash (1- key) next-gen 0) val)
        finally (return next-gen)))

(defun create-length-table (population)
  (loop for value in population
        with table = (make-hash-table)
        do (incf (gethash value table 0))
        finally (return table)))

(defun main ()
  (let ((start (read-file "./input.txt")))
    (loop repeat 256
          with start = (create-length-table start)
          for gen = (generation-table start) then (generation-table gen)
          finally (return (loop for x being the hash-values of gen
                                sum x)))))
