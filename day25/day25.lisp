(defconstant width 139)
(defconstant height 137)

(defun parse-file (file)
  (with-open-file (stream file)
    (loop with east = (make-hash-table :test #'equal)
          and south = (make-hash-table :test #'equal)
          for line = (read-line stream nil)
          and i from 0 while line
          do (loop for char across line and j from 0
                   do (cond
                        ((char= char #\>) (setf (gethash (cons i j) east) t))
                        ((char= char #\v) (setf (gethash (cons i j) south) t))))
          finally (return (cons east south)))))

(defun hash-set-equal (a b)
  (and (= (hash-table-count a) (hash-table-count b))
       (loop for k being the hash-keys of a
             always (nth-value 1 (gethash k b)))))

(defun move-all (current other eastp)
  (let ((new-table (make-hash-table :test #'equal)))
    (loop for (y . x) being the hash-keys of current
          for new-coord = (if eastp (cons y (mod (1+ x) width))
                              (cons (mod (1+ y) height) x))
          unless (or (gethash new-coord current)
                     (gethash new-coord other))
            do (setf (gethash new-coord new-table) t)
          else do (setf (gethash (cons y x) new-table) t))
    new-table))

(defun simulate (east south)
  (loop for i from 1
        for new-east = (move-all east south t)
        for new-south = (move-all south new-east nil)
        when (and (hash-set-equal new-east east)
                  (hash-set-equal new-south south))
          return i
        do (setf east new-east
                 south new-south)))

(defun part-1 ()
  (destructuring-bind (east . south) (parse-file "./input.txt")
    (simulate east south)))
