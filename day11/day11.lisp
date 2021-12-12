(defun load-file (file)
  (with-open-file (in file)
    (let ((data (make-array '(10 10) :element-type 'integer
                            :initial-element 0)))
      (loop for line = (read-line in nil) while line
            for i from 0
            do (loop for chr across line
                     for j from 0
                     do (setf (aref data i j) (digit-char-p chr))))
      data)))

(defmacro loop-array (arr &body body)
  `(let* ((dim (array-dimensions ,arr))
          (n (1- (car dim)))
          (m (1- (cadr dim))))
     (do* ((i 0 (if (= j m) (1+ i) i))
           (j 0 (if (= j m) 0 (1+ j))))
          ((= i (1+ n)) ,arr)
       (symbol-macrolet ((el (aref ,arr i j)))
         ,@body))))

(defun indices-where (pred arr)
  (let ((ret))
    (loop-array arr
      (when (funcall pred el) (push (cons i j) ret)))
    ret))

(defun neighbors (data i j)
  (loop for a in '(-1 0 1) append
        (loop for b in '(-1 0 1)
              if (array-in-bounds-p data (+ i a) (+ j b))
                collect (cons (+ i a) (+ j b)))))

(defun bump-energy-levels (octopodes &optional positions)
  (loop-array octopodes
    (when (or (null positions)
              (member (cons i j) positions :test #'equal))
      (incf el))))

(defun reset-octopodes (octopodes)
  (let ((dim (array-dimensions octopodes))
        (count 0))
    (loop-array octopodes
      (when (> el 9)
        (setf el 0)
        (incf count)))
    (values count (= count (apply #'* dim)))))

(defun flash (octopodes &optional exclude)
  (let* ((ready-to-flash
           (set-difference
            (indices-where (lambda (x) (> x 9)) octopodes) exclude :test #'equal))
         (affecteds (mapcar (lambda (x) (neighbors octopodes (car x) (cdr x))) ready-to-flash)))
    (if (null ready-to-flash)
        octopodes
        (progn
          (mapcan (lambda (positions) (bump-energy-levels octopodes positions)) affecteds)
          (flash octopodes (append ready-to-flash exclude))))))

(defun next-step (octopodes &optional (num 1) (num-flashes 0) syncp)
  (if (zerop num)
      (values octopodes num-flashes syncp)
      (multiple-value-bind (count syncp) (reset-octopodes (flash (bump-energy-levels octopodes)))
        (next-step octopodes (1- num) (+ num-flashes count) syncp))))

(defun solve-1 ()
  (next-step (load-file "./input.txt") 100))

(defun solve-2 ()
  (loop with octopodes = (load-file "./input.txt")
        for count from 0
        for syncp = nil then (nth-value 2 (next-step octopodes))
        if syncp return count))
