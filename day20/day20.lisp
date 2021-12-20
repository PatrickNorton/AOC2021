(defconstant x-width 100)
(defconstant y-width 100)

(defun parse-file (file)
  (with-open-file (stream file)
    (let ((input (read-line stream)))
      (assert (zerop (length (read-line stream))))
      (loop with table = (make-hash-table :test #'equal)
            for line = (read-line stream nil) while line
            for i from 0
            do (loop for chr across line
                     for j from 0
                     do (setf (gethash (cons i j) table) (char= chr #\#)))
            finally (return (values input table))))))

(defun hash-table-count-if (predicate table)
  (let ((count 0))
    (maphash (lambda (x y) (when (funcall predicate x y) (incf count))) table)
    count))

(defun neighbors (i j)
  (loop for x in '(-1 0 1)
        append (loop for y in '(-1 0 1)
                     collect (cons (+ i x) (+ j y)))))

(defun section-number (data x y default)
  (parse-integer
   (map 'string (lambda (pair) (if (gethash pair data default) #\1 #\0))
        (neighbors x y))
   :radix 2))

(defun default-value (default input)
  (char= #\# (char input (if default #b111111111 #b000000000))))

(defun step-1 (input data x-range y-range default)
  (let ((new-data (make-hash-table :test #'equal))
        (x-range (cons (1- (car x-range)) (1+ (cdr x-range))))
        (y-range (cons (1- (car y-range)) (1+ (cdr y-range)))))
    (loop for x from (car x-range) below (cdr x-range)
          do (loop for y from (car y-range) below (cdr y-range)
                   for value = (section-number data x y default)
                   do (setf (gethash (cons x y) new-data) (char= (char input value) #\#))))
    (let ((default (default-value default input)))
      (values new-data x-range y-range default))))

(defun step-n (input data count)
  (let ((x-range (cons 0 x-width))
        (y-range (cons 0 y-width))
        (default nil))
    (dotimes (i count)
      (setf (values data x-range y-range default) (step-1 input data x-range y-range default)))
    (hash-table-count-if (lambda (x y) (declare (ignore x)) y) data)))

(defun part-1 ()
  (multiple-value-bind (input table) (parse-file "./input.txt")
    (step-n input table 2)))

(defun part-2 ()
  (multiple-value-bind (input table) (parse-file "./input.txt")
    (step-n input table 50)))
