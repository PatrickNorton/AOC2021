(defun load-file (file)
  (with-open-file (stream file)
    (let ((start (read-line stream nil))
          (instructions ()))
      (assert (string-empty-p (read-line stream nil)))
      (loop for line = (read-line stream nil) while line
            do (push (cons (subseq line 0 2) (char line (1- (length line)))) instructions))
      (values start instructions))))

(defun string-empty-p (str)
  (zerop (length str)))

(defun pair-table (str)
  (loop with table = (make-hash-table :test #'equal)
        for i from 0 below (1- (length str))
        do (incf (gethash (subseq str i (+ i 2)) table 0))
        finally (return table)))

(defun char-table (str)
  (loop with table = (make-hash-table :test #'equal)
        for chr across str
        do (incf (gethash chr table 0))
        finally (return table)))

(defun rule-table (rules)
  (loop with table = (make-hash-table :test #'equal)
        for (from . to) in rules
        do (setf (gethash from table) to)
        finally (return table)))

(defun copy-hash-table (table)
  (let ((new (make-hash-table :test #'equal)))
    (maphash (lambda (x y) (setf (gethash x new) y)) table)
    new))

(defun make-str (&rest chars)
  (coerce chars 'string))

(defun next-step (pairs chars rules)
  (maphash (lambda (x y)
             (let ((a (char x 0))
                   (b (char x 1))
                   (x-rule (gethash x rules)))
               (decf (gethash x pairs) y)
               (incf (gethash (make-str a x-rule) pairs 0) y)
               (incf (gethash (make-str x-rule b) pairs 0) y)
               (incf (gethash x-rule chars 0) y)))
           (copy-hash-table pairs)))

(defun run (file steps)
  (multiple-value-bind (start instructions) (load-file file)
    (let ((pairs (pair-table start))
          (chars (char-table start))
          (rules (rule-table instructions)))
      (dotimes (i steps)
        (next-step pairs chars rules))
      (loop for val being the hash-values of chars
            maximize val into max
            minimize val into min
            finally (return (- max min))))))

(defun part-1 ()
  (run "./input.txt" 10))

(defun part-2 ()
  (run "./input.txt" 40))
