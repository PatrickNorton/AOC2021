(defun read-file (file)
  (with-open-file (stream file)
    (loop for line = (read-line stream nil) while line
          collect (parse-line line))))

(defun parse-line (line)
  (let ((*readtable* (copy-readtable)))
    (set-syntax-from-char #\, #\Space)
    (set-macro-character #\[ #'|[-reader|)
    (set-syntax-from-char #\] #\))
    (read-from-string line)))

(defun |[-reader| (stream char)
  (declare (ignore char))
  (read-delimited-list #\] stream))

(defun add-left (x n)
  (cond
    ((null n) x)
    ((integerp x) (+ x n))
    (t (list (add-left (car x) n) (cadr x)))))

(defun add-right (x n)
  (cond
    ((null n) x)
    ((integerp x) (+ x n))
    (t (list (car x) (add-right (cadr x) n)))))

(defun explode (data &optional (n 4))
  (when (numberp data)
    (return-from explode (values nil nil data nil)))
  (when (zerop n)
    (return-from explode (values t (car data) 0 (cadr data))))
  (destructuring-bind (a b) data
    (multiple-value-bind (exp left a right) (explode a (1- n))
      (when exp
        (return-from explode (values t left (list a (add-left b right)) nil)))
      (multiple-value-bind (exp left b right) (explode b (1- n))
        (if exp
            (values t nil (list (add-right a left) b) right)
            (values nil nil data nil))))))

(defun split (data)
  (if (integerp data)
      (if (>= data 10)
          (values t (list (floor data 2) (ceiling data 2)))
          (values nil data))
      (destructuring-bind (a b) data
        (multiple-value-bind (change a) (split a)
          (if change
              (values t (list a b))
              (multiple-value-bind (change b) (split b)
                (values change (list a b))))))))

(defun add (a b)
  (let ((x (list a b)))
    (loop (multiple-value-bind (change _a result _b) (explode x)
            (declare (ignore _a _b))
            (setf x result)
            (unless change
              (multiple-value-bind (change result) (split x)
                (setf x result)
                (unless change (return-from add x))))))))

(defun magnitude (data)
  (if (integerp data)
      data
      (+ (* 3 (magnitude (car data))) (* 2 (magnitude (cadr data))))))

(defun part-1 ()
  (magnitude (reduce #'add (read-file "./input.txt") :initial-value 0)))

(defun part-2 ()
  (loop with data = (read-file "./input.txt")
        for a in data maximize
        (loop for b in data
              unless (equal a b)
                maximize (magnitude (add a b)))))
