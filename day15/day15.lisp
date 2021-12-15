(defconstant data-height 100)
(defconstant data-width 100)

(define-modify-macro sortf (predicate) sort)

(defun zeroed-array (size)
  (make-array size :element-type '(integer 0 9) :initial-element 0))

(defun load-file (file)
  (with-open-file (stream file)
    (loop with data = (zeroed-array (list data-height data-width))
          for line = (read-line stream nil) while line
          for i from 0
          do (loop for chr across line and j from 0
                   do (setf (aref data i j) (digit-char-p chr)))
          finally (return data))))

(defun new-data-size (data scale)
  (mapcar (lambda (x) (* x 5)) (array-dimensions data)))

(defun wrap-nine (val)
  (if (> val 9)
      (- val 9)
      val))

(defun copy-data (data new-data i j)
  (destructuring-bind (data-x data-y) (array-dimensions data)
    (dotimes (delx data-x)
      (dotimes (dely data-y)
        (setf (aref new-data (+ delx (* data-x i)) (+ dely (* data-y j)))
              (wrap-nine (+ i j (aref data delx dely))))))))

(defun expand-array (data scale)
  (let ((new-data (zeroed-array (new-data-size data scale))))
    (dotimes (i scale)
      (dotimes (j scale) (copy-data data new-data i j)))
    new-data))

(defun neighbor-indices (i j arr)
  (remove-if-not
   (lambda (x) (array-in-bounds-p arr (car x) (cdr x)))
   (list (cons (1- i) j)
         (cons i (1- j))
         (cons (1+ i) j)
         (cons i (1+ j)))))

(defun leq-optional (x y)
  (and x (<= x y)))

(defun shortest-distance (data)
  (let ((q (list '(0 0 0)))
        (costs (make-hash-table :test #'equal)))
    (loop with (data-x data-y) = (array-dimensions data)
          for (cost x y) = (car q)
          if (and (= x (1- data-x)) (= y (1- data-y)))
            return cost
          do (pop q)
             (loop for (xx . yy) in (neighbor-indices x y data)
                   for new-cost = (+ cost (aref data xx yy))
                   unless (leq-optional (gethash (cons xx yy) costs) new-cost)
                     do (setf (gethash (cons xx yy) costs) new-cost)
                        (push (list new-cost xx yy) q))
             (sortf q (lambda (x y) (< (car x) (car y)))))))

(defun part-1 ()
  (shortest-distance (load-file "./input.txt")))

(defun part-2 ()
  (shortest-distance (expand-array (load-file "./input.txt") 5)))
