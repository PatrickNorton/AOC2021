(defconstant p1-start 4)
(defconstant p2-start 2)

(defconstant board-size 10)
(defconstant die-size 100)

(define-modify-macro modf (value) mod)

(defun move (square die score)
  (dotimes (i 3)
    (incf square (1+ die))
    (modf square 10)
    (incf die)
    (modf die 100))
  (values square die (+ score (1+ square))))

(defparameter *winner* (make-hash-table :test #'equal))

(defun count-wins (p1-square p2-square p1-score p2-score)
  (cond
    ((>= p1-score 21)
     '(1 . 0))
    ((>= p2-score 21)
     '(0 . 1))
    ((gethash (list p1-square p2-square p1-score p2-score) *winner*))
    (t (let ((ans (cons 0 0)))
         (dolist (d1 '(1 2 3))
           (dolist (d2 '(1 2 3))
             (dolist (d3 '(1 2 3))
               (let* ((new-p1-square (mod (+ p1-square d1 d2 d3) 10))
                      (new-p1-score (+ p1-score new-p1-square 1)))
                 (destructuring-bind (x1 . y1) (count-wins p2-square new-p1-square p2-score new-p1-score)
                   (setf ans (cons (+ (car ans) y1) (+ (cdr ans) x1))))))))
         (setf (gethash (list p1-square p2-square p1-score p2-score) *winner*) ans)
         ans))))

(defun part-1 ()
  (loop with p1-square = (1- p1-start)
        and p2-square = (1- p2-start)
        and die = 0
        and p1-score = 0
        and p2-score = 0
        and i = 0
        do (setf (values p1-square die p1-score) (move p1-square die p1-score))
           (incf i 3)
        if (>= p1-score 1000)
          do (return (* p2-score i))
        do (setf (values p2-square die p2-score) (move p2-square die p2-score))
           (incf i 3)
        if (>= p2-score 1000)
          do (return (* p1-score i))))

(defun part-2 ()
  (clrhash *winner*)
  (count-wins (1- p1-start) (1- p2-start) 0 0))
