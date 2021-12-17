(defconstant target-x '(139 . 187))
(defconstant target-y '(-148 . -89))

(defun in-range (pos range)
  (and (>= pos (car range))
       (<= pos (cdr range))))

(defun enters-target (velocity)
  (loop with current-x = 0 and current-y = 0
        while (and (< current-x (cdr target-x))
                   (> current-y (car target-y)))
        do (incf current-x (car velocity))
           (incf current-y (cdr velocity))
           (unless (zerop (car velocity))
             (decf (car velocity) (if (> (car velocity) 0) 1 -1)))
           (decf (cdr velocity))
        if (and (in-range current-x target-x)
                (in-range current-y target-y))
          return t))

(defun part-1 ()
  (* (car target-y) (/ (1+ (car target-y)) 2)))

(defun part-2 ()
  (loop
    with (min-x . max-x) = target-x
    with (min-y . max-y) = target-y
    for x-vel downfrom max-x to (floor (sqrt min-x))
    sum (loop
          for y-vel from min-y to (abs min-y)
          for vel = (cons x-vel y-vel)
          count (enters-target vel))))
