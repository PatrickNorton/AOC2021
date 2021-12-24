;; Problem completed by hand, on paper

(defconstant stacks
  '((a . (d b))
    (b . (d c))
    (c . (b a))
    (d . (a c))))

(defconstant new-stacks
  '((a . (d d d b))
    (b . (d c b c))
    (c . (b b a a))
    (d . (a a c c))))

(defconstant costs
  '((a . 1)
    (b . 10)
    (c . 100)
    (d . 1000)))

(define-modify-macro minf (&rest objs) min)

(defmacro enumerate ((counter obj list) &body body)
  `(loop for ,obj in ,list and ,counter from 0 do ,@body))

(defmacro stack-value (stack val)  ;; Macro for setf
  `(cdr (assoc ,val ,stack)))

(defun done (bot)
  (dolist (pair bot)
    (destructuring-bind (k . vals) pair
      (dolist (x vals)
        (unless (eq x k)
          (return-from done nil)))))
  t)

(defun can-move-from (k col)
  (some (lambda (x) (and x (not (eq x k)))) col))

(defun can-move-to (k col)
  (notany (lambda (x) (and x (not (eq x k)))) col))

(defconstant symbol-indices
  '((a . 2)
    (b . 4)
    (c . 6)
    (d . 8)))

(defun bot-idx (bot)
  (cdr (assoc bot symbol-indices)))

(defun top-idx (col)
  (enumerate (i c col)
    (when c (return-from top-idx i)))
  nil)

(defun dest-idx (col)
  (let ((last nil))
    (enumerate (i c col)
      (unless c (setf last i)))
    last))

(defun open-interval-contains (val bottom top)
  (and (< bottom val) (< val top)))

(defun between (a bot top)
  (or (open-interval-contains a (bot-idx bot) top)
      (open-interval-contains a top (bot-idx bot))))

(defun clear-path (bot top-idx top)
  (loop for val in top and ti from 0
        never (and val (between ti bot top-idx))))

(defun check-state (bot top)
  (let ((counter (make-hash-table))
        (expected (length (cdr (car bot)))))
    (dolist (x top)
      (incf (gethash x counter 0)))
    (dolist (col bot)
      (dolist (x (cdr col))
        (incf (gethash x counter 0))))
    (assert (= (gethash 'a counter) expected))
    (assert (= (gethash 'b counter) expected))
    (assert (= (gethash 'c counter) expected))
    (assert (= (gethash 'd counter) expected))))

(defun min-cost (bot top)
  (check-state bot top)
  (when (done bot)
    (return-from min-cost 0))
  (enumerate (i c top)
    (when (and c (can-move-to c (stack-value bot c)))
      (when (clear-path c i top)
        (let ((di (dest-idx (stack-value bot c))))
          (assert di)
          (let* ((dist (+ di 1 (abs (- (bot-idx c) i))))
                 (cost (* (cdr (assoc c costs)) dist))
                 (new-top (copy-list top))
                 (new-bot (copy-tree bot)))
            (setf (nth i new-top) nil
                  (nth i top) nil
                  (nth di (stack-value new-bot c)) c)
            (return-from min-cost (+ cost (cached-min-cost new-bot new-top))))))))
  (let ((ans most-positive-fixnum))
    (dolist (val bot)
      (tagbody
         (destructuring-bind (k . col) val
           (unless (can-move-from k col)
             (go continue))
           (let ((ki (top-idx col)))
             (unless ki (go continue))
             (let ((c (nth ki col)))
               (enumerate (to ignore top)
                 (tagbody
                    (when (or (member to '(2 4 6 8))
                              (nth to top))
                      (go continue-2))
                    (when (clear-path k to top)
                      (let ((dist (+ ki 1 (abs (- to (bot-idx k)))))
                            (new-top (copy-list top))
                            (new-bot (copy-tree bot)))
                        (assert (not (nth to new-top)))
                        (setf (nth to new-top) c
                              (nth ki (stack-value new-bot k)) nil)
                        (minf ans (+ (* (cdr (assoc c costs)) dist)
                                     (cached-min-cost new-bot new-top)))))
                continue-2)))))
       continue))
    ans))

(defun memoize-min-cost ()
  (let ((cache (make-hash-table :test #'equal)))
    #'(lambda (bot top)
        (let ((saved-bot (copy-tree bot))
              (saved-top (copy-tree top)))
          (multiple-value-bind
                (result exists)
              (gethash (cons bot top) cache)
            (if exists
                result
                (setf (gethash (cons (copy-tree bot) (copy-list top)) cache)
                      (min-cost bot top))))))))

(setf (fdefinition 'cached-min-cost) (memoize-min-cost))

(defun part-1 ()
  (setf (fdefinition 'cached-min-cost) (memoize-min-cost))
  (cached-min-cost (copy-tree stacks) (loop repeat 11 collect nil)))

(defun part-2 ()
  (setf (fdefinition 'cached-min-cost) (memoize-min-cost))
  (cached-min-cost (copy-tree new-stacks) (loop repeat 11 collect nil)))
