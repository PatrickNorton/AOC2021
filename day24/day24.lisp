(define-modify-macro mulf (other) *)

(defun string-prefix-p (prefix value)
  (and (>= (length value) (length prefix))
       (string= (subseq value 0 (length prefix)) prefix)))

(defun end-digit (line)
  (nth 2 (read-from-string (concatenate 'string "(" line ")"))))

;; The input consists of repeating "blocks" of data, each with subtle changes.
;; The only differences per "block" are:
;;  add x ~d
;;  div z ~d
;;  add y ~d
(defun parse-file (file)
  (with-open-file (stream file)
    (loop for line = (read-line stream nil)
          and i from 0 while line
          if (and (string-prefix-p "add x" line)
                  (not (string= "add x z" line)))
            collect (end-digit line) into add-x
          if (string-prefix-p "div z" line)
            collect (end-digit line) into div-z
          if (and (string-prefix-p "add y" line)
                  (= 15 (mod i 18)))
            collect (end-digit line) into add-y
          finally (return (values add-x div-z add-y)))))

(defun run (digit z w add-x div-z add-y)
  (let ((x (+ (nth digit add-x) (mod z 26)))
        (z (floor z (nth digit div-z))))
    (unless (= x w)
      (mulf z 26)
      (incf z (+ w (nth digit add-y))))
    z))

(defun search-val (ch z-so-far z-budget add-x div-z add-y)
  (when (= ch 14)
    (return-from search-val
      (if (zerop z-so-far) '("") ())))
  (when (> z-so-far (nth ch z-budget))
    (return-from search-val ()))
  (let ((x-will-be (+ (nth ch add-x) (mod z-so-far 26)))
        (w-options (loop for i from 1 below 10 collect i))
        (ret ()))
    (when (and (>= x-will-be 1) (< x-will-be 10))
      (setf w-options (list x-will-be)))
    (dolist (w w-options)
      (let* ((z-next (run ch z-so-far w add-x div-z add-y))
             (next (cached-search (1+ ch) z-next z-budget add-x div-z add-y)))
        (dolist (x next)
          (push (concatenate 'string (write-to-string w) x) ret))))
    ret))

(defun memoize (fn)
  (let ((cache (make-hash-table :test #'equal)))
    (lambda (&rest args)
      (multiple-value-bind
            (result exists)
          (gethash args cache)
        (if exists
            result
            (setf (gethash args cache)
                  (apply fn args)))))))

(setf (fdefinition 'cached-search) (memoize #'search-val))

(defun calculate-z-budget (div-z)
  (loop for z in div-z and i from 0
        collect
        (expt 26 (loop for z2 in div-z and x from 0
                       count (and (= z2 26) (>= x i))))))

(defun find-solutions (file)
  (multiple-value-bind (add-x div-z add-y) (parse-file file)
    (let ((z-budget (calculate-z-budget div-z)))
      (mapcar #'parse-integer (cached-search 0 0 z-budget add-x div-z add-y)))))

(defun part-1 ()
  (apply #'max (find-solutions "./input.txt")))

(defun part-2 ()
  (apply #'min (find-solutions "./input.txt")))
