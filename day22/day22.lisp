(ql:quickload "cl-ppcre")

(defstruct (powered-cube (:conc-name pcb-))
  (on t :type boolean)
  (x-range '(0 . 0) :type (cons integer integer))
  (y-range '(0 . 0) :type (cons integer integer))
  (z-range '(0 . 0) :type (cons integer integer)))

(defstruct cube
  (x-range '(0 . 0) :type (cons integer integer))
  (y-range '(0 . 0) :type (cons integer integer))
  (z-range '(0 . 0) :type (cons integer integer)))

(defun create-cube (powered)
  (make-cube
   :x-range (pcb-x-range powered)
   :y-range (pcb-y-range powered)
   :z-range (pcb-z-range powered)))

(defun cube-overlap (a b)
  (let ((new-x (pair-overlap (cube-x-range a) (cube-x-range b)))
        (new-y (pair-overlap (cube-y-range a) (cube-y-range b)))
        (new-z (pair-overlap (cube-z-range a) (cube-z-range b))))
    (and (pair-valid new-x)
         (pair-valid new-y)
         (pair-valid new-z)
         (make-cube
          :x-range new-x
          :y-range new-y
          :z-range new-z))))

(defun pair-overlap (pair-a pair-b)
  (cons (max (car pair-a) (car pair-b))
        (min (cdr pair-a) (cdr pair-b))))

(defun pair-valid (pair)
  (<= (car pair) (cdr pair)))

(defun pair-diff (pair)
  (- (cdr pair) (car pair)))

(defun cube-count (cube number)
  (* (1+ (pair-diff (cube-x-range cube)))
     (1+ (pair-diff (cube-y-range cube)))
     (1+ (pair-diff (cube-z-range cube)))
     number))

(defun pair-contains (a b)
  (and (<= (car a) (car b)) (>= (cdr a) (cdr b))))

(defun cube-in-range (cube clamp)
  (if (null clamp)
      t
      (and (pair-contains clamp (pcb-x-range cube))
           (pair-contains clamp (pcb-y-range cube))
           (pair-contains clamp (pcb-z-range cube)))))

(defun collect-hash (predicate hash)
  (let ((result ()))
    (maphash (lambda (a b) (push (funcall predicate a b) result))
             hash)
    result))

(defun parse-line (line)
  (multiple-value-bind
        (result values)
      (ppcre:scan-to-strings
       "(on|off) x=(-?\\d+)\\.\\.(-?\\d+),y=(-?\\d+)\\.\\.(-?\\d+),z=(-?\\d+)\\.\\.(-?\\d+)"
       line)
    (assert result)
    (make-powered-cube
     :on (string= (aref values 0) "on")
     :x-range (cons (parse-integer (aref values 1)) (parse-integer (aref values 2)))
     :y-range (cons (parse-integer (aref values 3)) (parse-integer (aref values 4)))
     :z-range (cons (parse-integer (aref values 5)) (parse-integer (aref values 6))))))

(defun parse-file (file)
  (with-open-file (stream file)
    (loop for line = (read-line stream nil) while line
          collect (parse-line line))))

(defun on-cubes (input &optional clamp)
  (let ((cubes (make-hash-table :test #'equal)))
    (dolist (cube input)
      (when (cube-in-range cube clamp)
        (let ((overlaps ())
              (cube-data (create-cube cube)))
          (maphash (lambda (cube-b count)
                     (let ((new-cube (cube-overlap cube-data cube-b)))
                       (when new-cube (push (cons new-cube count) overlaps))))
                   cubes)
          (dolist (new-cube overlaps)
            (decf (gethash (car new-cube) cubes 0) (cdr new-cube))))
        (when (pcb-on cube)
          (incf (gethash (create-cube cube) cubes 0)))))
    (apply #'+ (collect-hash #'cube-count cubes))))

(defun part-1 ()
  (on-cubes (parse-file "./input.txt") '(-50 . 50)))

(defun part-2 ()
  (on-cubes (parse-file "./input.txt")))
