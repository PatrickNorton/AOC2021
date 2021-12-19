(defmacro while (test &body body)
  `(loop while ,test do ,@body))

(defun load-file (file)
  (with-open-file (stream file)
    (loop for scanner = (parse-scanner stream) while scanner
          collect scanner)))

(defun parse-scanner (stream)
  (and (read-line stream nil)
       (loop for line = (read-line stream nil) until (emptyp line)
             collect (parse-line line))))

(defun parse-line (line)
  (let ((*readtable* (copy-readtable)))
    (set-syntax-from-char #\, #\Space)
    (read-from-string (concatenate 'string "(" line ")"))))

(defun emptyp (sequence)
  (zerop (length sequence)))

(defconstant coord-remaps
  '((0 1 2)
    (0 2 1)
    (1 0 2)
    (1 2 0)
    (2 0 1)
    (2 1 0)))

(defconstant coord-negations
  '((1 1 1)
    (1 1 -1)
    (1 -1 1)
    (1 -1 -1)
    (-1 1 1)
    (-1 1 -1)
    (-1 -1 1)
    (-1 -1 -1)))

(defun apply-remap (remap negation scan)
  (mapcar (lambda (item)
            (mapcar (lambda (negat remap) (* negat (nth remap item)))
                    negation remap))
          scan))

(defun find-alignment (scan-a scan-b)
  (dolist (remap coord-remaps)
    (dolist (negat coord-negations)
      (let ((a scan-a)
            (b (apply-remap remap negat scan-b)))
        (dolist (a-pos a)
          (dolist (b-pos b)
            (let ((remap-by (mapcar #'- b-pos a-pos))
                  (matches 0)
                  (all-remapped (list)))
              (dolist (other-b b)
                (let ((remapped-to-a (mapcar #'- other-b remap-by)))
                  (when (member remapped-to-a scan-a :test #'equal)
                    (incf matches))
                  (push remapped-to-a all-remapped)))
              (when (>= matches 12)
                (return-from find-alignment (values t all-remapped remap-by)))))))))
  (values nil nil nil))

(defun copy-hash-table (table)
  (let ((new-table (make-hash-table :test (hash-table-test table)
                                    :rehash-size (hash-table-rehash-size table)
                                    :rehash-threshold (hash-table-rehash-threshold table)
                                    :size (hash-table-size table))))
    (maphash (lambda (k v) (setf (gethash k new-table) v)) table)
    new-table))

(defun find-locations (data)
  (let ((aligned-indices (make-hash-table :test #'equal))
        (aligned (make-hash-table :test #'equal))
        (all-aligned (copy-list (car data)))
        (noalign (make-hash-table :test #'equal))
        (distances (list)))
    (setf (gethash 0 aligned-indices) t)
    (setf (gethash 0 aligned) (car data))
    (while (< (hash-table-count aligned-indices) (length data))
      (dotimes (i (length data))
        (unless (gethash i aligned-indices)
          (loop for j being the hash-keys of (copy-hash-table aligned-indices)
                unless (gethash (cons i j) noalign)
                  do (multiple-value-bind (ok remap remap-by)
                         (find-alignment (gethash j aligned) (nth i data))
                       (when ok
                         (push remap-by distances)
                         (setf (gethash i aligned-indices) t
                               (gethash i aligned) remap
                               all-aligned (append all-aligned (copy-list remap)))
                         (return))
                     (setf (gethash (cons i j) noalign) t))))))
    (values (remove-duplicates all-aligned :test #'equal)
            distances)))

(defun taxicab-distance (a b)
  (apply #'+ (mapcar #'abs (mapcar #'- a b))))

(defun part-1 ()
  (length (nth-value 0 (find-locations (load-file "./input.txt")))))

(defun part-2 ()
  (let ((dists (nth-value 1 (find-locations (load-file "./input.txt")))))
    (loop for a in dists maximize
          (loop for b in dists maximize
                (taxicab-distance a b)))))
