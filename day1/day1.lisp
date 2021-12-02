(defun load-file (file)
  (with-open-file (stream file)
    (loop for line = (read-line stream nil)
          while line collect (read-from-string line))))

(defun calc-depth (file)
  (let ((lines (load-file file)))
    (loop for (a b) on lines while b
          count (< a b))))

(defun calc-window (file)
  (let ((lines (load-file file)))
    (loop for (a b c) on lines while c
          for sum = (+ a b c)
          and prev-sum = nil then sum
          count (and prev-sum (> sum prev-sum)))))
