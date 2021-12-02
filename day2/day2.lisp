(defun load-file (file)
  (with-open-file (stream file)
    (loop for line = (read-line stream nil)
          while line
          for space = (position #\Space line)
          collect (cons (subseq line 0 space)
                        (read-from-string (subseq line (1+ space)))))))

(defun calculate-depth (file)
  (let ((lines (load-file file))
        (position 0)
        (depth 0))
    (loop for (dir . val) in lines
          do (cond
               ((string= dir "forward") (incf position val))
               ((string= dir "down") (incf depth val))
               ((string= dir "up") (decf depth val))))
    (* position depth)))

(defun calculate-depth-aim (file)
  (let ((lines (load-file file))
        (position 0)
        (depth 0)
        (aim 0))
    (loop for (dir . val) in lines
          do (cond
               ((string= dir "forward")
                (incf position val)
                (incf depth (* aim val)))
               ((string= dir "down") (incf aim val))
               ((string= dir "up") (decf aim val))))
    (* position depth)))
