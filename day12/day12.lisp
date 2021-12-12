(defun load-file (file)
  (with-open-file (in file)
    (loop for line = (read-line in nil) while line
          do (format t "~s~%" line)
          collect (split-hyphens line))))

(defun split-hyphens (line)
  (loop for beg = 0 then (1+ end)
        for end = (position #\- line :start beg)
        for seq = (subseq line beg (or end (length line)))
        if (/= (length seq) 0) collect seq
        while end))

(defun make-hash (input)
  (let ((map (make-hash-table :test #'equal)))
    (dolist (path input)
      (push (cadr path) (gethash (car path) map nil))
      (push (car path) (gethash (cadr path) map nil)))
    map))

(defun single-path-count (data visited to twice)
  (let ((already (and (string= (string-downcase to) to)
                      (find to visited :test #'string=))))
    (cond
      ((equal to "end") 1)
      ((equal to "start") 0)
      ((and already twice) 0)
      (t (push to visited)
         (loop for next in (gethash to data)
               sum (single-path-count data visited next (or twice already))
               finally (pop visited))))))

(defun path-count (data twice)
  (loop with table = (make-hash data)
        with visited = (list)
        for path in (gethash "start" table)
        sum (single-path-count table visited path twice)))

(defun main ()
  (path-count (load-file "./input.txt") nil))
