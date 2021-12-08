(defconstant *segments*
  (make-array
   10 :initial-contents
   '(#b1110111
     #b0010010
     #b1011101
     #b1011011
     #b0111010
     #b1101011
     #b1101111
     #b1010010
     #b1111111
     #b1111011)))

(defun split-spaces (line)
  (loop for beg = 0 then (1+ end)
        for end = (position #\Space line :start beg)
        for seq = (subseq line beg (or end (length line)))
        if (/= (length seq) 0) collect seq
        while end))

(defun parse-file (file)
  (with-open-file (in file)
    (loop for line = (read-line in nil) while line
          for bar = (position #\| line)
          collect (cons (split-spaces (subseq line 0 bar))
                        (split-spaces (subseq line (1+ bar) (length line)))))))

(defun count-1478-line (patterns output)
  (loop for out in output
        ; All of (1 4 7 8) have unique lengths in a 7-segment display
        count (find (length out) '(2 4 3 7))))

(defun count-1478 (list)
  (loop for (patterns . output) in list
        sum (count-1478-line patterns output)))

(defun lit-segments (digits coding)
  (loop for d across digits sum (ash 1 (- 6 (position d coding)))))

(defun decode-number (digits coding)
  (position (lit-segments digits coding) *segments*))

(defun count-in-bin (elem set)
  (loop for s in set count (find elem s)))

(defun remove-digit (digit freqs)
  (loop for i below 8
        do (setf (aref freqs i)
                 (mapcar (lambda (x) (remove digit x)) (aref freqs i)))))

(defun solve (freqs outputs)
  (let ((coding (make-array 7 :initial-element nil))
        (ret 0))
    ;; Pick A
    ;; A is the value in the length-3 code that is not also in the length-2 code
    (loop for digit across (car (aref freqs 3))
          when (zerop (count-in-bin digit (aref freqs 2)))
            do (setf (aref coding 0) digit))
    (remove-digit (aref coding 0) freqs)
    ;; Pick B
    ;; B is in the length-4 code, it never appears in the length-2 code
    ;; and once in a length-5 code (for value 5)
    (loop for digit across (car (aref freqs 4))
          when (and (= 0 (count-in-bin digit (aref freqs 2)))
                    (= 1 (count-in-bin digit (aref freqs 5))))
            do (setf (aref coding 1) digit))
    (remove-digit (aref coding 1) freqs)
    ;; Pick D
    ;; D is in the length-4 code and is not in length-2 (also it isn't B)
    (loop for digit across (car (aref freqs 4))
          when (zerop (count-in-bin digit (aref freqs 2)))
            do (setf (aref coding 3) digit))
    (remove-digit (aref coding 3) freqs)
    ;; Pick C
    ;; C is in the length-2 code and appears twice in the length-6 code
    (loop for digit across (car (aref freqs 2))
          when (= 2 (count-in-bin digit (aref freqs 6)))
            do (setf (aref coding 2) digit))
    (remove-digit (aref coding 2) freqs)
    ;; Pick F
    ;; F is the other one in the length-2 code
    (setf (aref coding 5) (aref (car (aref freqs 2)) 0))
    (remove-digit (aref coding 5) freqs)
    ;; Pick G
    ;; G appears thrice in the length-6 codes
    (loop for digit across (car (aref freqs 5))
          when (= 3 (count-in-bin digit (aref freqs 5)))
            do (setf (aref coding 6) digit))
    (remove-digit (aref coding 6) freqs)
    ;; Pick E
    ;; E is the segment we haven't chosen yet
    (setf (aref coding 4) (aref (car (aref freqs 7)) 0))
    (remove-digit (aref coding 4) freqs)

    (loop for o in outputs
          do (setf ret (+ (* 10 ret) (decode-number o coding))))

    ret))

(defun sum-lines (lines)
  (loop for (patterns . output) in lines
        for by-len = (make-array 8 :initial-element nil)
        do (loop for pat in patterns
                 do (push pat (aref by-len (length pat))))
        sum (solve by-len output)))

(defun main ()
  (sum-lines (parse-file "./input.txt")))
