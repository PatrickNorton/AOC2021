(defun load-file (file)
  (with-open-file (stream file)
    (let ((line (read-line stream)))
      (format nil "~{~a~}"
             (map 'list (lambda (x) (cdr (assoc x binary))) line)))))

(defconstant binary
  '((#\0 . "0000")
    (#\1 . "0001")
    (#\2 . "0010")
    (#\3 . "0011")
    (#\4 . "0100")
    (#\5 . "0101")
    (#\6 . "0110")
    (#\7 . "0111")
    (#\8 . "1000")
    (#\9 . "1001")
    (#\A . "1010")
    (#\B . "1011")
    (#\C . "1100")
    (#\D . "1101")
    (#\E . "1110")
    (#\F . "1111")))

(defun parse (text)
  (let* ((version (parse-integer text :end 3 :radix 2))
         (version-sum version)
         (data (subseq text 3))
         (tid (parse-integer data :end 3 :radix 2)))
    (setf data (subseq data 3))
    (if (= tid 4)
        (with-output-to-string (s)
          (loop
            for cnt = (char data 0)
            do (princ (subseq data 1 5) s)
               (setf data (subseq data 5))
            until (char= cnt #\0)))
        (let ((ltid (char data 0)))
          (setf data (subseq data 1))
          (if (char= ltid #\0)
              (let ((packet-len (parse-integer data :end 15 :radix 2)))
                (setf data (subseq data 15))
                (let ((subpackets (subseq data 0 packet-len)))
                  (loop with x and y
                        do (setf (values x y) (parse subpackets))
                           (setf subpackets x)
                           (incf version-sum y)
                        until (zerop (length subpackets))
                        finally (setf data (subseq data packet-len)))))
              (let ((packet-qty (parse-integer data :end 11 :radix 2)))
                (setf data (subseq data 11))
                (dotimes (i packet-qty)
                  (multiple-value-bind (a b) (parse data)
                    (setf data a)
                    (incf version-sum b)))))))
    (values data version-sum)))

(defun packet-result (packet-id values)
  (ecase tid
    (0 (apply #'+ spv))
    (1 (apply #'* spv))
    (2 (apply #'min spv))
    (3 (apply #'max spv))
    (5 (if (> (car spv) (cadr spv)) 1 0))
    (6 (if (< (car spv) (cadr spv)) 1 0))
    (7 (if (= (car spv) (cadr spv)) 1 0))))

(defun parse-new (text)
   (let* ((version (parse-integer text :end 3 :radix 2))
         (data (subseq text 3))
         (tid (parse-integer data :end 3 :radix 2)))
    (setf data (subseq data 3))
    (if (= tid 4)
        (let ((text (with-output-to-string (s)
                      (loop
                        for cnt = (char data 0)
                        do (princ (subseq data 1 5) s)
                           (setf data (subseq data 5))
                        until (char= cnt #\0)))))
          (values data (parse-integer text :radix 2)))
        (let ((ltid (char data 0))
              (spv (list)))
          (setf data (subseq data 1))
          (if (char= ltid #\0)
              (let ((packet-len (parse-integer data :end 15 :radix 2)))
                (setf data (subseq data 15))
                (let ((subpackets (subseq data 0 packet-len)))
                  (loop with x and y
                        do (setf (values x y) (parse-new subpackets))
                           (setf subpackets x)
                           (push y spv)
                        until (zerop (length subpackets))
                        finally (setf data (subseq data packet-len)))))
              (let ((packet-qty (parse-integer data :end 11 :radix 2)))
                (setf data (subseq data 11))
                (dotimes (i packet-qty)
                  (multiple-value-bind (a b) (parse-new data)
                    (setf data a)
                    (push b spv)))))
          (setf spv (nreverse spv))
          (values data (packet-result tid spv))))))

(defun part-1 ()
  (parse (load-file "./input.txt")))

(defun part-2 ()
  (parse-new (load-file "./input.txt")))
