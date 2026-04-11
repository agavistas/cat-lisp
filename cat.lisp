(defun cat(x y z)
  (let ((sum 0) (ax (reverse x)))
    (if (= y (+ z 1)) (progn (format t "~%~a~%~%~a~%" x (car x)) (exit)))
    (format t "~a~%" (/ y z))
    (loop for a in x and b in ax do
      (incf sum (* a b)))
    (cat (cons sum x) (+ y 1) z)))
