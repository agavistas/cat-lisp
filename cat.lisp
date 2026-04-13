(defun cat(x y z)
  (let ((sum 0) (ax (reverse x)))
    (if (= y (+ z 1)) (progn (format t "~%~a~%~%~a~%" x (car x)) (car x))
	(progn
	  (format t "~a~%" (/ y z))
	  (loop for a in x and b in ax do
	    (incf sum (* a b)))
	  (cat (cons sum x) (+ y 1) z)))))

(defun cat-multi (x y z)
  (let ((sum 0) (ax (reverse x)))
    (if (= y (+ z 1)) (progn (format t "~%~a~%~%~a~%" x (car x)) (exit)))
    (format t "~a~%" (/ y z))
    (let ((threads nil))
      (dotimes (i 8)
	(push
	 (sb-thread:make-thread 
	  (lambda (x ax y)
	    (let ((sum 0))
	      (dotimes (i y)
		(incf sum (* (car x) (car ax)))
		(setf x (cdr x))
		(setf ax (cdr ax)))
	      sum))
	  :arguments
	  (list
	  (nthcdr (* i (floor (/ y 8))) x)
	  (nthcdr (* i (floor (/ y 8))) ax)
	  (+ (* (mod y 8) (floor (/ i 7))) (floor (/ y 8))))) threads))
      (sb-thread:thread-yield)
      (dolist (thread threads)
	(incf sum (sb-thread:join-thread thread))))
    (cat_main (cons sum x) (+ y 1) z)))

(defun binom (n k acc)
  (if (= k 0) acc
      (binom (- n 1) (- k 1) (* acc (/ n k)))))

(defun cat-binom (n)
  (/ (binom (* 2 n) n 1) (+ n 1)))

(defun main()
  (format t "~a~%" (cat_binom (parse-integer (second *POSIX-ARGV*)))))

;;in repl, (require :sb-gmp) and (sb-gmp:install-gmp-funs) for faster binary

(save-lisp-and-die "catalanstrophe" :executable t :toplevel #'main)
