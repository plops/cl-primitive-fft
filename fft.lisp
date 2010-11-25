(defconstant +pi+ (coerce pi 'single-float))

(let* ((nel 32)
       (x (make-array nel :element-type '(complex single-float)))
       (y (make-array nel :element-type '(complex single-float))))
  (declare ((simple-array (complex single-float) 1) x y)
	   (fixnum nel))
  (defun init ()
    (dotimes (i nel)
      (setf (aref x i) (complex 0s0 (sin (/ (* 2s0 +pi+ i 3) nel)))
	    (aref y i) (complex 0s0))))
  
  (defun result ()
    y)
  ;; depth first recursive radix-2 DIT cooley-tukey fft
  ;; out-of-place, in-order-output
  (defun recfft2 (&optional (xo 0) (yo 0) (n nel) (stride 1))
   (declare (fixnum stride xo yo)
	    (values null &optional))
   (format t "~a: " (list 'fft xo yo n stride))
   (dotimes (i n)
     (format t "~a " (aref y (+ yo i))))
   (format t "~%")
   (if (= 1 n)
       (progn 
	 (setf (aref y yo) (aref x xo))
	 nil)
       (let ((n/2 (floor n 2))
	     (2i (* 2 stride)))
	 (recfft2 xo yo n/2 2i)
	 (recfft2 (+ xo stride) (+ yo n/2) n/2 2i)
	 (dotimes (k n/2)
	   (let* ((yok (+ yo k))
		  (h (aref y yok))
		  (w (exp (complex 0s0 (/ (* 2s0 +pi+ k) n))))
		  (kn2 (+ yok n/2))
		  (wy (* w (aref y kn2))))
	     (setf (aref y yok) wy 
		   (aref y kn2) (- h wy))))))))

#+nil
(progn
  (init)
  (recfft2)
  (format t "~a~%" 
	  (list 
	   (result)
	   (map 'list #'abs (result)))))
