(defvar vlc (definst vlc vlc normal-cello-tuning *fund* '(5 7 11 16)))
(defvar hs-range '(80 10000))
(defvar hs-9897-filt (mapcan #'(lambda (h) (when (and (<= h (cadr hs-range)) (>= h (car hs-range))) (list h))) (hailstone 9897)))

(defun plot-pitches ()
  (let ((hs (ftom hs-9897-filt))
	(fl (arithm-seq 108 :min 60))
	(vlc (inst-nat-harm-midi vlc)))
    (let ((all-pitches (sort (flatten (list hs fl vlc)) '<)))
      (let ((minp (reduce #'min all-pitches))
	    (maxp (reduce #'max all-pitches)))
	(print vlc)
	(gp-command "unset key")
	(gp-command "set ytics 1")
	(lplot
	 (flatten1
	  (list
	   ;; all pitches
	   ;;(list (mapcar #'round all-pitches) all-pitches)
	   (list (mapcar #'round hs) hs "with points pt 1")
	   (list (mapcar #'round fl) fl "with points pt 2")
	   (list (mapcar #'round (flatten vlc)) (flatten vlc) "with points pt 3")
	   ;; vlc
	   (flatten1 (mapcar #'(lambda (p) (list (list (reduce #'min all-pitches) (round p)) (list p p) "with lines lt rgb \"red\"")) (car vlc)))
	   (flatten1 (mapcar #'(lambda (p) (list (list (reduce #'min all-pitches) (round p)) (list p p) "with lines lt rgb \"green\"")) (cadr vlc)))
	   (flatten1 (mapcar #'(lambda (p) (list (list (reduce #'min all-pitches) (round p)) (list p p) "with lines lt rgb \"blue\"")) (caddr vlc)))
	   (flatten1 (mapcar #'(lambda (p) (list (list (reduce #'min all-pitches) (round p)) (list p p) "with lines lt rgb \"black\"")) (cadddr vlc)))
	   ;; flute
	   (flatten1 (mapcar #'(lambda (p) (list (list (reduce #'max all-pitches) (round p)) (list p p) "with lines")) fl)))))))))

(defun plot-fl (xy)
  (let ((x (car (deinterleave 2 xy)))
	(y (cadr (deinterleave 2 xy))))
    ;(gp-command "unset xtics")
    (gp-command "unset ytics")
    (gp-command "unset grid")
    ;(gp-command "set xtics 10")
    (gp-command "set ytics 0.5")
    (gp-command "set grid ytics")
    (plot x y "with linespoints")))

(defun plot-vlc (xy)
  (let ((x (car (deinterleave 2 xy)))
	(y (cadr (deinterleave 2 xy)))
	(vlc (inst-nat-harm-midi vlc)))
    (let ((xmin (reduce #'min x))
	  (xmax (reduce #'max x))
	  (ymin (reduce #'min y))
	  (ymax (reduce #'max y)))
      ;(gp-command "unset xtics")
      (gp-command "unset ytics")
      (gp-command "set xtics")
      (gp-command "set ytics 1")
      (gp-command "unset grid")
      (gp-command "unset key")
      (lplot
       (flatten1
	(list
	 (list x y "with linespoints")
	 (flatten1 (flatten1 (mapcar #'(lambda (xxx) (list
	 (flatten1 (mapcar #'(lambda (p) (list (list xxx (+ xxx 10)) (list p p) "with lines lt rgb \"red\"")) (car vlc)))
	 (flatten1 (mapcar #'(lambda (p) (list (list xxx (+ xxx 10)) (list p p) "with lines lt rgb \"green\"")) (cadr vlc)))
	 (flatten1 (mapcar #'(lambda (p) (list (list xxx (+ xxx 10)) (list p p) "with lines lt rgb \"blue\"")) (caddr vlc)))
	 (flatten1 (mapcar #'(lambda (p) (list (list xxx (+ xxx 10)) (list p p) "with lines lt rgb \"black\"")) (cadddr vlc))))) (arithm-seq 410 :step 10))))
	 ))))))

(defun plot-fl-with-vlc-partials (flxy vlcxy)
  (let ((vlc (inst-nat-harm-midi vlc)))
  (lplot
   (flatten1
    (list
     (list (arithm-seq 420.0 :step 0.01) (mapcar (2dpcf (car (deinterleave 2 flxy)) (cadr (deinterleave 2 flxy))) (arithm-seq 420.0 :step 0.01)))
     (list (arithm-seq 420.0 :step 0.01) (mapcar (2dpcf (car (deinterleave 2 vlcxy)) (cadr (deinterleave 2 vlcxy))) (arithm-seq 420.0 :step 0.01)))
     (flatten1 (flatten1 (mapcar #'(lambda (xxx) (list
						  (flatten1 (mapcar #'(lambda (p) (list (list xxx (+ xxx 10)) (list p p) "with lines lt rgb \"red\"")) (car vlc)))
						  (flatten1 (mapcar #'(lambda (p) (list (list xxx (+ xxx 10)) (list p p) "with lines lt rgb \"green\"")) (cadr vlc)))
						  (flatten1 (mapcar #'(lambda (p) (list (list xxx (+ xxx 10)) (list p p) "with lines lt rgb \"blue\"")) (caddr vlc)))
						  (flatten1 (mapcar #'(lambda (p) (list (list xxx (+ xxx 10)) (list p p) "with lines lt rgb \"black\"")) (cadddr vlc))))) (arithm-seq 410 :step 10)))))))))
	  
  

;(defparameter pwf (2dplf '(0 180 180.1 360 420) '(0.5 2.0 0.5 0.5 0.2)))
(defparameter pwf (2dplf '(0 180 180.1 360 420) '(0.25 0.5 0.125 0.125 0.1)))

(defparameter fl-test (read-list-from-file "fl-timeseries.txt"))
(defparameter vlc-test (read-list-from-file "vlc-timeseries.txt"))

(defun plot-histo (x y)
  (labels ((rec (y alist)
	     (if (null y)
		 (let ((y (mapcar #'(lambda (x) (let* ((v (assoc x alist)) (count (if (null v) 0 (cdr v)))) count)) x)))
		     (gp-command "set boxwidth 0.95 relative")
		     (plot x y "with boxes")
		     (mapcar #'(lambda (x y) (format t "~a: ~a~%" x y)) x y)
		     (list x y))
		 (let* ((v (assoc (car y) alist))
			(count (if (null v) 0 (cdr v))))
		   (rec (cdr y) (acons (car y) (1+ count) alist))))))
    (rec y '())))

(defun write-approx-list (target-list target-basename inst-list subdiv-list n bw &optional (print-nvisits 0))
  "same as write-approx except it takes a list for the target instead of a file name."
  (let ((inst-subdiv-pairs (flatten1
			    (mapcar #'(lambda (inst)
					(mapcar #'(lambda (subdiv)
						    (list inst subdiv))
						subdiv-list))
				    inst-list))))
    (let ((target-dat target-list) ;; just rename--this is old code
	  (dat (mapcar #'(lambda (p)
			   (car (deinterleave 3 (read-list-from-file (concatenate 'string
									     (princ-to-string (car p))
									     "_subdivs_"
									     (princ-to-string (cadr p))
									     ".txt")))))
		       inst-subdiv-pairs)))
      (let ((approx (multiple-value-list (approximate-list n bw target-dat dat print-nvisits))))
	
	(with-open-file (ostream (concatenate 'string target-basename "_approx.txt") :direction :output :if-exists :supersede)
	  (mapcar #'(lambda (a i)
		      (format ostream "~a ~a ~a " a (car (nth (car i) inst-subdiv-pairs)) (cadr (nth (car i) inst-subdiv-pairs))))
		  (car approx)
		  (cadr approx))))))
  nil)

(defun write-approx (target-filename target-basename inst-list subdiv-list n bw &optional (target-stride 1) (print-nvisits 0))
  "target-filename: name of file containing data to be approximated.

   target-basename: target-filename without the extension.  will be used to generate the output filename.

   inst-list: list of instrument names.

   subdiv-list: a list of integers representing the subdivision to be used.  

   n: number of iterations.

   bw: bandwidth.

   target-stride: set to 3 if list is time/tempo/phase, or set to 1 if only a list of times

   Example:

   (write-approx \"virtual2_subdivs_2.txt\" \"virtual2_subdivs_2\" '(\"violin1\" \"violin2\" \"viola\" \"cello\") '(3 4 5) 1000 .06)"
  (write-approx-list (car (deinterleave target-stride (read-list-from-file target-filename)))
		     target-basename inst-list subdiv-list n bw print-nvisits))
