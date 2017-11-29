(defun makenicepictureforloren (filenames instnames)
  (let ((points (deinterleave 4 (read-list-from-file "points.txt"))))
    (let ((pointtimes (mapcan #'(lambda (f tt) (when (and (<= tt 420.0) (= f 0) (> tt 0.0)) (list tt)))
			      (car points)
			      (nth 2 points)))
	  (rehearsal-marks '("A" "B" "C" "D" "E" "F" "G" "H" "I" "K" "L" "M" "N" "O" "P" "Q" "R" "S" "")))
      (gp-command (format nil "set xtics (~{~A~^, ~})" (arithm-seq 420 :step 60)))
      ;; (gp-command (format nil "set x2tics (~{~{~A ~}~^,~})"
      ;; 			  (flatten1 (list-explode 2
      ;; 						  (interleave (mapcar #'(lambda (i) (format nil "\"~A\"" i))
      ;; 								      rehearsal-marks)
      ;; 							      pointtimes)))))
      (gp-command "set grid x2tics")
      (gp-command "set xlabel \"Time (sec.)\"")
      (gp-command "set ylabel \"Tempo (beats/sec.)\"")
      (gp-command "set xrange [0 : 420]")
      (gp-command "set termoption font \"Times,24\"")
      (lplot
       (flatten1 (labels ((rec (files instnames acc)
			    (if (null files)
				(reverse acc)
				(let ((dat (deinterleave 3 (read-list-from-file (car files)))))
				  (let ((dat-pruned
					 (deinterleave 2
						       (mapcan #'(lambda (time tempo)
								   (when (< time 420)
								     (list time
									   tempo)))
							       (car dat)
							       (cadr dat)))))
				  (rec (cdr files)
				       (cdr instnames)
				       (cons (list (car dat-pruned)
						   (cadr dat-pruned)
						   (format nil "with lines title \"~A\"" (car instnames)))
					     acc)))))))
		   (rec filenames instnames '())))))))