(async
  (format t "~%Finished. The execution took ~a seconds.~%"
          (/ (- (get-internal-real-time) *timestamp*) internal-time-units-per-second 1.0))

<<<<<<< d2eafe0d16620b22081789c992c7218750b240b9
  (if (= *passed-tests* *total-tests*)
      (format t "All the tests (~a) passed successfully.~%" *total-tests*)
      (format t "~a/~a test(s) passed successfully.~%     (~d% of expected success)~%"
              *passed-tests* *total-tests*
              (floor (* 100 (/ *passed-tests*
                               (- *total-tests*
                                  *expected-failures*
                                  *unexpected-passes*))))))
  
  (unless (zerop *expected-failures*)
    (format t "~a test(s) failed expectedly.~%" *expected-failures*))

  (unless (zerop *unexpected-passes*)
    (format t "~a test(s) passed unexpectedly.~%" *unexpected-passes*))
  
  (let (unbound)
    (do-external-symbols (symbol :jscl)
      (unless (or (boundp symbol) (fboundp symbol))
        (push symbol unbound)))
    (when unbound 
      (format t "Unbound, exported symbols in JSCL package:~%")
      (dolist (symbol unbound)
        (format t " • ~a~%" symbol))))
  
  (terpri)

  #+jscl
  (when #j:phantom
    (#j:phantom:exit *failed-tests*))
  )
=======
 (if (= *passed-tests* *total-tests*)
     (format t "All the tests (~a) passed successfully.~%" *total-tests*)
     (format t "~a/~a test(s) passed successfully.~%     (~d% of expected success)~%"
             *passed-tests* *total-tests*
             (floor (* 100 (/ *passed-tests*
                              (- *total-tests*
                                 *expected-failures*
                                 *unexpected-passes*))))))

 (unless (zerop *expected-failures*)
   (format t "~a test(s) failed expectedly.~%" *expected-failures*))

 (unless (zerop *unexpected-passes*)
   (format t "~a test(s) passed unexpectedly.~%" *unexpected-passes*))

 (let (unbound)
   (do-external-symbols (symbol :jscl)
     (unless (or (boundp symbol) (fboundp symbol))
       (push symbol unbound)))
   (when unbound
     (format t "Unbound, exported symbols in JSCL package:~%")
     (dolist (symbol unbound)
       (format t " • ~a~%" symbol))))

 (terpri)

 #+jscl
 (when #j:phantom
   (#j:phantom:exit *failed-tests*))
 )
>>>>>>> Emacs auto-indent tests-report.lisp
