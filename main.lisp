(in-package #:map-bind)

(defun %parse-binding (binding)
  (destructuring-bind (vars &rest values) binding
    (values (etypecase vars
	      (list (copy-seq vars))
	      (symbol (list vars)))
	    (copy-seq values))))

(defmacro map-bind (mapping-call-prologue bindings &body body)
  (multiple-value-bind (lambda-list mapping-call-arguments)
      (let (ll mca)
        (dolist (binding bindings (values (apply #'nconc (nreverse ll))
                                          (apply #'nconc (nreverse mca))))
          (multiple-value-bind (vars values) (%parse-binding binding)
            (push vars ll)
            (push values mca))))
    `(,@mapping-call-prologue
      (lambda ,lambda-list ,@body)
      ,@mapping-call-arguments)))

