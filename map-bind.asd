(asdf:defsystem #:hexstream-project-template

  :author "Jean-Philippe Paradis <hexstream@gmail.com>"

  ;; See the UNLICENSE file for details.
  :license "Public Domain"

  :description "MAP-BIND is a macro that allows visual grouping of variables with their corresponding values (not necessarily 1:1) in calls to mapping operators when using an inline LAMBDA. It does so in a way that automatically supports virtually every existing and future mapping operator, all lambda keywords and FUNCALL/APPLY/MULTIPLE-VALUE-CALL variations."

  :depends-on ()

  :version "1.0"
  :serial cl:t
  :components ((:file "package")
	       (:file "main")))
