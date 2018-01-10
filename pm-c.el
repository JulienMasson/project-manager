(require 'project-manager)

(defun pm-c-compile ()
  (interactive)
  (if (fboundp 'project-compile-fun)
      (project-compile-fun current-project)
    (call-interactively 'compile)))

(pm-register-backend
 (make-pm-backend :name "c"
                  :compile 'pm-c-compile
		  :find-file 'project-find-file-subproject))

(provide 'pm-c)
