;;; pm-c.el --- C Project Management

;; Copyright (C) 2019 Julien Masson

;; Author: Julien Masson <massonju.eseo@gmail.com>
;; URL: https://github.com/JulienMasson/jm-config/

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(require 'project-manager)

;; External
(defvar c-export-vars nil)
(defvar c-build-system nil)

;; Internal
(defvar pm-c-current-build-system nil)

(defvar pm-c-make-build-system
  '(("make"	.	"make -j$(nproc)")
    ("clean"	.	"make clean")
    ("install"	.	"make install")
    ("test"	.	"make test")))

(defvar pm-c-autotools-build-system
  '(("make"		.	"make -j$(nproc)")
    ("clean"		.	"make clean")
    ("configure"	.	"./autogen.sh && ./configure")
    ("install"		.	"make install")
    ("test"		.	"make test")))

(defvar pm-c-meson-build-system
  '(("make"		.	"ninja -j$(nproc) -C build")
    ("clean"		.	"ninja clean -C build && rm -rf build")
    ("configure"	.	"meson build")
    ("install"		.	"ninja -C build install")))

(defvar pm-c-build-system-assoc
  '((make	.	pm-c-make-build-system)
    (autotools	.	pm-c-autotools-build-system)
    (meson	.	pm-c-meson-build-system)))

(defun pm-c-error-msg (msg)
  (message (concat (propertize "Error: " 'face 'error) msg)))

(defun pm-c-export-vars ()
  (when c-export-vars
    (mapconcat (lambda (x)
		 (format "%s=%s" (car x) (cdr x)))
	       c-export-vars " ")))

(defun pm-c-build-target (target)
  (let ((default-directory current-root-path)
	(vars (pm-c-export-vars)))
    (compile (concat (when vars
		       (format "export %s && " vars))
		     target))))

(defun pm-c-build-interactive ()
  (let ((target (read-string "Build command: ")))
    (pm-c-build-target target)))

(defun pm-c-compile (target)
  (interactive (list (completing-read "Build target: "
				      (mapcar 'car pm-c-current-build-system))))
  (let ((t-or-f (assoc-default target pm-c-current-build-system)))
    (if (functionp t-or-f)
  	(funcall t-or-f)
      (pm-c-build-target t-or-f))))

(defun pm-c-set-current-build-system ()
  (let ((default-targets '(("interactive" . pm-c-build-interactive)))
	(build-targets (eval (assoc-default c-build-system pm-c-build-system-assoc))))
  (setq pm-c-current-build-system (append build-targets default-targets))))

(defun pm-c-reset-external-vars ()
  (setq c-export-vars nil)
  (setq c-build-system nil))

(defun pm-c-search (search)
  (interactive "sGrep search: ")
  (let ((default-directory current-root-path))
    (grep (concat grep-command " --include=\"*.[c|h]\" " search))))

(defun pm-c-debug ()
  (interactive)
  (let ((default-directory current-root-path)
	(executable (project-name current-project)))
    (if (file-exists-p executable)
	(gdb executable)
      (pm-c-error-msg (format "Executable %s not found" executable)))))

(pm-register-backend
 (make-pm-backend :name "c"
		  :open-hook 'pm-c-set-current-build-system
		  :close-hook 'pm-c-reset-external-vars
		  :search 'pm-c-search
                  :compile 'pm-c-compile
		  :debug 'pm-c-debug))

(provide 'pm-c)
