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
(defvar c-executable nil)
(defvar c-executable-args nil)
(defvar c-export-vars nil)
(defvar c-build-system nil)
(defvar c-configure-extra-args nil)

;; Internal
(defvar pm-c-exec-process nil)
(defvar pm-c-current-build-system nil)

(defvar pm-c-make-build-system
  '(("make"	.	"make -j$(nproc)")
    ("clean"	.	"make clean")
    ("install"	.	"make install")
    ("test"	.	"make test")))

(defvar pm-c-autotools-build-system
  '(("make"		.	"make -j$(nproc)")
    ("clean"		.	"make clean")
    ("configure"	.	pm-c-autotools-configure)
    ("install"		.	"make install")
    ("test"		.	"make test")))

(defvar pm-c-meson-build-system
  '(("make"		.	"ninja -j$(nproc) -C build")
    ("clean"		.	"ninja clean -C build && rm -rf build")
    ("configure"	.	pm-c-meson-configure)
    ("install"		.	"ninja -C build install")))

(defvar pm-c-build-system-assoc
  '((make	.	pm-c-make-build-system)
    (autotools	.	pm-c-autotools-build-system)
    (meson	.	pm-c-meson-build-system)))

(defun pm-c-autotools-configure ()
  (pm-c-build-target (if c-configure-extra-args
			 (format "./autogen.sh %s && ./configure %s"
				 c-configure-extra-args c-configure-extra-args)
		       "./autogen.sh && ./configure")))

(defun pm-c-meson-configure ()
  (pm-c-build-target (if c-configure-extra-args
			 (concat "meson build " c-configure-extra-args)
		       "meson build")))

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
      (pm-c-build-target (eval t-or-f)))))

(defun pm-c-set-current-build-system ()
  (let ((default-targets '(("interactive" . pm-c-build-interactive)))
	(build-targets (eval (assoc-default c-build-system pm-c-build-system-assoc))))
    (setq pm-c-current-build-system (append build-targets default-targets))))

(defun pm-c-reset-external-vars ()
  (setq c-executable nil)
  (setq c-executable-args nil)
  (setq c-export-vars nil)
  (setq c-build-system nil)
  (setq c-configure-extra-args nil))

(defun pm-c-search (search)
  (interactive "sGrep search: ")
  (let ((default-directory current-root-path))
    (grep (concat grep-command " --include=\"*.[c|h]\" " search))))

(defun pm-c-executable-path ()
  (let* ((default-directory current-root-path)
	 (exec (concat default-directory c-executable)))
    (untramp-path (if (file-exists-p exec)
		      exec
		    (read-file-name "Executable: ")))))

(defun pm-c-process-filter (p str)
  (with-current-buffer (process-buffer p)
    (let ((inhibit-read-only t))
      (save-excursion-if-not-at-point-max (current-buffer)
	(goto-char (point-max))
	(insert (ansi-color-apply
		 (replace-regexp-in-string "\r" "\n" str)))))))

(defun pm-c-exec-insert-header (program)
  (let ((inhibit-read-only t)
	(root (propertize current-root-path 'face 'font-lock-type-face))
	(cmd (propertize program 'face 'font-lock-keyword-face)))
    (insert (format "Working directory: %s\n\n" root))
    (insert (format "Bash command: %s\n\n" cmd))))

(defun pm-c-exec-get-cmd (exec vars)
  (concat (if vars (format "export %s && " vars))
	  exec
	  (if c-executable-args (concat " " c-executable-args))))

(defun pm-c-exec ()
  (interactive)
  (let* ((default-directory current-root-path)
	 (exec (pm-c-executable-path))
	 (exec-name (file-name-base exec))
	 (vars (pm-c-export-vars))
	 (cmd (pm-c-exec-get-cmd exec vars))
	 (buffer-name (format "*project-manager: %s*" exec-name))
	 (process (get-buffer-process buffer-name)))
    (if (get-buffer buffer-name)
	(with-current-buffer buffer-name
	  (let ((inhibit-read-only t)
		(prompt-process (format "%s running, kill it ? " exec-name)))
	    (when (and process (yes-or-no-p prompt-process))
	      (kill-process process))
	    (erase-buffer)))
      (get-buffer-create buffer-name))
    (with-current-buffer buffer-name
      (pm-c-exec-insert-header cmd)
      (read-only-mode t)
      (start-file-process exec-name (current-buffer) "bash" "-c" cmd)
      (set-process-filter (get-buffer-process (current-buffer)) #'pm-c-process-filter)
      (if (get-buffer-window-list)
	  (pop-to-buffer (current-buffer))
	(switch-to-buffer-other-window (current-buffer))))))

(defun pm-c-debug ()
  (interactive)
  (let ((default-directory current-root-path)
	(executable (pm-c-executable-path)))
    (gdb executable)))

(pm-register-backend
 (make-pm-backend :name "c"
		  :open-hook 'pm-c-set-current-build-system
		  :close-hook 'pm-c-reset-external-vars
		  :search 'pm-c-search
                  :compile 'pm-c-compile
		  :exec 'pm-c-exec
		  :debug 'pm-c-debug))

(provide 'pm-c)
