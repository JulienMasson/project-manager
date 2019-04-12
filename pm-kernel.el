;;; pm-kernel.el --- Kernel Project Management

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
(defvar kernel-arch nil)
(defvar kernel-cross-compile nil)
(defvar kernel-defconfig nil)
(defvar kernel-toolchain-path nil)
(defvar kernel-out-files nil)

;; Internal
(defvar pm-kernel-subprojects-history '())
(defvar pm-kernel-targets
  '(("kernel"		.	"make -j$(nproc)")
    ("all"		.	pm-kernel-build-all)
    ("clean"		.	"make clean")
    ("defconfig"	.	pm-kernel-build-defconfig)))

(defvar pm-kernel-subprojects '(("out"	. kernel-out-files)))

(defvar pm-kernel-debug-default-port "/dev/ttyUSB0")
(defvar pm-kernel-debug-default-speed 115200)

(defun pm-kernel-error-msg (msg)
  (message (concat (propertize "Error: " 'face 'error) msg)))

(defun pm-kernel-path-env ()
  (format "PATH=%s/bin:$PATH" (untramp-path kernel-toolchain-path)))

(defun pm-kernel-compile-env ()
  (format "ARCH=%s CROSS_COMPILE=%s" kernel-arch kernel-cross-compile))

(defun pm-kernel-build-target (target)
  (let ((default-directory current-root-path)
	(path-env (pm-kernel-path-env))
	(compile-env (pm-kernel-compile-env)))
    (compile (format "export %s %s && %s" path-env compile-env target))))

(defun pm-kernel-build-defconfig ()
  (pm-kernel-build-target (concat "make " kernel-defconfig)))

(defun pm-kernel-build-all ()
  (let ((default-directory current-root-path))
    (if (file-directory-p kernel-out-files)
	(pm-kernel-build-target (mapconcat
				 'identity
				 `("make -j$(nproc)"
				   ,(concat "make install INSTALL_PATH=" kernel-out-files)
				   ,(concat "make headers_install INSTALL_HDR_PATH=" kernel-out-files)
				   ,(concat "make modules_install INSTALL_MOD_PATH=" kernel-out-files))
				 " && "))
      (pm-kernel-error-msg (format "%s doesn't exist" kernel-out-files)))))

(defun pm-kernel-compile (target)
  (interactive (list (completing-read "Build target: "
				      (mapcar 'car pm-kernel-targets))))
  (let ((t-or-f (assoc-default target pm-kernel-targets)))
    (if (functionp t-or-f)
	(funcall t-or-f)
      (pm-kernel-build-target t-or-f))))

(defun pm-kernel-build-subprojects ()
  (append `(("root"	.	,current-root-path)
	    ("out"	.	,kernel-out-files))
	  (mapcar (lambda (p)
		    `(,(car p) . ,(concat current-root-path (cdr p))))
		  (project-subprojects current-project))))

(defun pm-kernel-ido-find-file ()
  (interactive)
  (let* ((subprojects (pm-kernel-build-subprojects))
	 (subprojects (cl-remove-if-not 'file-directory-p subprojects
					:key #'cdr))
	 (subproject (ido-completing-read (format "Subproject (project %s): "
					      (project-name current-project))
				      subprojects
				      nil t nil 'pm-kernel-subprojects-history))
	 (default-directory (assoc-default subproject subprojects)))
    (ido-find-file)))

(defun pm-kernel-search (search)
  (interactive "sGrep search: ")
  (let ((default-directory current-root-path))
    (grep (concat grep-command " --include=\"*.[c|h]\" " search))))

(defun pm-kernel-debug (trigger)
  (interactive (list (yes-or-no-p "Trigger KGDB ? ")))
  (let ((default-directory current-root-path)
	(gdb-default-cmd (format "%s/bin/%sgdb" kernel-toolchain-path
				 kernel-cross-compile))
	(vmlinux (concat current-root-path "/vmlinux"))
	(port pm-kernel-debug-default-port)
	(speed pm-kernel-debug-default-speed)
	(kgdb-default-root-cmd "su"))
    (kgdb vmlinux port speed trigger)))

(pm-register-backend
 (make-pm-backend :name "kernel"
		  :find-file 'pm-kernel-ido-find-file
		  :search 'pm-kernel-search
		  :compile 'pm-kernel-compile
		  :debug 'pm-kernel-debug))

(provide 'pm-kernel)
