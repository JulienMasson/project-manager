;;; pm-u-boot.el --- U-Boot Project Management

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
(defvar u-boot-arch nil)
(defvar u-boot-cross-compile nil)
(defvar u-boot-defconfig nil)
(defvar u-boot-toolchain-path nil)
(defvar u-boot-extra-targets nil)

;; Internal
(defvar pm-u-boot-default-targets
  '(("make"       . "make -j$(nproc)")
    ("clean"      . "make clean")
    ("defconfig"  . pm-u-boot-build-defconfig)
    ("menuconfig" . pm-u-boot-make-menuconfig)))
(defvar pm-u-boot-targets nil)

(defun pm-u-boot-path-env ()
  (format "PATH=%sbin:$PATH" (untramp-path u-boot-toolchain-path)))

(defun pm-u-boot-compile-env ()
  (format "ARCH=%s CROSS_COMPILE=%s" u-boot-arch u-boot-cross-compile))

(defun pm-u-boot-build-target (target)
  (let ((default-directory current-root-path)
	(path-env (pm-u-boot-path-env))
	(compile-env (pm-u-boot-compile-env)))
    (compile (format "export %s %s && %s" path-env compile-env target))))

(defun pm-u-boot-build-defconfig ()
  (pm-u-boot-build-target (concat "make " u-boot-defconfig)))

(defun pm-u-boot-make-menuconfig ()
  (interactive)
  (let* ((default-directory current-root-path)
	 (path-env (pm-u-boot-path-env))
	 (compile-env (pm-u-boot-compile-env))
	 (buffer-name (format "menuconfig-%s" (project-name current-project)))
	 (term-buffer (make-term buffer-name "/bin/bash")))
    (set-buffer term-buffer)
    (multi-term-internal)
    (when (tramp-tramp-file-p current-root-path)
      (pm-kernel-term-setup-remote term-buffer))
    (pm-kernel-term-send-command (format "export %s %s" path-env compile-env))
    (pm-kernel-term-send-command "make menuconfig")
    (setq-local global-hl-line-mode nil)
    (switch-to-buffer term-buffer)))

(defun pm-u-boot-compile (target)
  (interactive (list (completing-read "Build target: "
				      (mapcar 'car pm-u-boot-targets))))
  (let ((t-or-f (assoc-default target pm-u-boot-targets)))
    (if (functionp t-or-f)
	(funcall t-or-f)
      (pm-u-boot-build-target t-or-f))))

(defun pm-u-boot-search (search)
  (interactive "sGrep search: ")
  (let ((default-directory current-root-path))
    (grep (concat grep-command " --include=\"*.[c|h]\" " search))))

(defun pm-u-boot-open-hook ()
  (setq pm-u-boot-targets (append pm-u-boot-default-targets u-boot-extra-targets)))

(defun pm-u-boot-close-hook ()
  (setq u-boot-arch nil)
  (setq u-boot-cross-compile nil)
  (setq u-boot-defconfig nil)
  (setq u-boot-toolchain-path nil)
  (setq u-boot-extra-targets nil))

(pm-register-backend
 (make-pm-backend :name "u-boot"
		  :open-hook 'pm-u-boot-open-hook
		  :close-hook 'pm-u-boot-close-hook
		  :search 'pm-u-boot-search
		  :compile 'pm-u-boot-compile))

(provide 'pm-u-boot)
