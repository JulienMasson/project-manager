(require 'project-manager)

;; External
(defvar aosp-path nil)
(defvar aosp-out-path nil)
(defvar aosp-board-name nil)
(defvar aosp-build-variant nil)
(defvar aosp-compile-options '())
(defvar aosp-other-targets '())
(defvar aosp-env-vars '())
(defvar aosp-debug-func nil)

;; Internal
(defconst pm-android-default-targets
  '(("make"        . "")
    ("current"     . pm-android-build-current)
    ("dist"        . "dist")
    ("boot"        . "bootimage")
    ("dtbo"        . "out/target/product/onyx/dtbo.img")
    ("recovery"    . "recoveryimage")
    ("system"      . "systemimage")
    ("vendor"      . "vendorimage")
    ("super"       . "superimage")
    ("interactive" . pm-android-interactive-target)
    ("clean"       . pm-android-clean-build)
    ("repo-sync"   . pm-android-repo-sync)))

(defvar pm-android-search-tools
  '(("mgrep"       "mgrep "  nil)
    ("cgrep"       "cgrep "  nil)
    ("rcgrep"      "rcgrep " nil)
    ("jgrep"       "jgrep "  nil)
    ("find-module" "mgrep "  " | grep LOCAL_MODULE")))

(defvar pm-android-subprojects
  '(("core"	.	"system/core")
    ("recovery"	.	"bootable/recovery")
    ("out"      .	pm-android-out-subpath)))

(defvar pm-android-targets nil)
(defvar pm-android-compile-history '())
(defvar pm-android-interactive-history '())
(defvar pm-android-subprojects-history '())
(defvar pm-android-compile-options-history '())
(defvar pm-android-search-history '())

(defsubst pm-android-device ()
  (concat aosp-board-name "-" aosp-build-variant))

(defun pm-android-hostname (&optional dir)
  (let ((dir (or dir default-directory)))
    (or (and (tramp-tramp-file-p dir)
	     (with-parsed-tramp-file-name dir info
	       info-host))
	"localhost")))

(defsubst pm-android-env-vars ()
  (when aosp-env-vars
    (concat "export "
	    (mapconcat (lambda (x) (format "%s=%S" (car x) (eval (cdr x))))
		       aosp-env-vars " ")
	    " && ")))

(defsubst pm-android-load-compile-env (&optional silent)
  (concat (pm-android-env-vars)
	  "source build/envsetup.sh "
	  (if silent "&>/dev/null")
	  " && lunch " (pm-android-device)
	  (if silent "&>/dev/null")
	  " && "))

(defun pm-android-out-path ()
  (let ((default-directory aosp-path)
	(env (pm-android-load-compile-env t)))
    (shell-command-to-string (concat env "echo -n $ANDROID_PRODUCT_OUT"))))

(defun pm-android-out-subpath ()
  (replace-regexp-in-string (untramp-path aosp-path) "" aosp-out-path))

;; Search tools
(defun pm-android-search (search command-args)
  (interactive (list (ido-completing-read "Search tools: "
					  (mapcar 'car pm-android-search-tools)
					  nil t nil nil)
		     (read-string "Pattern: "
				  nil 'pm-android-search-history
				  (car pm-android-search-history))))
  (let ((module-dir default-directory)
	(default-directory aosp-path))
    (unless (string-match default-directory module-dir)
      (setq module-dir default-directory))
    (compilation-start (concat
			"/bin/bash -c '"
			"source build/envsetup.sh && "
			(format "cd %s && " (untramp-path module-dir))
			(car (assoc-default search pm-android-search-tools))
			command-args " ./ "
			(car (last (assoc-default search pm-android-search-tools)))
			"'")
		       'grep-mode)
    (with-current-buffer "*grep*"
      (setq default-directory module-dir))))

(defun pm-android-compile (target)
  (interactive (list (ido-completing-read (format "Build target (%s:%s on %s): "
						  (project-name current-project)
						  (pm-android-device) (pm-android-hostname))
					  (mapcar 'car pm-android-targets)
					  nil t nil 'pm-android-compile-history)))
  (let ((t-or-f (assoc-default target pm-android-targets)))
    (if (functionp t-or-f)
	(call-interactively t-or-f)
      (pm-android-build-target t-or-f))))

(defun pm-android-clean-build ()
  (interactive)
  (when (y-or-n-p "Are you sure you want to clean all your repo ?")
    (pm-android-build-target "clean")))

(defun pm-android-repo-sync ()
  (interactive)
  (let ((default-directory aosp-path))
    (compile (concat (pm-android-env-vars)
		     (format "repo sync -j")))))

(defun pm-android-build-current ()
  (interactive)
  (let ((module-dir (untramp-path default-directory))
	(default-directory aosp-path))
    (compile (concat (pm-android-load-compile-env)
		     (format "cd %s && mm -j$(nproc)" module-dir)
		     (if aosp-compile-options
			 (mapconcat 'identity aosp-compile-options " ")
		       "")))))

(defun pm-android-interactive-target (target)
  (interactive (list (read-string (format "Target (default: %s): "
					  (or (car pm-android-interactive-history) ""))
				  nil 'pm-android-interactive-history
				  (car pm-android-interactive-history))))
  (pm-android-build-target target))

(defun pm-android-build-target (target)
  (let ((default-directory aosp-path))
    (compile (concat
	      "/bin/bash -c '"
	      (pm-android-load-compile-env)
	      "make -j$(nproc)"
	      (if aosp-compile-options
		  (mapconcat 'identity aosp-compile-options " ")
		"")
	      " " target "'"))))

(defun pm-android-debug ()
  (interactive)
  (when aosp-debug-func (functionp aosp-debug-func)
	(call-interactively aosp-debug-func)))

(defun pm-android-subproject ()
  (let ((cur-path (expand-file-name default-directory)))
    (while (not (file-exists-p (concat cur-path "/.git")))
      (setq cur-path (expand-file-name (concat cur-path "/.."))))
    (substring cur-path (length aosp-path))))

(defun pm-android-find-file-hook ()
  (let ((file-name (buffer-file-name)))
    (when (and file-name
	       (file-exists-p file-name)
	       (file-writable-p file-name)
	       (string-prefix-p (aosp-path) file-name))
      (project-uniquify-buffer-name (pm-android-subproject)))))

(defun pm-android-load-subprojects ()
  (mapcar (lambda (elem)
	    (let ((v-or-f (cdr elem)))
	      (cons (car elem) (if (functionp v-or-f)
				   (funcall v-or-f)
				 (eval v-or-f)))))
	  (append (project-subprojects current-project)
		  pm-android-subprojects)))

(defun pm-android-find-file ()
  (interactive)
  (project-find-file-subproject (pm-android-load-subprojects)
				'pm-android-subprojects-history))

(defun pm-android-toggle-command (cmd)
  (interactive (list (read-string "Compilation option: " nil
				  'pm-android-compile-options-history)))
  (let ((msg-fmt "Compilation option \"%s\" %s."))
    (if (find cmd aosp-compile-options :test 'string=)
	(progn (setq aosp-compile-options (delete cmd aosp-compile-options))
	       (message (propertize (format msg-fmt cmd "deactivated") 'face 'error)))
      (add-to-list 'aosp-compile-options cmd)
      (message (propertize (format msg-fmt cmd "activated") 'face 'success)))))

(defun pm-android-toggle-showcommands ()
  (interactive)
  (pm-android-toggle-command "showcommands"))

(defun pm-android-open-hook ()
  (setq aosp-path current-root-path)
  (setq aosp-out-path (concat (pm-android-out-path) "/"))
  (setq pm-android-targets (append pm-android-default-targets aosp-other-targets)))

(defun pm-android-reset-external-vars ()
  (setq aosp-path nil)
  (setq aosp-out-path nil)
  (setq aosp-board-name nil)
  (setq aosp-build-variant nil)
  (setq aosp-compile-options '())
  (setq aosp-other-targets '())
  (setq aosp-env-vars '())
  (setq aosp-debug-func nil))

(defun pm-android-lunch-list ()
  (let ((default-directory aosp-path))
    (shell-command-to-string "source build/envsetup.sh >/dev/null 2>&1 && echo ${LUNCH_MENU_CHOICES[*]}")))

(defun pm-android-set-board (board)
  "Set the android board to compile"
  (interactive (list (ido-completing-read "Board: " (split-string (pm-android-lunch-list)))))
  (string-match "\\([a-z0-9_]+\\)-\\(\\w+\\)" board)
  (setq aosp-board-name (match-string 1 board))
  (setq aosp-build-variant (match-string 2 board)))

(pm-register-backend
 (make-pm-backend :name "android"
		  :open-hook 'pm-android-open-hook
		  :close-hook 'pm-android-reset-external-vars
		  :find-file 'pm-android-find-file
		  :find-file-hook 'pm-android-find-file-hook
		  :search 'pm-android-search
		  :compile 'pm-android-compile
		  :debug 'pm-android-debug))

(provide 'pm-android)
