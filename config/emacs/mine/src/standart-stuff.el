(with-system '(gnu/kfreebsd gnu/linux)
  (unless-conf-variable "colorscheme" "pywal"
	(set-frame-parameter nil 'alpha-background 100)
	(add-to-list 'default-frame-alist '(alpha-background . 100)))

  (with-conf-variable "colorscheme" "pywal"
	(set-frame-parameter nil 'alpha-background 93)
	(add-to-list 'default-frame-alist '(alpha-background . 93))))

(defvar emacs-font-free "JetBrains Mono NFM-13"
  "Emacs font on free systems")

(defvar emacs-font-nt "JetBrainsMono NF-13"
  "Emacs font on nt systems")

(with-system '(gnu/kfreebsd gnu/linux)
  (add-to-list 'default-frame-alist `(font . ,emacs-font-free)))

(with-system '(windows-nt)
  (add-to-list 'default-frame-alist `(font . ,emacs-font-nt)))

(setq-default tab-width 4)

(with-system '(gnu/linux gnu/kfreebsd)
  (unless-conf-variable "profile" "no-wm"
	(setq frame-title-format
		  '((:eval (if (buffer-modified-p) " "))
			(:eval (if (buffer-file-name)
					   (abbreviate-file-name (buffer-file-name)) "%b"))
			" - GNU emacs"))))

(with-system '(windows-nt darwin haiku cygwin)
  (setq frame-title-format
		'((:eval (if (buffer-modified-p) "* "))
		  (:eval (if (buffer-file-name)
					 (abbreviate-file-name (buffer-file-name)) "%b"))
		  " - GNU emacs")))

(setq display-line-numbers-type 'visual
	  display-line-numbers-current-absolute t
	  display-line-numbers-width 4 
	  display-line-numbers-widen t)

(global-display-line-numbers-mode)

(setq initial-major-mode 'org-mode
      initial-scratch-message "#+TITLE: Scratch buffer
#+LATEX_HEADER: \\usepackage[english,ukrainian]{babel}
#+LATEX_HEADER: \\usepackage{mhchem}

")

(setq backup-directory-alist '((".*" . "~/.config/emacs/mine/backups")))

(setq kill-buffer-query-functions nil)

(setq erc-join-buffer 'buffer)

(global-set-key (kbd "C-c e") 'erc-tls)

(with-conf-variable "emacsEnableWarnings" "no"
  (setq warning-minimum-level :error))

(setq org-export-coding-system 'utf-8
	  coding-system-for-read 'utf-8-unix
	  coding-system-for-write 'utf-8-unix)

(setq column-number-mode t)

(with-conf-variable "emacsEnableOldVersionConfig" "no"
  (which-key-mode))

(defun truncate-lines-hook ()
  (setq truncate-lines nil))

;; (add-hook 'org-mode-hook #'truncate-lines-hook)

(defun truncate-lines-toggle ()
  (interactive)
  (setq-local truncate-lines (not truncate-lines))
  (message "truncate-lines has changed to %s" truncate-lines))

(global-set-key (kbd "C-c h") 'truncate-lines-toggle)

(defun change-transparency (level)
  "Prompt for a transparency level and apply it to all frames"
  (interactive
   (list
    (string-to-number (read-string "Enter transparency level (0 - 100) >>> ")))
   )
  (dolist (frame (frame-list))
    (set-frame-parameter frame 'alpha-background level))
  (setq default-frame-alist (assq-delete-all 'alpha-background default-frame-alist))
  (add-to-list 'default-frame-alist (cons 'alpha-background level)))

(global-set-key (kbd "C-c g") 'change-transparency)

(defun improved-enlarge-window-horizontally ()
  (interactive)
  (enlarge-window-horizontally 5))

(defun improved-shrink-window-horizontally ()
  (interactive)
  (shrink-window-horizontally 5))

(defun improved-enlarge-window ()
  (interactive)
  (enlarge-window 3))

(defun improved-shrink-window ()
  (interactive)
  (shrink-window 3))

(global-bind '(("C-&" . improved-enlarge-window-horizontally)
			   ("C-*" . improved-shrink-window-horizontally)
			   ("C-M-&" . improved-shrink-window)
			   ("C-M-*" . improved-enlarge-window)))

(defun kill-buffer-close-window ()
  (interactive)
  (kill-buffer)
  (delete-window))

(global-set-key (kbd "C-x c") 'kill-buffer-close-window)

(global-bind '(("i" . package-install)
			   ("d" . package-delete)
			   ("l" . package-list-packages)) "C-c P")

(global-set-key (kbd "C-S-n") 'display-line-numbers-mode)

(global-set-key (kbd "C-c C-<return>") 'eval-buffer)

(defun restart-emacs-ask ()
  (interactive)
  (when (y-or-n-p "Are you sure to restart emacs?")
	(restart-emacs)))

(global-set-key (kbd "C-c R") 'restart-emacs-ask)

(global-set-key (kbd "C-c m m") 'make-directory)

(defun make-directory-dired (directory)
  "Make directory DIRECTORY and open in dired"
  (interactive
   (list (read-file-name "Make directory and open in dired: "
						 default-directory default-directory nil nil)))
  (make-directory directory)
  (dired directory))

(global-set-key (kbd "C-c m d") 'make-directory-dired)

(defun truncate-toggle ()
  (interactive)
  (setq truncate-lines (not truncate-lines)))

(global-set-key (kbd "C-c q") 'truncate-toggle)

(defun scroll-up-like-mwheel ()
  (interactive)
  (scroll-down-command 1))

(defun scroll-down-like-mwheel ()
  (interactive)
  (scroll-up-command 1))

(global-bind '(("C-M-}" . scroll-up-like-mwheel)
			   ("C-M-{" . scroll-down-like-mwheel)))

(defun emacs-reload-config ()
  "Load config from ~/.config/emacs/mine/init.el"
  (interactive)
  (load-file "~/.config/emacs/mine/init.el"))

(global-set-key (kbd "C-x R") 'emacs-reload-config)

(defun org-latex-preview-scale-change (scale)
  "Change scale of LaTeX previews"
  (interactive
   (list
	(read-number "Pick LaTeX preview scale (1.5 is normal) >>> ")))
  (setq org-format-latex-options
		(plist-put org-format-latex-options :scale scale))
  (message "LaTeX preview scale was changed to %s" scale))

(with-eval-after-load 'org
  (define-key org-mode-map (kbd "C-c C-x S") 'org-latex-preview-scale-change))

(global-set-key (kbd "C-c a") 'align-regexp)

(global-bind '(("C-4 C-f" . find-file-other-window)
			   ("C-1" . delete-other-windows)
			   ("C-o" . other-window)) "C-x")

(defun other-window-backward ()
  (interactive)
  (other-window -1))

(global-set-key (kbd "C-x C-S-o") 'other-window-backward)

(global-set-key (kbd "C-c s") 'scratch-buffer)

(global-set-key (kbd "C-S-F") 'find-grep-dired)

(defun split-below-switch-window (&optional no-redisplay)
  (interactive)
  (split-window-below)

  (unless no-redisplay
	(redisplay))

  (other-window 1))

(defun split-right-switch-window (&optional no-redisplay)
  (interactive)
  (split-window-right)

  (unless no-redisplay
	(redisplay))

  (other-window 1))

(global-bind '(("C-2" . split-below-switch-window)
			   ("C-3" . split-right-switch-window)) "C-x")

(defun mx ()
  (interactive)
  (call-interactively 'execute-extended-command))

(defun split-below-mx ()
  (interactive)
  (split-below-switch-window)
  (mx))

(defun split-right-mx ()
  (interactive)
  (split-right-switch-window)
  (mx))

(global-bind '(("C-#" . split-right-mx)
			   ("#"   . split-right-mx)
			   ("C-@" . split-below-mx)
			   ("@"   . split-below-mx)) "C-x")

(defun list-buffers-other-window ()
  (interactive)
  (list-buffers)
  (other-window 1))

(global-set-key (kbd "C-x C-b") 'list-buffers-other-window)

(defun copy-buffer-name ()
  (interactive)
  (kill-new buffer-file-name)
  (message "`%s' copied to clipboard" (buffer-file-name)))

(global-set-key (kbd "C-c {") 'copy-buffer-name)

(global-bind '(("n" . tab-new)
			   ("c" . tab-close)
			   ("C" . tab-close-other)
			   ("b" . switch-to-buffer-other-tab)
			   ("f" . find-file-other-tab)
			   ("d" . dired-other-tab)
			   ("s" . tab-switch)
			   ("r" . tab-rename)
			   ("o" . other-tab-prefix)) "C-x T")

(unless-conf-variable "profile" "macos"
  (menu-bar-mode -1)
  )

(tool-bar-mode -1)
(scroll-bar-mode -1)

(setq vc-follow-symlinks t)

(dolist (keybinding '("C-z" "C-x C-z"))
  (global-unset-key (kbd keybinding)))

(with-conf-variable "emacsEnableTetris" "no"
  (defun tetris ()
	"Print message to not play tetris"
	(interactive)
	(message "Don't play in it now. Read the Bible or study something")))
