(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
						 ("nongnu" . "https://elpa.nongnu.org/nongnu/")
						 ("elpa" . "https://elpa.gnu.org/packages/") 
						 ("melpa" . "https://melpa.org/packages/")))

(require 'package)
(package-initialize)

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(use-package evil
  :ensure t
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-i-jump nil)
  :config
  (evil-mode 1)
  (evil-set-leader 'normal (kbd ";"))
  (evil-set-undo-system 'undo-redo)

  ;; insert mode
  (map-bind '(("\C-a" . evil-first-non-blank)
			  ("\C-e" . end-of-line)
			  ("\C-n" . next-line)
			  ("\C-p" . previous-line)) evil-insert-state-map)

  ;; visual mode
  (map-bind '(
			  ("\C-e" . end-of-line)
			  ) evil-visual-state-map)

  ;; normal mode
  (map-bind '(("\C-a"        . evil-first-non-blank)
			  ("\C-e"        . end-of-line)
			  ("gcc"         . comment-line)
			  ("gr"          . revert-buffer)
			  ("<leader>SPC" . counsel-dired)
			  ("<leader>w"   . split-right-switch-window)
			  ("<leader>W"   . split-below-switch-window)
			  ("<leader>of"  . counsel-find-file)
			  ("<leader>or"  . counsel-recentf)
			  ("<leader>xw"  . delete-other-windows)
			  ("<leader>xc"  . delete-window)
			  ("<leader>xC"  . kill-buffer-close-window)
			  ("<leader>Bb"  . counsel-evil-marks)
			  ("<leader>Ba"  . bookmark-set)
			  ("<leader>Bj"  . counsel-bookmark)
			  ("<leader>Bl"  . bookmark-bmenu-list)
			  ("<leader>Bd"  . bookmark-delete)
			  ("<leader>Br"  . bookmark-rename)
			  ("<leader>Bs"  . bookmark-save)
			  ("<leader>bb"  . switch-to-buffer)
			  ("<leader>bo"  . switch-to-buffer-other-window)
			  ("<leader>k"   . kill-buffer)
			  ("<leader>s"   . swiper)
			  ("<leader>;"   . evil-window-next)
			  ("<leader>:"   . other-window-backward)) evil-normal-state-map))

(defalias 'org-format-long-string
  (kmacro ": s / [-|­] SPC / / g <return> : s / ­ / / g <return> g q q")
  "Delete all '- ' and press gqq")

(evil-set-register ?f 'org-format-long-string)

(defalias 'write-numbers-list
  (kmacro "y y p C-=")
  "Copy line and multiple first number that appears")

(evil-set-register ?n 'write-numbers-list)

(defalias 'org-auto-link
  (kmacro "y $ v $ h C-c C-l C-y <return> <return>")
  "Link from point to end to header in current org document")

(evil-set-register ?l 'org-auto-link)

(defun org-auto-link-ask-file (filename)
  "Ask for file to link in and use everything from point to end as header name"
  (interactive "FChoose file to link: ")
  (let ((header-name (buffer-substring (point) (line-end-position))))
    (kill-region (point) (line-end-position))
    (org-insert-link nil
					 (format "file:%s::%s" filename header-name) header-name)))

(with-eval-after-load 'org
  (dolist (keybinding '("C-c u u" "M-o l"))
	(define-key org-mode-map (kbd keybinding) 'org-auto-link-ask-file)))

(defalias 'org-move-scheduled-deadline
  (kmacro "d f > A SPC <escape> p 0 x")
  "Move '.*>' to the end")

(evil-set-register ?m 'org-move-scheduled-deadline)

(defalias 'check-fold-go-down
  (kmacro "C-S-c <tab> C-M-SPC")
  "Check the checkbox, fold and go down to the next")

(defalias 'check-go-down
  (kmacro "C-S-c C-M-SPC")
  "Check the checkbox and go down to the next")

(evil-set-register ?c 'check-fold-go-down)
(evil-set-register ?x 'check-go-down)

(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init))

(use-package evil-numbers
  :ensure t
  :bind (:map evil-normal-state-map
			  ("C--" . evil-numbers/dec-at-pt)
			  ("C-=" . evil-numbers/inc-at-pt)))

(use-package nerd-icons
  :ensure t
  :init (require 'nerd-icons nil t)
  :bind (:map global-map
			  ("C-c n" . nerd-icons-insert)))

(use-package nerd-icons-dired
  :ensure t
  :hook
  (dired-mode . nerd-icons-dired-mode))

(use-package all-the-icons
  :ensure t
  :init (require 'all-the-icons))

(use-package flycheck
  :ensure t
  :hook ((org-mode . flycheck-mode)
		 (prog-mode . (lambda () (flycheck-mode -1)))))

(use-package ivy
  :ensure t
  :init
  (ivy-mode)
  :config
  (setq ivy-use-virtual-buffers t
        ivy-count-format "(%d/%d) "
		ivy-initial-inputs-alist nil
        enable-recursive-minibuffers t))

(use-package counsel
  :ensure t
  :bind
  ("C-c k t" . counsel-load-theme)
  ("C-c l i" . counsel-imenu)
  )

(use-package nerd-icons-ivy-rich
  :after ivy
  :ensure t
  :init
  (nerd-icons-ivy-rich-mode 1)
  (ivy-rich-mode 1))

(use-package helpful
  :ensure t
  :init
  (setq counsel-describe-function-function #'helpful-callable
		counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

(defun indent-between-pair (&rest _ignored)
  "smartparens posthandler hook.
Create indent after pressing RET inside pair symbols"
  (newline)
  (indent-according-to-mode)
  (forward-line -1)
  (indent-according-to-mode))

(defun space-between-pair (&rest _ignored)
  "smartparens posthandler hook.
Create space after pressing SPC inside pair symbols and go backwards"
  (insert " ")
  (backward-char))

(use-package smartparens
  :ensure t
  :hook (prog-mode text-mode markdown-mode org-mode conf-unix-mode sxhkdrc-mode)
  :config
  (require 'smartparens-config)
  (setq smartparens-posthandlers '((indent-between-pair "RET")
								   (space-between-pair "SPC"))
		smartparens-posthandler-call-symbols '("{" "[" "(")
		smartparens-strict-mode nil)

  (sp-local-pair 'org-mode "~" nil :actions nil)
  (sp-local-pair 'org-mode "=" "=" :wrap "C-+")
  (sp-local-pair 'org-mode "*" "*" :wrap "C-_")
  (sp-local-pair 'org-mode "$" "$" :wrap "C-M-$")

  (dolist (symbol smartparens-posthandler-call-symbols)
	(sp-pair symbol nil :post-handlers smartparens-posthandlers)))

(defun disable-smartparens-during-norm (orig-fun &rest args)
  "Disable Smartparens features during :norm command execution."
  (let ((sp-post-handlers nil)                ;; Disable posthandlers
        (sp-autoinsert-pair nil)              ;; Disable auto-pairing
        (sp-autoinsert-space nil)             ;; Disable auto space insertion
        (sp-pair-overlay-keymap nil)          ;; Disable overlays
        (sp-pair-overlay-list nil))           ;; Disable overlay list
    (apply orig-fun args)))                   ;; Run the original :norm

(advice-add 'evil-ex-normal :around #'disable-smartparens-during-norm)

(defun insert-header-counter ()
  (interactive)
  (end-of-line)
  (insert " [/] [%]")
  (org-ctrl-c-ctrl-c))

(defun insert-counter ()
  (interactive)
  (end-of-line)
  (insert " [/]")
  (org-ctrl-c-ctrl-c))

(defun insert-time ()
  (interactive)
  (insert (shell-command-to-string "date +%H:%M"))
  (evil-insert 1))

(defun insert-date ()
  (interactive)
  (insert (shell-command-to-string "date +%d.%m.%Y"))
  (evil-insert 1))

(defun create-header-insert-time ()
  (interactive)
  (org-insert-heading-after-current)
  (insert-time))

(defun create-header-insert-date ()
  (interactive)
  (org-insert-heading-after-current)
  (insert-date))

(defun org-agenda-delete-windows ()
  (interactive)
  (org-agenda)
  (delete-other-windows))

(defun load-prettify-symbols ()
  (interactive)
  (setq prettify-symbols-alist
		'(
		  ("#+begin_src"     . ?)
		  ("#+BEGIN_SRC"     . ?)
		  ("#+end_src"       . ?)
		  ("#+END_SRC"       . ?)
		  ("#+begin_example" . ?)
		  ("#+BEGIN_EXAMPLE" . ?)
		  ("#+end_example"   . ?)
		  ("#+END_EXAMPLE"   . ?)
		  ("#+begin_quote"   . ?)
		  ("#+BEGIN_QUOTE"   . ?)
		  ("#+end_quote"     . ?)
		  ("#+END_QUOTE"     . ?)
		  ("lambda"          . ?λ)
		  ))
  (prettify-symbols-mode 1))

(add-hook 'org-mode-hook 'load-prettify-symbols)

(defun disable-evil-autoindent ()
  (setq evil-auto-indent nil))

(add-hook 'org-mode-hook #'disable-evil-autoindent)

(defun org-mode-configure-faces ()
  (with-eval-after-load 'org-faces
	  (dolist (face '((org-level-1 . 1.3)
					  (org-level-2 . 1.2)
					  (org-level-3 . 1.05)
					  (org-level-4 . 1.02)
					  (org-level-5 . 1.02)
					  (org-level-6 . 1.02)
					  (org-level-7 . 1.02)
					  (org-level-8 . 1.02)))
	  (set-face-attribute (car face) nil
						  :font "JetBrainsMono NFM Bold" :height (cdr face)))))

(defun org-create-metric (type key title file columns &rest parameters)
  "Create an org-capture template entry for metrics"
  `(,key ,title ,type (file ,file) ,columns ,@parameters))

(defun org-mode-change-variables ()
  (setq-default org-display-custom-times t)
  (setq org-log-done 'time
		org-export-with-broken-links t
		org-hide-emphasis-markers t
		org-time-stamp-custom-formats '("<%d.%m.%Y %a>" . "<%d.%m.%Y %H:%M %a>")
		org-format-latex-options (plist-put org-format-latex-options :scale 1.5)
		org-agenda-span 10
		org-agenda-start-on-weekday nil
		org-startup-with-inline-images t
		org-startup-indented t
		org-startup-folded t
		org-agenda-skip-scheduled-if-done t
		org-priority-highest ?A
		org-plantuml-jar-path "/usr/share/java/plantuml/plantuml.jar"
		org-priority-default ?B
		org-priority-lowest  ?C
		org-latex-create-formula-image-program 'dvipng
		org-image-actual-width nil
		org-ellipsis " "
		org-agenda-include-diary t
		org-list-allow-alphabetical t
		org-src-preserve-indentation 0)

  (setq org-capture-templates
      (list
       '("m" "Metrics")
	   (org-create-metric 'table-line "mp" "Head pain" "~/Org/metrics/pain.org"
                          "| %U | %^{length} | %^{strength} | %^{in nose?} |"
                          :kill-buffer t)

	   (org-create-metric 'table-line "mt" "Body temperature"
						  "~/Org/metrics/body-temperature.org"
                          "| %U | %^{body temperature} | %^{how do you feel} |"
                          :kill-buffer t)

	   (org-create-metric 'table-line "mw" "Body weight" "~/Org/metrics/weight.org"
						  "| %U | %^{weight} | %^{is fat?} |"
                          :kill-buffer t)

	   (org-create-metric 'table-line "mb" "Blood leak" "~/Org/metrics/blood-leak.org"
                          "| %U | %^{duration (seconds)} | %^{something special?} |"
                          :kill-buffer t)

	   (org-create-metric
		'table-line "mD" "Dark urge"
		"~/Org/metrics/dark-urge.org"
		"| %U | %^{thought} | %^{person that was harmed} | %^{event before} | %^{strength} | %^{comments}"
		:kill-buffer t)

	   (org-create-metric 'entry "md" "Diary" "~/Org/metrics/thoughts.org"
						  "\n* %^{Thought title}\n%U\n%?\n"
						  :clock-in t :clock-resume t :prepend t)
	   ))

  (setq org-agenda-custom-commands '(("M" "Monthly agenda"
									  ((agenda "" ((org-agenda-span 'month)))) nil)
									  ("y" "Yearly agenda"
									  ((agenda "" ((org-agenda-span 'year)))) nil)))

  (setq org-todo-keywords
	  '((sequence
		  "TODO(t)" "PROJECT(p)" "DOING(T)" "ACTIVE(a)" "NEXT(n)"
		  "START(s)" "FINISH(R)" "PLAN(P)" "IMPROVE(i)"
		  "|"
		  "DONE(d/!)" "CANCEL(c@/!)" "WAIT(w)" "FAILED(f)"
		  )
		(sequence "HUMAN(h)" "|" "DONE(d)")
		(sequence "BUG(b)" "|" "DONE(F)")
		(sequence "FIX(d)" "|" "DONE(d)")
		(sequence "REMEMBER(r)" "|" "DONE(d)")
		(sequence "LEARN(L)" "|" "DONE(d)"))))

(use-package org
  :ensure t
  :straight t
  :after evil
  :config
  (require 'ox-md)
  (org-mode-change-variables)
  (org-mode-configure-faces)

  (plist-put org-format-latex-options :background "Transparent")
  
  (org-babel-do-load-languages
	  'org-babel-load-languages
	  '((python . t)
		(emacs-lisp . t)
		(shell . t)
		(plantuml . t)
		(lua . t)
		(C . t)
		(sql . t)
	  ))

  (evil-define-key '(normal visual motion) org-mode-map
			  "gj" 'evil-next-visual-line
			  "gk" 'evil-previous-visual-line)

  :bind (:map global-map
			  ("M-o a"   . org-agenda)
			  ("C-c C-'" . org-edit-src-exit)
			  ("C-c C--" . org-edit-src-exit)
			  ("C-c x"   . org-capture)

			  ;; pomodoro timer
			  ("M-o P s"   . org-timer-set-timer)
			  ("M-o P S"   . org-timer-stop)
			  ("M-o P b"   . org-timer-start)
			  ("M-o P SPC" . org-timer-pause-or-continue)
			  )

		  (:map org-mode-map
				("C-<tab>" . org-cycle)
				("C-c c" . insert-header-counter)
				("C-c C" . insert-counter)
				("C-c t" . org-todo)
				("C-M-i" . org-toggle-inline-images)
				("C-S-C" . org-toggle-checkbox)
				("M-p" . org-move-subtree-up)
				("M-n" . org-move-subtree-down)
				("M-o s s" . org-schedule)
				("M-o s d" . org-deadline)
				("M-o e" . org-export-dispatch)
				("C-M-SPC"   . org-next-item)
				("C-M-S-SPC" . org-previous-item)
				("C-c RET" . org-insert-subheading)
				("M-o t" . org-babel-tangle)
				("C-c i d" . create-header-insert-date)
				("C-c i t" . create-header-insert-time)
				("C-c i T" . insert-time)
				("C-c i D" . insert-date)
				("C-c C-'" . org-edit-special)
				("C-M-h" . org-table-move-column-left)
				("C-M-l" . org-table-move-column-right)
				("C-c C--" . org-edit-latex-fragment)
				("C-c u k" . org-priority-up)
				("C-c u j" . org-priority-down)
				("C-}" . org-ctrl-c-minus)))

(use-package org-superstar
  :ensure t
  :hook (org-mode . org-superstar-mode)
  :config
  (setq org-superstar-headline-bullets-list '("◉" "○" "" "✿")))

(use-package org-roam
  :ensure t
  :after org
  :init
  (setq org-roam-v2-ack t)
  :config
  (setq org-roam-directory "~/Org/roam")
  :bind (:map org-mode-map
			  ("M-o r s" . org-roam-db-sync)
			  ("M-o r t" . org-roam-buffer-toggle)
			  ("M-o r i" . org-roam-node-insert)
			  ("M-o r f" . org-roam-node-find)))

(use-package org-contrib
  :ensure t
  :after org
  :init
  (require 'ox-extra)
  (require 'org-checklist)
  (require 'org-invoice)
  (ox-extras-activate '(ignore-headlines)))

(use-package org-drill
  :ensure t)

(with-conf-variable "emacsEnableOldVersionConfig" "no"
  (use-package org-gcal
	:ensure t
	:after org
	:straight (org-gcal
			   :host github
			   :type git
			   :repo "kidd/org-gcal.el")
	;; If you want to use the plugin, you need to configure these variables
	;; in src/local.el.
	;; Follow guide on line below in order to configure these variables
	;; https://github.com/kidd/org-gcal.el/blob/master/README.org.
	;; 
	;; + org-gcal-client-id
	;; + org-gcal-client-secret
	;; + org-gcal-file-alist
	:bind (:map org-mode-map
				("M-o S b" . org-gcal-sync-buffer)
				("M-o S a" . org-gcal-sync)
				("M-o S p" . org-gcal-post-at-point))))

(use-package org-wild-notifier
  :ensure t
  :config
  (org-wild-notifier-mode)
  (setq org-wild-notifier-alert-time '(1)
		org-wild-notifier--alert-severity 'low
		org-wild-notifier-notification-title "org-agenda"
		org-wild-notifier-notification-icon (format
											 "/home/%s/.config/emacs/mine/src/resources/icons/org-mode-unicorn.png"
											 user-login-name)
		alert-default-style 'libnotify
		alert-fade-time 40))

(with-conf-variable "emacsEnableOrgTrello" "yes"
  (use-package org-trello
	:ensure t
	:config
	(custom-set-variables
	 '(org-trello-files '(
						  "~/Org/trello/balandins-trello.org"
						  "~/Org/trello/test.org"
						  )))))

(use-package org-download
  :ensure t
  :config
  (setq-default org-download-image-dir "~/Org/images/downloaded/")
  :bind (:map org-mode-map
			  ("M-o d" . org-download-image)))

(use-package org-appear
  :ensure t
  :hook
  (org-mode . org-appear-mode))

(use-package org-inline-anim
  :ensure t
  :hook (org-mode . org-inline-anim-mode))

(use-package toc-org
  :ensure t
  :hook (org-mode . toc-org-mode))

;; (load "~/.config/emacs/mine/src/not-mine/org+align-table.el")

;; (add-hook 'org-mode-hook 'org+-align-table-mode)

(defun enable-visual-fill-column ()
  (interactive)
  (visual-fill-column-mode)
  (display-line-numbers-mode 0))

(defun disable-visual-fill-column ()
  (interactive)
  (visual-fill-column-mode 0)
  (display-line-numbers-mode t))

(defun visual-fill-column-enable-center ()
  (setq visual-fill-column-center-text t))

(add-hook 'org-mode-hook #'visual-fill-column-enable-center)

(use-package visual-fill-column
  :ensure t
  :straight (visual-fill-column
			 :host github
			 :repo "nobody926/visual-fill-column")
  :hook (visual-line-mode-hook . visual-fill-column-mode)
  :config
  (setq visual-fill-column-center-text t)
  (advice-add 'text-scale-adjust :after #'visual-fill-column-adjust)
  :bind (:map org-mode-map
    ("M-o i e" . enable-visual-fill-column)
    ("M-o i d" . disable-visual-fill-column)))

(defvar org-latex-previews-presentation-font-size 2.5
  "Org LaTeX preview font size for when (enable-presentation-mode) is running")

(defvar org-latex-previews-on-disable-font-size 1.5
  "Org LaTeX preview font size for when (enable-presentation-mode) stopped running")

(defun enable-presentation-mode ()
  (text-scale-increase 4)
  (org-latex-preview-scale-change org-latex-previews-presentation-font-size)

  (org-display-inline-images)
  (enable-visual-fill-column)
  (org-present)

  (when (y-or-n-p (format "Load theme %s?" (get-theme "presentation")))
	(disable-ewal "presentation"))

  (message "Press C-c C-x C-l to enable LaTeX fragments"))

(defun disable-presentation-mode ()
  (text-scale-set 0)
  (org-latex-preview 0)
  (org-latex-preview-scale-change org-latex-previews-on-disable-font-size)

  (org-remove-inline-images)
  (disable-visual-fill-column)
  (org-present-quit)

  (if-conf-variable "colorscheme" "pywal"
	(enable-ewal)
	(disable-ewal)))

(define-minor-mode org-presentation-mode
  "Toggle 'org-presentation-mode' on or off"
  :interactive t
  :init-value nil
  (if (derived-mode-p 'org-mode)
	  (if (eq org-presentation-mode nil)
		  (disable-presentation-mode)
		(enable-presentation-mode))
	(progn
	  (setq org-presentation-mode nil)
	  (message "You can use this minor-mode only in org-mode"))))

(defun org-presentation-export ()
  "Export presentation to pdf format. This one will have name
according to header 'PRESENTATION_EXPORT_NAME' in org-mode. This
function call script '~/.config/scripts/emacs/org-presentation-export'.
You can also configure this script by configuring settings.conf
variables
- '$emacsOPEcopyOutputPath'
- '$emacsOPEworkdir'"
  (interactive)
  (if (bound-and-true-p org-present-mode)
	(start-process-shell-command "async-shell-command" nil
								 (concat
								  "~/.config/scripts/emacs/org-presentation-export "
								  buffer-file-name " "
								  (org-get-keyword-variable-value
								   "PRESENTATION_EXPORT_NAME")))
	(message "You need to activate org-present-mode first")))

(defun org-presentation-export-images ()
  "Export presentation to images to '$emacsOPEworkdir'. This one will have name
according to header 'PRESENTATION_EXPORT_NAME' in org-mode. This
function call script '~/.config/scripts/emacs/org-presentation-export'.
You can also configure this script by configuring settings.conf
variables
- '$emacsOPEcopyOutputPath'
- '$emacsOPEworkdir'"
  (interactive)
  (if (bound-and-true-p org-present-mode)
	(start-process-shell-command "async-shell-command" nil
								 (concat
								  "~/.config/scripts/emacs/org-presentation-export "
								  buffer-file-name " "
								  (org-get-keyword-variable-value
								   "PRESENTATION_EXPORT_NAME") " "
								  "--photos-only"))
	(message "You need to activate org-present-mode first")))

(use-package org-present
  :ensure t
  :config
  (setq org-present-hide-stars-in-headings nil)
  :bind (:map org-mode-map
    ("M-o p p" . org-presentation-mode)
    ("M-o p e" . org-presentation-export)
    ("M-o p E" . org-presentation-export-images)
    ("C-S-T" . org-present)
    ("C->" . org-present-next)
    ("C-<" . org-present-prev)))

(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package rainbow-mode
  :ensure t
  :hook (org-mode prog-mode conf-mode))

(use-package doom-modeline
  :ensure t
  :init
  (doom-modeline-mode)
  :config
  (setq doom-modeline-height 36
        doom-modeline-modal-icon 'evil
        doom-modeline-evil-indicator t))

(use-package doom-themes
  :ensure t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
		;; use "doom-colors" for less minimal icon theme
        doom-themes-treemacs-theme "doom-atom"
		doom-themes-treemacs-enable-variable-pitch nil
        doom-themes-enable-italic nil) ; if nil, italics is universallyb disabled

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Enable custom neotree theme (all-the-icons must be installed!)
  (doom-themes-neotree-config)
  ;; or for treemacs users
  (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(with-conf-variable "emacsEnableOldVersionConfig" "no"
  (use-package doom-everblush-theme
	:ensure t
	:straight (doom-everblush-theme
			   :type git
			   :host github
			   :repo "Everblush/doomemacs")))

(use-package highlight-indent-guides
  :ensure t
  :hook (prog-mode yaml-mode)
  :config
  (setq highlight-indent-guides-method 'character))

(defvar transparency-level 93
  "Transparency level for emacs. It is used in functions that set
transparency")

(defun get-theme (&optional choice)
  "If CHOICE is not nil, it will choose theme depending on this variable.
Here are values of this parameter
'presentation' --> doom-nord-light
'reader' --> doom-gruvbox-light

If CHOICE is nil, it will pick return theme depending on $colorscheme
from settings.conf. You can find available colorschemes in
~/.files-balamah/src/colorschemes
"
  (if choice
	  (progn (pcase choice
			   ("presentation" 'doom-nord-light)
			   ("reader" 'doom-gruvbox-light)))
	(progn (pcase (read-conf-variable "~/.files-balamah/settings.conf"
									  "colorscheme")
			 ((or "onedark-gray" "onedark-cyan") 'doom-one)
			 ("gruvbox"                          'doom-gruvbox)
			 ("dracula"                          'doom-dracula)
			 ("everblush"                        'doom-everblush)
			 ("nord"                             'doom-nord)
			 ("pywal"                            'ewal-doom-one)))
	)
  )

(defun disable-themes ()
  "Disable all loaded themes"
  (dolist (theme custom-enabled-themes)
	(disable-theme theme)))

(defun disable-ewal (&optional is-not-coding)
  "Check documentation for 'get-theme' to get lawful values of
parameter 'is-not-coding'"

  (disable-themes)
  (if-conf-variable "transparencyWithoutPywal" "no"
	(change-transparency 100)
	(change-transparency transparency-level))
  (if is-not-coding
      (load-theme (get-theme is-not-coding) t)
	(load-theme (get-theme) t)))

(defun disable-ewal-for-toggle ()
  "It disables ewal for 'ewal-mode', for toggling.
This function depends on '$emacsAlternateTheme'"
  (let* ((theme (read-conf-variable
				 "~/.files-balamah/settings.conf" "emacsAlternateTheme"))
		 (theme-to-load (if (equal theme "current")
							(get-theme)
						  (intern theme))))
	(load-theme theme-to-load t))
  (if-conf-variable "transparencyWithoutPywal" "no"
	(change-transparency 100)
	(change-transparency transparency-level)))

(defun enable-ewal ()
  (disable-themes)
  (with-conf-variable "colorscheme" "pywal"
	(change-transparency transparency-level)
	(load-theme 'ewal-doom-one t))

  (unless-conf-variable "colorscheme" "pywal"
	(load-theme (get-theme) t)))

;; FUCK: I couldn't convert this to minor-mode for it causes recursion.
;; Don't know why

(defvar ewal-mode nil)

(defun ewal-mode ()
  (interactive)
  (if (eq ewal-mode nil)
      (disable-ewal-for-toggle)
	(enable-ewal))

  (setq ewal-mode (not ewal-mode)))

(defun ewal-mode-presentation ()
  (interactive)
  (if (eq ewal-mode nil)
      (disable-ewal "presentation")
	(enable-ewal))

  (setq ewal-mode (not ewal-mode)))

(use-package ewal-doom-themes
  :ensure t
  :if (equal system-type 'gnu/linux)
  :bind
  ("C-c w" . ewal-mode)
  ("C-c W" . ewal-mode-presentation)
  :config
  (setq ewal-built-in-palette "sexy-material"))

(with-eval-after-load 'doom-themes
  (load-theme (get-theme) t))

(defun dashboard-dired-open-dotfiles (&rest _)
  (dired "~/.files-balamah"))

(defun dashboard-open-dotfiles-readme (&rest _)
  (find-file "~/.files-balamah/README.org"))

(defun dashboard-edit-emacs-config (&rest _)
  (find-file "~/.files-balamah/config/emacs/mine/editor-config.org"))

(defun dashboard-package-upgrade-all (&rest _)
  (when (y-or-n-p "Are you sure you want to upgrade plugins?")
	(package-refresh-contents)
	(package-upgrade-all)))

(defun dashboard-restart-emacs (&rest _)
  (restart-emacs-ask))

(defun dashboard-scratch-buffer (&rest _)
  (scratch-buffer))

(defun dashboard-open-org-agenda (&rest _)
  (org-agenda))

(defun dashboard-counsel-bookmark (&rest _)
  (call-interactively 'counsel-bookmark))

(defun dashboard-projectile-switch-project (&rest _)
  (projectile-find-file))

(defun dashboard-councel-load-theme (&rest _)
  (counsel-load-theme))

(defun dashboard-ewal-mode (&rest _)
  (ewal-mode))

(defun emacsclient-dashboard-start ()
  "Start dashboard on start of 'emacsclient -c'"
  (dashboard-refresh-buffer)
  (get-buffer "*dashboard*"))

(use-package dashboard
  :ensure t
  :init
  (setq dashboard-banner-logo-title "Welcome to GNU Emacs"
        dashboard-show-shortcuts t
        dashboard-display-icons-p t
        dashboard-icon-type 'nerd-icons
        dashboard-set-heading-icons t
        dashboard-set-file-icons t
        dashboard-startupify-list '(dashboard-insert-banner
                                    dashboard-insert-newline ;; newline
                                    dashboard-insert-banner-title
                                    dashboard-insert-newline ;; newline
                                    dashboard-insert-navigator
                                    dashboard-insert-newline ;; newline
                                    dashboard-insert-init-info
                                    dashboard-insert-items
									dashboard-insert-footer)
        dashboard-center-content t
        dashboard-startup-banner "~/.config/emacs/mine/src/resources/icons/10.txt"
        dashboard-items-face t)
  ;; here are 3 colors:
  ;; normal, warning, error
  (with-conf-variable "emacsDashboardOption" "buttons"
	(setq dashboard-navigator-buttons
		  `(;; dotfiles
			(("" "Dotfiles"
			  "Open ~/.files-balamah/ directory\ndashboard-dired-open-dotfiles"
			  dashboard-dired-open-dotfiles)
			 ("" "Dotfiles guide"
			  "Open ~/.files-balamah/README.org\ndashboard-open-dotfiles-readme"
			  dashboard-open-dotfiles-readme)
			 )
			(;; dashboard sections substitution
			 ("" "Open project files"
			  "Open files from projects\ndashboard-projectile-switch-project"
			  dashboard-projectile-switch-project)
			 ("" "Open scratch buffer"
			  "Open scratch buffer to write something\ndashboard-scratch-buffer"
			  dashboard-scratch-buffer)
			 ("" "Jump to bookmark"
			  "Open file at location quickly\ncounsel-bookmark"
			  dashboard-counsel-bookmark)
			 )
			(;; org-mode stuff
			 ("" "Org agenda"
			  "Open org-agenda using M-o a\ndashboard-open-org-agenda"
			  dashboard-open-org-agenda)
			 ("" "Emacs config"
			  "Open emacs org config\ndashboard-edit-emacs-config"
			  dashboard-edit-emacs-config)
			 )
			(;; emacs actions
			 ("" "Upgrade plugins"
			  "Upgrade emacs packages to new versions\ndashboard-package-upgrade-all"
			  dashboard-package-upgrade-all error)
			 ("" "Restart emacs"
			  "Restart emacs\ndashboard-restart-emacs"
			  dashboard-restart-emacs error)
			 ("" "Change theme"
			  "Change emacs ui theme\ndashboard-councel-load-theme"
			  dashboard-councel-load-theme error)
			 ("" "Toggle theme"
			  "Toggle between pywal and onedark\ndashboard-ewal-mode"
			  dashboard-ewal-mode error)))
        dashboard-items '((recents  . 10)
						  )))
  (with-conf-variable "emacsDashboardOption" "elements"
	(setq dashboard-items '((recents  . 5)
							(bookmarks . 5)
							(agenda . 6))))
  :config
  (dashboard-setup-startup-hook)
  (setq initial-buffer-choice 'emacsclient-dashboard-start)
  (global-set-key (kbd "C-c b") 'dashboard-open)

  (dolist (message '("Christ is the LORD" 
                   "Ο Ιησούς Χριστός είναι Κυρίως"
                   "Focus on the task"))
	(add-to-list 'dashboard-footer-messages message)))

(with-conf-variable "emacsDashboardOption" "doom"
  (use-package doom-dashboard
	:straight (doom-dashboard-fixed
			   :host github
			   :repo "nobody926/doom-dashboard-fixed")
	:after dashboard
	:demand t
	:bind (:map dashboard-mode-map
				("<remap> <dashboard-previous-line>" . widget-backward)
				("<remap> <dashboard-next-line>" . widget-forward)
				("<remap> <previous-line>" . widget-backward)
				("<remap> <next-line>"  . widget-forward)
				("<remap> <right-char>" . widget-forward)
				("<remap> <left-char>"  . widget-backward))
	:custom
	(dashboard-banner-logo-title "Balamah's Emacs")
	(dashboard-startup-banner "~/.config/emacs/mine/src/resources/icons/10.txt")
	(dashboard-footer-icon
	 (nerd-icons-faicon "nf-fa-github_alt" :face 'success :height 1.5))
	(dashboard-page-separator "\n")
	(dashboard-startupify-list `(dashboard-insert-banner
								 dashboard-insert-banner-title
								 dashboard-insert-items
								 ,(dashboard-insert-newline 2)
								 dashboard-insert-init-info
								 ,(dashboard-insert-newline 2)
								 doom-dashboard-insert-homepage-footer))
	(dashboard-item-generators
	 '((recents   . doom-dashboard-insert-recents-shortmenu)
	   (bookmarks . doom-dashboard-insert-bookmark-shortmenu)
	   (projects  . doom-dashboard-insert-project-shortmenu)
	   (agenda    . doom-dashboard-insert-org-agenda-shortmenu)))
	(dashboard-items '(projects agenda bookmarks recents))))

(use-package keycast
  :ensure t)

(defun custom-monkeytype-mode-hook ()
  "Hooks for monkeytype-mode"
  (evil-insert -1))

(use-package monkeytype
  :ensure t
  :config (add-hook 'monkeytype-mode-hook #'custom-monkeytype-mode-hook)
  :bind (:map org-mode-map
			  ("C-c M" . monkeytype-region)))

(use-package evil-string-inflection
  :ensure t
  :bind (:map global-map
			  ("C-{" . string-inflection-all-cycle)))

(use-package emacs-everywhere
  :ensure t
  :config
  (setq emacs-everywhere-major-mode-function 'org-mode))

(defun add-project (dir)
  (interactive (list (read-directory-name "Add to known projects: ")))
  (projectile-add-known-project dir)
  (treemacs-add-project dir))

(use-package projectile
  :ensure t
  :config
  (projectile-mode)
  (setq projectile-resolve-symlinks nil)
  :bind (:map global-map
			  ("C-c C-p C-a" . add-project)
			  ("C-c C-p C-r" . projectile-remove-known-project)
			  ("C-c C-p C-s" . projectile-switch-project)
			  ("<leader>Ps" . projectile-switch-project)
			  ("<leader>Pf" . projectile-find-file)
			  ("<leader>Pg" . projectile-grep)
			  ))

(require 'cl-lib)

(defun file-truename-with-symlink-preserved (orig-fn &rest args)
  "Prevent resolving symlinks in file paths under certain directories."
  (let* ((file (car args))
         (directories-original (read-conf-variable
                                (expand-file-name "~/.files-balamah/settings.conf")
                                "emacsFileSymlinkPreserveDirectories"))
         (directories (split-string directories-original ",")))
    (if (cl-some (lambda (directory)
                   (string-prefix-p (expand-file-name (string-trim directory))
                                    (expand-file-name file)))
                 directories)
        (expand-file-name file)
      (apply orig-fn args))))

(with-conf-variable "emacsFileSymlinkPreserve" "yes"
  (advice-add 'file-truename :around #'file-truename-with-symlink-preserved))

(defun around-projectile-without-resolving-symlinks (orig-fn &rest args)
  "Temporarily disable symlink resolution inside Projectile."
  (cl-letf (((symbol-function 'file-truename)
             (lambda (f) (expand-file-name f))))
    (apply orig-fn args)))

(with-conf-variable "emacsFileGlobalSymlinkPreserve" "yes"
  (advice-add 'projectile-project-root
			:around #'around-projectile-without-resolving-symlinks))

(use-package treemacs
  :ensure t
  :defer t
  :bind
  (:map global-map
			  ("M-0" . treemacs))
  (:map treemacs-mode-map
		("C-<tab>" . treemacs-TAB-action))
  :config
  (setq treemacs-no-png-images t))

(use-package treemacs-evil
  :after (treemacs evil)
  :ensure t)

(use-package treemacs-nerd-icons
  :after (treemacs evil)
  :ensure t
  :config (treemacs-load-theme "nerd-icons"))

(use-package treemacs-projectile
  :after (treemacs projectile)
  :ensure t)

(use-package transpose-frame
  :ensure t
  :bind ((:map global-map
			   ("C-$" . flop-frame)
			   ("C-%" . transpose-frame)
			   ("<leader>ff" . flop-frame)
			   ("<leader>ft" . transpose-frame)
			   )))

(use-package dired-open-with
  :ensure t
  :after dired
  :bind (:map dired-mode-map
			  ("C-S-<return>" . dired-open-with)))

(use-package dired
  :ensure nil
  :commands (dired dired-jump)
  :config
  (require 'dired-x)
  (setq dired-listing-switches "-lhaF --group-directories-first"
		dired-kill-when-opening-new-dired-buffer t
		dired-dwim-target t
		dired-guess-shell-alist-user '(("\\.gif\\'"  . "sxiv")
                                       ("\\.jpg\\'"  . "sxiv")
                                       ("\\.png\\'"  . "sxiv")
                                       ("\\.webp\\'" . "sxiv")
                                       ("\\.mkv\\'"  . "mpv")
                                       ("\\.mp4\\'"  . "mpv")))
  (evil-collection-define-key 'normal 'dired-mode-map
	"h" 'dired-up-directory
	"l" 'dired-find-file
	")" 'dired-create-empty-file
	"%p" 'set-file-modes))

(with-conf-variable "emacsEnableOldVersionConfig" "yes"
  (use-package which-key
    :ensure t
    :straight (which-key
			   :host github
			   :repo "justbur/emacs-which-key")
	:init
	(require 'which-key)
	(which-key-mode)))

(use-package pyvenv
  :ensure t
  :init
  (pyvenv-mode t)
  :config
  (setq pyvenv-post-activate-hooks
        (list (lambda ()
                (setq python-shell-interpreter
                      (concat pyvenv-virtual-env "/bin/python3")))))
  (setq pyvenv-post-deactivate-hooks
        (list (lambda ()
                (setq python-shell-interpreter "python3"))))

  :bind (:map global-map
          ("C-c p a" . pyvenv-activate)
          ("C-c p d" . pyvenv-deactivate)
         :map evil-normal-state-map
          ("<leader>pa" . pyvenv-activate)
          ("<leader>pd" . pyvenv-deactivate))
          )

(use-package drag-stuff
  :ensure t
  :init
  (drag-stuff-global-mode 1)
  :bind
  (:map global-map
		("M-H" . drag-stuff-left)
		("M-L" . drag-stuff-right)
		("M-J" . drag-stuff-down)
		("M-K" . drag-stuff-up)))

(use-package dotenv-mode :ensure t)
(use-package yaml-mode :ensure t)
(use-package dockerfile-mode :ensure t)
(use-package logview :ensure t)
(use-package git-modes :ensure t)
(use-package lua-mode :ensure t)
(use-package crontab-mode :ensure t)
(use-package twig-mode :ensure t)
(use-package sxhkdrc-mode :ensure t)
(use-package plantuml-mode :ensure t)
(use-package groovy-mode :ensure t)
(use-package ebuild-mode :ensure t
  :straight (ebuild-mode :host github :repo "emacsmirror/ebuild-mode"))

(use-package gradle-mode
  :ensure t
  :hook (java-mode . gradle-mode))

(use-package holyc-mode
  :ensure t
  :straight (holyc-mode
			 :host github
			 :repo "Naheel-Azawy/holyc-mode.el"))

(use-package hl-todo
  :ensure t
  :hook ((prog-mode . hl-todo-mode)
		 (yaml-mode . hl-todo-mode) )
  :config
  (setq hl-todo-highlight-punctuation ":"
        hl-todo-keyword-faces
        '(("TODO" warning bold)
          ("FIXME" error bold)
          ("REVIEW" font-lock-keyword-face bold)
          ("HACK" font-lock-constant-face bold)
          ("DEPRECATED" font-lock-doc-face bold)
          ("NOTE" success bold)
          ("BUG" error bold)
          ("FUCK" error bold)
          ("XXX" font-lock-constant-face bold))))

(use-package doom-todo-ivy
  :ensure t
  :straight (doom-todo-ivy
			 :host github
			 :repo "jsmestad/doom-todo-ivy")
  :init
  (require 'doom-todo-ivy)
  :bind (:map global-map
			  ("C-c C-x t" . doom/ivy-tasks)))

(use-package yasnippet
  :ensure t
  :init
  (yas-global-mode 1)
  :config
  (require 'warnings)
  (setq yas-snippet-dirs '("~/.config/emacs/mine/snippets")
        yas-indent-line 'fixed)
  (add-to-list 'warning-suppress-types '(yasnippet backquote-change)))

(use-package magit
  :ensure t
  :bind ("C-x g" . magit-status))

(use-package docker
  :ensure t
  :bind ("C-c 1 d" . docker))

(use-package lsp-mode
  :ensure t
  :init
  (setq lsp-keymap-prefix "C-c l")
  :config
  (lsp-enable-which-key-integration t)
  (setq lsp-headerline-breadcrumb-enable nil
		lsp-enable-file-watchers nil)
  :bind (:map lsp-mode-map
			  ("C-<return>" . lsp-find-definition)
			  ("C-S-<return>" . flycheck-list-errors)))

(use-package lsp-ui
  :ensure t)

(use-package lsp-treemacs
  :ensure t
  :disabled t)

(use-package python-mode
  :ensure t
  :mode "\\.py\\'"
  :after lsp-mode
  :hook (python-mode . lsp-deferred)
  :init
  (add-hook 'python-mode-hook 'flycheck-mode)
  (setq lsp-pylsp-plugins-flake8-ignore '((
										   "D103" "D100" "E402" "E302" "E501" "C0116"
										   "C0103"))
        lsp-pylsp-server-command "~/.local/bin/pylsp"
        lsp-pylsp-plugins-flake8-max-line-length 87))

(use-package php-mode
  :ensure t
  :mode "\\.php\\'"
  :after lsp-mode
  :hook (php-mode . lsp-deferred))

(use-package lua-mode
  :ensure nil
  :hook (lua-mode . lsp-mode))

(use-package lsp-java
  :ensure t
  :hook (java-mode . lsp-deferred)
  :config
  (setq lsp-java-download-sources t
        lsp-java-content-provider-preferred "fernflower")
  :bind (:map java-mode-map
			  ("M-C-0" . lsp-treemacs-java-deps-list)))

(setq c-basic-offset 4)

(use-package c-mode
  :ensure nil
  :hook (c-mode . lsp-mode))

(use-package c++-mode
  :ensure nil
  :hook (c++-mode . lsp-mode))

(use-package company
  :ensure t
  :straight (company
			 :host github
			 :repo "nobody926/company-mode")
  :init
  (global-company-mode t)
  :config
  ;; (add-to-list 'company-backends 'company-yasnippet)
  (setq company-idle-delay 0
        company-minimum-prefix-length 2
        company-show-numbers t
        company-tooltip-limit 10
        company-tooltip-align-annotations t
		company-replace-existing nil
        company-tooltip-flip-when-above t))

(defun yas-disable-company ()
  "Disable company-mode while a yasnippet snippet is active"
  (with-conf-variable "emacsEnableCompanySnippetsLaTeX" "no"
	(when (or (equal major-mode 'latex-mode) (equal major-mode 'LaTeX-mode))
	  (company-mode -1))))

(defun yas-enable-company ()
  "Re-enable company-mode after yasnippet snippet exits"
  (with-conf-variable "emacsEnableCompanySnippetsLaTeX" "no"
	(when (or (equal major-mode 'latex-mode) (equal major-mode 'LaTeX-mode))
	  (company-mode 1))))

(add-hook 'yas-before-expand-snippet-hook #'yas-disable-company)
(add-hook 'yas-after-exit-snippet-hook #'yas-enable-company)

(defun hook/disable-company-in-org ()
  (company-mode -1))

;; (add-hook 'org-mode-hook #'hook/disable-company-in-org)

(use-package ac-php
  :ensure t
  :init
  (auto-complete-mode t)
  :config
  (require 'company-php)
  (ac-php-core-eldoc-setup)
  (setq ac-sources '(ac-source-php))
  :bind (:map global-map
              ("M-]" . ac-php-find-symbol-at-point)
              ("M-[" . ac-php-location-stack-back)))

(use-package anaconda-mode
  :ensure t
  :hook (python-mode-hook . anaconda-mode))

(use-package company-anaconda
  :ensure t
  :init (require 'rx)
  :after company
  :config
  (add-to-list 'company-backends 'company-anaconda))

(use-package company-quickhelp
  :ensure t
  :config
  (company-quickhelp-mode))

(use-package haskell-mode
  :ensure t)

(use-package company-ghci
  :ensure t
  :hook (haskell-mode-hook . company-mode))

(use-package company-web
  :ensure t
  :init
  (require 'company)
  (require 'company-web-html)
  (require 'company-css))

(use-package company-auctex
  :ensure t
  :init
  (company-auctex-init))

(use-package dap-mode
  :ensure t
  :config
  (require 'dap-php)
  (dap-php-setup)
  :bind (:map global-map
			  ("C-M-<return>" . dap-breakpoint-toggle)))

(defun open-vterm-below-project-root ()
  (interactive)
  (split-below-switch-window 1)
  (projectile-run-vterm))

(defun open-vterm-right-project-root ()
  (interactive)
  (split-right-switch-window 1)
  (projectile-run-vterm))

(defun open-vterm-below ()
  (interactive)
  (split-below-switch-window 1)
  (vterm))

(defun open-vterm-right ()
  (interactive)
  (split-right-switch-window 1)
  (vterm))

(defun vterm-run-kill (process event)
  (let ((b (process-buffer process)))
    (and (buffer-live-p b)
         (kill-buffer b))))

(defun vterm-run (command &optional buffer-name)
  (interactive
   (list
    (let* ((f (cond (buffer-file-name)))
           (filename
            (concat " " (shell-quote-argument (and f (file-relative-name f))))))
      (read-shell-command "Terminal command: "))))
  (let* ((name (if (not buffer-name)
				   (concat "*" command "*")
				 buffer-name)))
	(with-current-buffer (vterm name)
	  (set-process-sentinel vterm--process #'vterm-run-kill)
	  (vterm-send-string command)
	  (vterm-send-return))))

(defun vterm-default-keybindings ()
  (setq evil-default-state 'emacs))

(defun run-prepare ()
  (when buffer-file-name (save-buffer))
  (split-below-switch-window))

(defun run (binary-name &optional is-only-file-name)
  "Run current file using BINARY-NAME in vterm"
  (run-prepare)
  (let* ((filename (if is-only-file-name
					   (get-only-file-name buffer-file-name)
					 buffer-file-name))
		 (formatted-filename (replace-regexp-in-string " " "\\\\ " filename)))
	(vterm-run (format "%s %s" binary-name formatted-filename))))

(defmacro defrun (function-name binary-name &optional is-only-file-name)
  "Create interactive function with (run).
WARNING: this macro creates function with prefix 'run-',
for example:
java --> run-java
php  --> run-php
"
  (declare (indent defun))
  `(defun ,(intern (concat "run-" (symbol-name function-name))) ()
     (interactive)
     (run ,binary-name ,is-only-file-name)))

(defmacro defrunc (function-name &rest body)
  "Create interactive function with (run-prepare).
WARNING: this macro creates function with prefix 'run-',
for example:
java --> run-java
php  --> run-php
"
  (declare (indent defun))
  `(defun ,(intern (concat "run-" (symbol-name function-name))) ()
     (interactive)
	 (run-prepare)
	 ,@body))

(defrun php "php")
(defrunc php-web (eww (concat "localhost/" (get-only-file-name buffer-file-name))))
(defrunc localhost (eww "localhost"))
(defrun python "python3")
(defrun bookmark "br")
(defrun java "java")

(defrun php-container
  "~/.config/scripts/emacs/run-php-file-container"
  (get-only-file-name buffer-file-name))

(defrun shell-script
  (replace-regexp-in-string "\^J" ""
							(shell-command-to-string
							 (format "cat %s | grep '^#!' | sed 's@.*/@@g'"
									 buffer-file-name))))

(defvar LaTeX-create-output-directory t
  "The variable determines whether to create 'LaTeX-output-directory' or not")

(defvar LaTeX-compiler "pdflatex"
  "Compiler for LaTeX compilation")

(defvar LaTeX-output-directory "./.output"
  "Directory for LaTeX compilation")

(defvar LaTeX-cflags (format "-output-directory %s" LaTeX-output-directory)
  "LaTeX compilation flags")

(defun LaTeX-compile ()
  (interactive)
  (call-process-shell-command (concat LaTeX-compiler " "
									  LaTeX-cflags " " buffer-file-name)))

(defun LaTeX-open-log-file (filename)
  "Open log file if exists and if prompt is yes"
  (if (file-exists-p filename)
	  (when (y-or-n-p "Open log file to see errors?")
		(find-file filename))
	(message "Errors appeared, and file didn't appear")))

(defun LaTeX-compile-open ()
  "Compile LaTeX document and open the file in zathura, if exists.
Otherwise ask for input to open log file to see errors"
  (interactive)
  (unless (file-directory-p LaTeX-output-directory)
	(make-directory LaTeX-output-directory))
  (let* ((filename-base (file-name-base buffer-file-name))
		 (filename (format "%s/%s.pdf" LaTeX-output-directory filename-base))
		 (log-file (format "%s/%s.log" LaTeX-output-directory filename-base))
		 (exit-code (LaTeX-compile))
		 )
	(if (equal exit-code 0)
		(progn
		  (start-process-shell-command "zathura" nil (format "zathura %s" filename))
		  (message "%s successfully compiled" (get-only-file-name buffer-file-name))
		  )
	  (LaTeX-open-log-file log-file))))

(require 'python)
(require 'php-mode)
(require 'latex)

(global-set-key (kbd "S-<f9>") 'run-shell-script)

(map-bind '(("S-<f9>" . run-python)) python-mode-map)

(map-bind '(("S-<f9>"   . run-php)
			("S-M-<f9>" . run-php-container)
			("S-<f10>"  . run-php-web)) php-mode-map)

(map-bind '(("S-<f9>" . run-java)) java-mode-map)

(with-eval-after-load 'bookmark
  (map-bind '(("S-<f9>" . run-bookmark)) bookmark-mode-map))

(map-bind '(("S-<f9>" . LaTeX-compile-open)
			("S-M-<f9>" . LaTeX-compile)) LaTeX-mode-map)

(global-set-key (kbd "S-<f12>") 'run-localhost)

(use-package vterm
  :after evil
  :ensure t
  :config
  :bind (:map global-map
    ("C-M-`" . open-vterm-below)
    ("C-M-~" . open-vterm-right)
    ("C-`" . open-vterm-below-project-root)
    ("C-~" . open-vterm-right-project-root))
  (:map vterm-mode-map
		("M-0" . treemacs)))

(use-package nyan-mode
  :ensure t
  :if (and (equal (nth 4 (decode-time (current-time))) 4)
           (equal (nth 3 (decode-time (current-time))) 1))
  :init
  (nyan-mode)
  (nyan-start-animation))

(use-package sudo-edit
  :ensure t)

(use-package workgroups2
  :ensure t
  :config
  (setq wg-session-file "~/.config/emacs/mine/.emacs-workgroups")
  (workgroups-mode 1))

(use-package ligature
  :ensure t
  :config
  (ligature-set-ligatures '(org-mode
                            prog-mode
                            haskell-mode
                            text-mode
                            markdown-mode
                            lsp
                            cc-mode
                            vterm-mode
							major-mode)

                          '("|||>" "<|||" "<==>" "<!--" "####" "~~>"
                            "***" "||=" "||>" ":::" "::=" "=:=" "==="
                            "==>" "=!=" "=>>" "=<<" "=/=" "!==" "!!."
                            ">=>" ">>=" ">>>" ">>-" ">->" "->>" "-->"
                            "--" "-<<" "<~~" "<~>" "<*>" "<||" "<|>"
                            "<$>" "<==" "<=>" "<=<" "<->" "<--" "<-<"
                            "<<=" "<<-" "<<<" "<+>" "</>" "###" "#_("
                            "..<" "..." "+++" "/==" "///" "_|_" "www"
                            "&&" "^=" "~~" "~@" "~=" "~>" "~-" "**"
                            "*>" "*/" "||" "|}" "|]" "|=" "|>" "|-"
                            "{|" "[|" "]#" "::" ":=" ":>" ":<" "$>"
                            "==" "=>" "!=" "!!" ">:" ">=" ">>" ">-"
                            "-~" "-|" "->" "--" "-<" "<~" "<*" "<|"
                            "<:" "<$" "<=" "<>" "<-" "<<" "<+" "</"
                            "#{" "#[" "#:" "#=" "#!"  "##" "#(" "#?"
                            "#_" "%%" ".=" ".-" ".." ".?" "+>" "++"
                            "?:" "?=" "?." "??" ";;" "/*" "/=" "/>"
                            "//" "__" "~~" "(*" "*)"
                             "\\\\" "://"))

  (global-ligature-mode t))
