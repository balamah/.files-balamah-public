(defvar git-sync-directories '("~/Org/")
  "directories for git-sync")

(defvar git-sync-keybinding-prefix "C-c S"
  "Prefix for git-sync keybindings")

(require 'vc-git)

(defun get-current-git-branch ()
  (vc-git--symbolic-ref (vc-git-root default-directory)))

(defun git-add (repo-directory)
  (message " adding files to commit...")
  (cd repo-directory)
  (shell-command "git add ."))

(defun git-commit ()
  (message " creating commit...")
  (let ((date (format-time-string "%d.%m.%Y"))
		(time (format-time-string "%H:%M")))
	(shell-command (format "git commit -m '%s %s'" date time))))

(defun git-push (branch repo-directory)
  (message (format " pushing %s..." repo-directory))
  (start-process-shell-command "git-sync-push" nil
							   (format "git push -u origin %s" branch)))

(defun git-pull ()
  (interactive)
  (message (format " pulling %s..." (vc-root-dir)))
  (start-process-shell-command "git-sync-pull" nil "git pull")
  (magit-refresh-all))

(defun git-sync (repo-directory branch)
  (git-add repo-directory)
  (git-commit)
  (git-push branch repo-directory)

  (message " done syncing %s" repo-directory))

(defun git-pull-all-list ()
  (interactive)

  (let ((previous-directory default-directory))
	(dolist (directory git-sync-directories)
	  (when (file-directory-p directory)
		(cd directory)
		(git-pull)))
	(cd previous-directory)))

(defun git-sync-current-directory ()
  (interactive)
  (if (get-current-git-branch)
	  (progn
		(when buffer-file-name (save-buffer))
		(when (member (vc-root-dir) git-sync-directories)
		  (git-sync (vc-root-dir) (get-current-git-branch))))
	(message "This is not a git repository")))

(defun git-sync-all-list ()
  (interactive)

  (when buffer-file-name (save-buffer))
  (let ((previous-directory default-directory))
	(dolist (directory git-sync-directories)
	  (when (file-directory-p directory)
		(message " syncing %s..." directory)
		(cd directory)
		(git-sync directory (get-current-git-branch))
		(message "------------------------------")))

	(cd previous-directory))

  (message " done syncing git-sync-directories"))

(define-minor-mode git-sync-mode
  "Toggle 'git-sync' functional on or off"
  :global t
  :interactive t
  :init-value t
  :keymap
  (list (cons (pkbd git-sync-keybinding-prefix "a") #'git-sync-all-list)
		(cons (pkbd git-sync-keybinding-prefix "c") #'git-sync-current-directory)
		(cons (pkbd git-sync-keybinding-prefix "P c") #'git-pull)
		(cons (pkbd git-sync-keybinding-prefix "P a") #'git-pull-all-list)))

(defun project-get-root ()
  (let* ((to-print (if (boundp 'current-project)
					   current-project
					 (projectile-project-root))))
	(file-name-as-directory to-print)))

(defun project-get-root ()
  (let* ((to-print (if (bound-and-true-p current-project)
                       current-project
                     (projectile-project-root))))
    (if to-print
        (file-name-as-directory to-print)
      default-directory)))

(defun project-config-get-directory ()
  (concat (project-get-root) ".emacs/"))

(defun project-config-file-get ()
  (concat (project-config-get-directory) "config.el"))

(defun project-config-file-directory-get-path (file-directory)
  "Returns path of file-directory.
Here is example how to use
(project-config-file-directory-get-path \"scripts/launcher\")"
  (concat (project-config-get-directory) file-directory))

(defvar project-config-file-no-file-message
  "Project doesn't have .emacs/config.el, create it in project root"
  "project-config-file-open and project-config-file-load show the message when
project doesn't have .emacs/config.el")

(defvar project-config-file-keybinding-prefix "C-c p f"
  "Prefix for project-config-file-* keybindings")

(defvar project-config-directory-template
  "~/.config/emacs/mine/src/resources/.emacs"
  "Template directory for project config. The template should have that structure
.emacs
├── config.el
└── snippets
    ├── conf-space-mode
    │   ├── bookmark-all
    │   ├── bookmark-base
    │   └── bookmark-include
    └── php-mode
        ├── array-in
        ├── array-not-in
        └── array-push

snippets/ directory is like your yasnippet directory, therefore it
should have structure with modes like in that example that i have shown
")

(defun project-config-file-create ()
  "Copy directory from project-config-directory-template to project root"
  (interactive)
  (copy-directory project-config-directory-template (project-get-root))
  (message "%s has been created" (project-config-file-get)))

(defun project-config-file-open ()
  "Open project-config-file which gets from project-config-file-get and the file
is located in (project-config-get-directory)/config.el"
  (interactive)
  (let ((project-config-file (project-config-file-get)))
	(if (file-exists-p project-config-file)
		(find-file project-config-file)
	  (message project-config-file-no-file-message))))

(defun project-config-directory-delete (project-config-directory)
  "Delete project config"
  (when (y-or-n-p "Are you sure you want to delete project config?")
	(delete-directory project-config-directory "recursive")))

(defun project-config-directory-find-delete ()
  "Find project config directory and delete"
  (interactive)
  (let ((project-config-directory (project-config-get-directory)))
	(if (file-directory-p project-config-directory)
		(project-config-directory-delete project-config-directory)
	  (message project-config-file-no-file-message))))

(require 'projectile)

(defun project-config-file-load (project-config-file)
  "Load project config file"
  (message "Loading project config from %s" project-config-file)
  (load-file project-config-file))

(defun project-config-file-find-load ()
  "Find project config file and load"
  (interactive)
  
  (let ((project-config-file (project-config-file-get)))
	(if (file-exists-p project-config-file)
		(project-config-file-load project-config-file)
	  (message project-config-file-no-file-message))))

(define-minor-mode project-config-mode
  "Toggle 'project-config-mode' functional on or off"
  :global t
  :interactive t
  :init-value t
  :keymap
  (list
   (cons (pkbd project-config-file-keybinding-prefix "c") #'project-config-file-create)
   (cons (pkbd project-config-file-keybinding-prefix "o") #'project-config-file-open)
   (cons (pkbd project-config-file-keybinding-prefix "d") #'project-config-directory-find-delete)
   (cons (pkbd project-config-file-keybinding-prefix "l") #'project-config-file-find-load)
   ))

(defun enable-reader-mode ()
  (when (y-or-n-p "Enable gruvbox theme?")
	(disable-ewal "reader"))

  (setq truncate-lines nil)
  (display-line-numbers-mode 0) 
  (hl-line-mode t)

  (message "reader-mode enabled"))

(defun disable-reader-mode ()
  (message "reader-mode disabled")

  (if-conf-variable "colorscheme" "pywal"
	(enable-ewal)
	(disable-ewal))

  (setq truncate-lines t)
  (display-line-numbers-mode t) 
  (hl-line-mode 0))

(define-minor-mode reader-mode
  "Toggle 'reader-mode' functional on or off"
  :interactive t
  :init-value nil
  (if (derived-mode-p 'org-mode)
	 (if reader-mode
		 (enable-reader-mode)
	   (disable-reader-mode))
	(progn
	  (setq reader-mode nil)
	  (message "You can use this minor-mode only in org-mode"))))

(define-key org-mode-map (kbd "C-c r") 'reader-mode)

(defvar open-bookmark-file "~/Documents/bookmarks/emacs.dmenu-ignore"
  "Bookmark file to open bookmark")

(defun read-bookmark-file ()
  "Run 'br' on bookmark file"
  (let* ((file open-bookmark-file)
		 (command (concat "br " open-bookmark-file)))
	(if (file-exists-p open-bookmark-file)
		(replace-regexp-in-string "'" ""
								  (replace-regexp-in-string
								   "\\\\ " " "
								   (source-shell-script-run-command
									"~/.config/scripts/functions.sh" command)))
	  nil)))

(defun open-bookmark ()
  "Open selected file from (read-bookmark-file)"
  (interactive)
  (let* ((bookmark-file-contents (read-bookmark-file))
		 (bookmark (if (not bookmark-file-contents)
					   (error "Bookmark file: %s doesn't exist" open-bookmark-file)
					 (ivy-read "Choose bookmark to open: "
							   (split-string bookmark-file-contents "\n"))))
		 (file (remove-prefix-to-first-space bookmark)))
	(when file
	  (find-file file))))

(define-key evil-normal-state-map (kbd "<leader>Bo") 'open-bookmark)

(defvar bookmark-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?# "<" st)
    (modify-syntax-entry ?\n ">" st)
    st)
  "Syntax table for 'bookmark-mode'")

(defvar bookmark-indent-offset 18
  "Indentation offset for 'bookmark-mode'")

(defvar bookmark-align-column 18
  "Column at which the second column should start in 'bookmark-mode'")

(defun bookmark-align-line ()
  "Align current line so the second column starts at 'bookmark-align-column'"
  (interactive)
  (save-excursion
    (beginning-of-line)
    (when (re-search-forward "^\\(\\S-+\\)\\s-*" (line-end-position) t)
      (let* ((key (match-string 1))
             (padding (max 1 (- bookmark-align-column (length key))))
             (spaces (make-string padding ?\s)))
        (replace-match (concat key spaces) t t)))))

(define-derived-mode bookmark-mode fundamental-mode "bookmark"
  "A major mode for editing Bookmark files"
  :syntax-table bookmark-mode-syntax-table
  (setq-local font-lock-defaults '((bookmark-font-lock-keywords))
			  comment-start "#"
              comment-end ""
			  tab-width bookmark-indent-offset
              indent-line-function #'bookmark-align-line)
  (define-key bookmark-mode-map (kbd "TAB") #'bookmark-align-line))

(defvar bookmark-font-lock-keywords
  '(("TODO:" . font-lock-warning-face)
    ("\\<\\(FIXME\\|BUG\\):" 1 font-lock-warning-face t)
	("^!.*:" . font-lock-keyword-face)
	)
  "Highlighting expressions for `bookmark-mode'")

(add-to-list 'auto-mode-alist '("\\.bookmark\\'" . bookmark-mode))

(defvar org-discipline-xp-storage-file "~/.config/emacs/mine/.org-xp"
  "Org-discipline storage for xp")

(defvar org-discipline-prefix "M-o D"
  "Org-discipline keybindings prefix")

(defvar org-discipline-xp-sum 60
  "Current amount of XP the user has accumulated")

(defvar org-discipline-required-starting-xp 100
  "Base amount of 'XP' required to level up in the discipline system.
This value may increase with each level by 'org-discipline-xp-sum'")

(defvar org-discipline-level 0
  "Level in org-discipline. It automatically will be determined
according to xp in 'org-discipline-xp-storage-file'")

(defun org-discipline-get-level (xp &optional do-print-information)
  "Get level according to 'XP'. Print information if
'DO-PRINT-INFORMATION' is not nil"
  (let ((level 0)
		(required org-discipline-required-starting-xp))
	(while (>= xp required)
	  (setq level (+ level 1)
			required (+ required org-discipline-xp-sum)
			xp (- xp org-discipline-xp-sum)))
	(if do-print-information
		(message " level: %d,  xp: %d,  required xp for new level: %d"
				 level xp required)
	  level)))

(defun org-discipline-get-stored-xp ()
  "Get xp from 'org-discipline-xp-storage-file' file location.
If file doesn't exist, create it and write 0 to the file"
  (interactive)
  (unless (file-exists-p org-discipline-xp-storage-file)
	(with-temp-buffer
	  (insert "0")
	  (write-region (point-min) (point-max) org-discipline-xp-storage-file)))
  (let ((xp (string-to-number (read-file org-discipline-xp-storage-file))))
	(if (called-interactively-p)
		(org-discipline-get-level xp t)
	  xp)))

(defun org-discipline-get-skipped-days ()
  "Substract current date and 'SCHEDULED'"
  (interactive)
  (let ((scheduled (org-get-scheduled-time nil)))
    (if scheduled
        (let* ((now (current-time))
               (days (floor (/
							 (float-time (time-subtract now scheduled)) 86400))))
		  (when (called-interactively-p)
			(if (< days 0)
				(message "Days until the task: %d" (* days -1))
			  (message "Skipped days: %d" days)))
		  days)
	  (when (called-interactively-p)
		(error "No SCHEDULED timestamp found"))
      0)))

(define-key org-mode-map (kbd "M-o g s") 'org-discipline-get-skipped-days)

(defun org-discipline-get-difficulty-xp (difficulty)
  "Get xp amount according to 'DIFFICULTY'"
  (pcase (downcase difficulty)
	("easy"   5)
	("medium" 10)
	("hard"   15)))

(defun org-discipline-get-difficulty-xp-loss-percentage (difficulty days-skipped)
  "Get xp loss percentage amount according to 'DIFFICULTY'"
  (if (> days-skipped 0)
	  (pcase (downcase difficulty)
		("easy"   0.3)
		("medium" 0.2)
		("hard"   0.1))
	0))

(defun org-discipline-get-skipped-days-xp-loss-percentage (days-skipped)
  "Get xp loss percentage amount according to skipped days.
Skipped days have formula: current date - scheduled"
  (cond
   ((>= days-skipped 10) 0.3)
   ((>= days-skipped 5) 0.2)
   ((>= days-skipped 2) 0.1)
   (t 0)))

(defun org-discipline-calculate ()
  "Sum stored stored xp according to ':DIFFICULTY' header property,
then write to 'org-discipline-xp-storage-file' file. Depending on
difficulty and days skipped count, you will lose xp and the easier
task the more xp you will lose. And the longer you skipped the
task, the more xp you will lose"
  (let* ((xp (org-discipline-get-stored-xp))
		 (difficulty (org-entry-get nil "DIFFICULTY" t))
		 (profit (org-discipline-get-difficulty-xp difficulty))
		 (skipped-days (org-discipline-get-skipped-days))
		 (loss-skipped (org-discipline-get-skipped-days-xp-loss-percentage
						skipped-days))
		 (loss-difficulty (org-discipline-get-difficulty-xp-loss-percentage
						   difficulty skipped-days))
		 (loss (+ loss-difficulty loss-skipped)))
	(if (> loss 0)
		(- xp (* xp loss))
	  (+ xp profit))))

(defun org-discipline-calculate-write-to-file ()
  "Calculate and redirect result to 'org-discipline-xp-storage-file'"
  (let ((calculated-xp (org-discipline-calculate)))
	(with-temp-file org-discipline-xp-storage-file
	  (insert (number-to-string calculated-xp)))))

(defun org-discipline-calculation-on-todo-write-file ()
  "Calculate xp according to ':DIFFICULTY' and skipped days.
If loss >= 0, xp from 'org-discipline-xp-storage-file' will be
substracted by summary loss percentage of xp. Otherwise, you will
get profit"
  (when (and (equal org-state "DONE") (org-entry-get nil "DIFFICULTY" t))
	(org-discipline-calculate-write-to-file)
	;; Dumb shit, but i don't know how to print it after "Entry repeats: ..."
	(run-at-time 0.01 nil
				(lambda ()
				(call-interactively 'org-discipline-get-stored-xp)))))

(define-minor-mode org-discipline-mode
  "Toggle 'org-discipline-mode' functional on or off"
  :global t
  :interactive t
  :keymap
  (list
   (cons (pkbd org-discipline-prefix "m") #'org-discipline-get-stored-xp)
   (cons (pkbd org-discipline-prefix "S") #'org-discipline-get-skipped-days)
   )

  (let ((target-hook 'org-after-todo-state-change-hook)
		(function 'org-discipline-calculation-on-todo-write-file))
	(if (eq org-discipline-mode nil)
		(remove-hook target-hook function)
	  (add-hook target-hook function t))
	)
  )
