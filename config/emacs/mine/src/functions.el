(defun pkbd (prefix keychord)
  "Get keybinding using prefix.
Useful when you make your functional and the functional has its prefix.
It is not very convenient to always create keybindings with the same prefix like
(kbd (concat prefix \" \" keychord)).
It is much better to just use
(pkbd prefix keychord)
"
  (kbd (concat prefix " " keychord)))

(defun global-bind (keybindings-alist &optional prefix)
  "Bind commands to keybindings sometimes using prefix.
Usage is here:
(global-bind '(
    (\"C-c j f\" . function-call)
    (\"C-c j o\" . otherfunction-call)))

If you want to create prefix keybindings, you need to set prefix to
some prefix, like C-c S, which will look like
(global-bind '(
    (\"f\" . function-call)
    (\"o\" . otherfunction-call)) \"C-c j\")
"

  (dolist (alist keybindings-alist)
	(if prefix
		(global-set-key (pkbd prefix (car alist)) (cdr alist))
	  (global-set-key (kbd (car alist)) (cdr alist)))))

(defun map-bind (keybindings-alist keymap &optional prefix)
  "Bind commands to keybindings for keymap sometimes using prefix.
Usage is here:
(map-bind '((\"C-c j f\" . function-call)
            (\"C-c j o\" . otherfunction-call)) php-mode-map)

If you want to create prefix keybindings, you need to set prefix to
some prefix, like C-c S, which will look like
(map-bind '((\"f\" . function-call)
            (\"o\" . otherfunction-call)) php-mode-map \"C-c j\")
"

  (dolist (alist keybindings-alist)
	(if prefix
		(define-key keymap (pkbd prefix (car alist)) (cdr alist))
	  (define-key keymap (kbd (car alist)) (cdr alist)))))

(defun get-only-file-name (file-path)
  "file-path can be buffer-file-name"
  (interactive)
  (file-name-nondirectory file-path))

(defun read-file (filename)
  "Return string that is readen from FILENAME"
  (with-temp-buffer
	(insert-file-contents filename)
	(buffer-string)))

(defun copy-string-to-clipboard (string)
  "Copy string to the clipboard"
  (when string
    (kill-new string)
    (message "Copied to clipboard: %s" string)))

(defun read-conf-variable (file variable-name)
  "Read the value of VARIABLE-NAME from FILE with format key=value"
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (let ((regex (format "^%s[ \t]*=[ \t]*\\(.*\\)$" (regexp-quote variable-name)))
          value)
      (while (re-search-forward regex nil t)
        (setq value (string-trim (match-string 1))))
      value)))

(defmacro with-conf-variable (variable value &rest body)
  "Execute depending on variable which should be equal to value.
Usage:
(with-conf-variable \"profile\" \"no-wm\"
  (set-frame-parameter nil 'alpha-background 93))"

  (declare (indent defun))
  `(when (equal
		  (read-conf-variable "~/.files-balamah/settings.conf" ,variable) ,value)
	 ,@body))

(defmacro unless-conf-variable (variable value &rest body)
  "Execute depending on variable which should NOT be equal to value.
Usage:
(unless-conf-variable \"profile\" \"no-wm\"
  (set-frame-parameter nil 'alpha-background 93))"

  (declare (indent defun))
  `(unless (equal
		  (read-conf-variable "~/.files-balamah/settings.conf" ,variable) ,value)
	 ,@body))

(defmacro if-conf-variable (variable value then else)
  "Execute depending on variable. If condition is t, then it will
execute first code block, otherwise it will execute second.
Just like 'if' in elisp"

  (declare (indent defun))
  `(if (equal
		(read-conf-variable "~/.files-balamah/settings.conf" ,variable) ,value)
	   ,then
	 ,else))

(defmacro with-system (types &rest body)
  "Execute code if system-type is one of systems in list.
It should look like
(with-system '(gnu/kfreebsd gnu/linux)
  (some-code-here))
"
  (declare (indent defun))
  `(when (member system-type ,types)
     ,@body))

(defmacro with-not-system (types &rest body)
  "Execute code if system-type is not from types list.
It should look like
(with-not-system '(darwin windows-nt)
  (some-code-here)
  (it-wont-execute-on-darwin-and-windows-nt))
"
  (declare (indent defun))
  `(when (not (member system-type ,types))
     ,@body))

(defun crontab-edit ()
  "Open crontab -e in emacs buffer"
  (interactive)
  (with-editor-async-shell-command "crontab -e"))

(defun remove-prefix-to-first-space (str)
  "Remove everything before and including the first space in the string"
  (replace-regexp-in-string "^.*?\\s-+" "" str))

(defun source-shell-script-run-command (shell-config-file command)
  "Source the shell script and run the command in the same shell process"
  (let ((script (concat "source " shell-config-file " && " command)))
    (shell-command-to-string script)))

(defun org-get-keyword-variable-value (key)
  "Get value from org-mode header"
  (let ((parsed (org-element-parse-buffer)))
    (org-element-map parsed 'keyword
      (lambda (el)
        (when (string-equal (org-element-property :key el) key)
          (org-element-property :value el)))
      nil t)))
