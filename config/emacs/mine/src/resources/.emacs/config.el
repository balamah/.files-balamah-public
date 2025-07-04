(defvar current-project (project-get-root)
  "Current project directory")

;; run project
(defrunc project
  (let ((launcher (project-config-file-directory-get-path "launcher")))
    (vterm-run (format "sh %s" launcher))))

(global-set-key (kbd "S-<f10>") 'run-project)

;; add snippets
(let ((snippet-directory (project-config-file-directory-get-path "snippets")))
  (add-to-list 'yas-snippet-dirs 'snippet-directory)
  (yas-load-directory snippet-directory))
