(dolist (file '("~/.config/emacs/mine/src/functions.el"
				"~/.config/emacs/mine/src/standart-stuff.el"
				"~/.config/emacs/mine/src/plugins.el"
				"~/.config/emacs/mine/src/custom-functional.el"
				"~/.config/emacs/mine/src/local.el"
				))
  (load file))

(with-system '(gnu/linux gnu/kfreebsd)
  (with-conf-variable "profile" "exwm"
	(load "~/.config/emacs/mine/src/exwm.el")))

(let ((temporary-configuration "/tmp/.files-balamah/temporary-config.el"))
  (when (file-exists-p temporary-configuration)
	(load temporary-configuration)))
