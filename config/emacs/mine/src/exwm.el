(defun exwm-update-class ()
  (exwm-workspace-rename-buffer (format "*%s*" exwm-class-name)))

(defun exwm-change-workspace-map ()
  (setq exwm-workspace-index-map
		(lambda (index) (number-to-string (1+ index))))

  (dotimes (i 10)
	(exwm-input-set-key (kbd (format "s-%d" i))
						`(lambda ()
						   (interactive)
						   (exwm-workspace-switch-create (1- ,i))))))

(defun run-in-background (command)
  (let ((command-parts (split-string command "[ ]+")))
    (apply #'call-process `(,(car command-parts) nil 0 nil ,@(cdr command-parts)))))

(defun update-displays ()
  (efs/run-in-background "autorandr --change --force")
  (message "Display config: %s"
           (string-trim (shell-command-to-string "autorandr --current"))))

(define-key exwm-mode-map [?\C-q] 'exwm-input-send-next-key)

(setq exwm-input-global-keys
    `(
        ;; Reset to line-mode
        ;; (C-c C-k switches to char-mode via exwm-input-release-keyboard)
        ([?\s-r] . exwm-reset)

        ;; launch application launcher
        ([?\s-p] . counsel-linux-app)

        ;; interactive shell command
        ([?\s-&] . (lambda (command)
                    (interactive (list (read-shell-command "$ ")))
                    (start-process-shell-command command nil command)))

        ;; Move between windows
        ([s-h] . windmove-left)
        ([s-l] . windmove-right)
        ([s-k] . windmove-up)
        ([s-j] . windmove-down)

        ;; Switch workspace
        ([?\s-w] . exwm-workspace-switch)
    ))

(pcase exwm-class-name
  ("Firefox"         (exwm-workspace-move-window 1))
  ("Spotify"         (exwm-workspace-move-window 2))
  ("ViberPC"         (exwm-workspace-move-window 3))
  ("TelegramDesktop" (exwm-workspace-move-window 3))
  ("discord"         (exwm-workspace-move-window 4))
  ("qBittorrent"     (exwm-workspace-move-window 8))
  ("zoom"            (exwm-workspace-move-window 9))
  )

(use-package exwm
  :init
  ;; These keys should always pass through to Emacs
  (setq exwm-input-prefix-keys
    '(?\C-x
      ?\C-u
      ?\C-h
      ?\M-x
      ?\M-`
      ?\M-&
      ?\M-:
      ?\C-\M-j  ;; Buffer list
      ?\C-\ ))  ;; Ctrl+Space
  (dolist (key '( ;; multimedia and other keys
          XF86AudioRaiseVolume XF86AudioLowerVolume XF86AudioMute print
          XF86AudioMicMute XF86Bluetooth XF86AudioPlay
          XF86AudioPause XF86AudioPrev XF86AudioNext
          XF86AudioStop XF86MonBrightnessUp XF86MonBrightnessDown
          XF86ScreenSaver XF86WLAN))
    (cl-pushnew key exwm-input-prefix-keys))
  (setq exwm-workspace-number 9)
  (exwm-change-workspace-map)
  :config
  ;; Add hooks
  (add-hook 'exwm-update-class-hook #'exwm-update-class)
  (add-hook 'exwm-randr-screenchange-hook #'update-displays)

  ;; Follow mouse and focus on 2nd monitor
  (setq mouse-autoselect-window t
        focus-follows-mouse t)

  (exwm-enable))
