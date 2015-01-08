;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; General
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; use y/n instead of yes/no
(fset 'yes-or-no-p 'y-or-n-p)

;; Interactively open files and buffers
(ido-mode 1)

;; set text-mode as default major-mode
(setq default-major-mode 'text-mode)

;; set current_dir/filename as buffer name, and show in mode line
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; use UTF-8 encoding
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; Use xsel to access the X clipboard
;; From https://hugoheden.wordpress.com/2009/03/08/copypaste-with-emacs-in-terminal/
(unless window-system
 (when (getenv "DISPLAY")
   ;; Callback for when user cuts
   (defun xsel-cut-function (text &optional push)
     ;; Insert text to temp-buffer, and "send" content to xsel stdin
     (with-temp-buffer
       (insert text)
       ;; Use primary the primary selection
       ;; mouse-select/middle-button-click
       (call-process-region (point-min) (point-max) "xsel" nil 0 nil "--primary" "--input")))
   ;; Call back for when user pastes
   (defun xsel-paste-function()
     ;; Find out what is current selection by xsel. If it is different
     ;; from the top of the kill-ring (car kill-ring), then return
     ;; it. Else, nil is returned, so whatever is in the top of the
     ;; kill-ring will be used.
     (let ((xsel-output (shell-command-to-string "xsel --primary --output")))
       (unless (string= (car kill-ring) xsel-output)
         xsel-output)))
   ;; Attach callbacks to hooks
   (setq interprogram-cut-function 'xsel-cut-function)
   (setq interprogram-paste-function 'xsel-paste-function)))

;;;hippie-expand
(global-set-key (kbd "M-/") 'hippie-expand)
(setq hippie-expand-try-functions-list
      '(try-expand-dabbrev
        try-expand-dabbrev-visible
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-complete-file-name-partially
        try-complete-file-name
        try-expand-line
	))

;; remove the prompt for killing emacsclient buffers
(defun server-remove-kill-buffer-hook () (remove-hook
'kill-buffer-query-functions 'server-kill-buffer-query-function))
(add-hook 'server-visit-hook 'server-remove-kill-buffer-hook)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interface
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; no menu-bar
(menu-bar-mode -1)

;; show column-number
(setq column-number-mode t)

;; line number
(global-linum-mode t)
(setq linum-format "%4d ")
(global-set-key (kbd "<f3>") 'linum-mode)

;; show matching parenthesis.
(show-paren-mode 1)

;; solarized color theme
(add-to-list 'custom-theme-load-path "~/.emacs.d/emacs-color-theme-solarized/")
(load-theme 'solarized-dark t)

;; Workaround broken solarized colours in emacsclient. Issue #60
(if (daemonp)
    (add-hook 'after-make-frame-functions
	      (lambda (frame)
		(select-frame frame)
		(load-theme 'solarized-dark t)))
      (load-theme 'solarized-dark t))

;; Automatically set screen title
;; ref http://vim.wikia.com/wiki/Automatically_set_screen_title
;; FIXME: emacsclient in xterm will have problem if emacs daemon start in screen
(defun update-title ()
  (interactive)
  (if (getenv "STY")	; check whether in GNU screen
      (send-string-to-terminal (concat "\033k\033\134\033k" "Emacs("(buffer-name)")" "\033\134"))
    (send-string-to-terminal (concat "\033]2; " "Emacs("(buffer-name)")" "\007"))))
(add-hook 'post-command-hook 'update-title)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Indent
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; C-indenting: use Linux kernel coding style
(setq c-default-style "linux")

;; shell-script: use tab to indent
(setq sh-basic-offset 8)

;; Automatically indent new line according to its context
(electric-indent-mode 1)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; org-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq org-startup-indented t)
(setq org-hide-leading-stars t)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done 'time)


