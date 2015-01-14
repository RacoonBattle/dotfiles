;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; General
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; use y/n instead of yes/no
(fset 'yes-or-no-p 'y-or-n-p)

;; wrap long lines
(global-visual-line-mode)

;; remember last edit position
(setq save-place-file "~/.emacs.d/saveplace")
(setq-default save-place t)
(require 'saveplace)

;; cleanup trailing whitespace when saving
(add-hook 'before-save-hook 'whitespace-cleanup)
;; make whitespace-mode use just basic coloring
(setq whitespace-style (quote (spaces tabs newline space-mark tab-mark newline-mark)))

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

;; no startup message
(setq inhibit-startup-message t)

;; no menu-bar, tool-bar, scroll-bar
(menu-bar-mode -1)
(if (display-graphic-p)
    (progn
      (tool-bar-mode -1)
      (scroll-bar-mode -1)))

;; GUI font
(if (display-graphic-p)
    (progn
      (set-frame-font "Terminus-15")
      (set-fontset-font "fontset-default" 'han "AR PL UMing TW-12")))

;; show column-number
(setq column-number-mode t)

;; line number
(global-linum-mode t)
(setq linum-format "%4d ")

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
;; Backup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Put autosave files (ie #foo#) and backup files (ie foo~) in ~/.emacs.d/.
(custom-set-variables
 '(auto-save-file-name-transforms '((".*" "~/.emacs.d/autosaves/\\1" t)))
 '(backup-directory-alist '((".*" . "~/.emacs.d/backups/"))))

;; create the autosave dir if necessary, since emacs won't.
(make-directory "~/.emacs.d/autosaves/" t)

(setq backup-by-copying t)
(setq delete-old-versions t
      kept-new-versions 7
      kept-old-versions 3
      version-control t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Programming
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; C-indenting: use Linux kernel coding style
(setq c-default-style "linux")

;; shell-script: use tab to indent
(setq sh-basic-offset 8)

;; disable bash here document completion, when typing <<
(add-hook 'sh-mode-hook
	  (lambda ()
	    (sh-electric-here-document-mode -1)))

;; Automatically indent new line according to its context
(electric-indent-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Mail with mutt
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-to-list 'auto-mode-alist '(".*mutt.*" . message-mode))
(setq mail-header-separator "")			; for M-q and auto-fill to work correctly
(add-hook 'message-mode-hook 'auto-fill-mode)	; break lines at text width=72
(add-hook 'message-mode-hook			; colorizing multiply-quoted lines
	  (lambda ()
	    (font-lock-add-keywords nil
				    '(("^[ \t]*>[ \t]*>[ \t]*>.*$"
				       (0 'message-multiply-quoted-text-face))
				      ("^[ \t]*>[ \t]*>.*$"
				                         (0 'message-double-quoted-text-face))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; org-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq org-startup-indented t)
(setq org-hide-leading-stars t)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done 'time)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Evil
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-to-list 'load-path "~/.emacs.d/evil")
(setq evil-toggle-key "")	; remove default evil-toggle-key C-z, manually setup later
(setq evil-want-C-i-jump nil)	; don't bind [tab] to evil-jump-forward
(require 'evil)
(evil-mode 1)

;; remove all keybindings from insert-state keymap, use emacs-state when editing
(setcdr evil-insert-state-map nil)

;; ESC to switch back normal-state
(define-key evil-insert-state-map [escape] 'evil-normal-state)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Plugins
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; default load-path
(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp"))

;; Ido, Interactively open files and buffers
(ido-mode 1)

;; Smex, a M-x enhancement for Emacs
(require 'smex)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

;; read file's vim modeline to set Emacs's file local variables
(require 'vim-modeline)
(add-to-list 'find-file-hook 'vim-modeline/do)

;; Markdown mode
(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;; Diable markdown-mode’s auto-indent and trailing whitespace clean
(add-hook 'markdown-mode-hook
	  (lambda ()
	    (remove-hook 'before-save-hook 'delete-trailing-whitespace)
	    (set (make-local-variable 'electric-indent-mode) nil)
	    (define-key markdown-mode-map (kbd "RET") 'newline)
	    ))

;; Undo Tree
(require 'undo-tree)
(global-undo-tree-mode)

;; Smart mode-line
(setq sml/name-width		40
      sml/line-number-format	"%4l"
      sml/mode-width		'full
      sml/themea		'dark
      sml/no-confirm-load-theme t)
(require 'smart-mode-line)
(sml/setup)

;; Hidden minor-mode, by rich-minority
(setq rm-excluded-modes
      '(" Guide"			;; guide-key mode
	" hc"				;; hardcore mode
	" AC"				;; auto-complete
	" vl"				;; global visual line mode enabled
	" Wrap"				;; shows up if visual-line-mode is enabled for that buffer
	" Omit"				;; omit mode in dired
	" yas"				;; yasnippet
	" drag"				;; drag-stuff-mode
	" VHl"				;; volatile highlights
	" ctagsU"			;; ctags update
	" Undo-Tree"			;; undo tree
	" wr"				;; Wrap Region
	" SliNav"			;; elisp-slime-nav
	" Fly"				;; Flycheck
	" PgLn"				;; page-line-break
	" GG"				;; ggtags
	" ElDoc"			;; eldoc
	" hl-highlight"			;; hl-anything
	))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Hotkeys
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-set-key (kbd "<f2>") 'save-buffer)
(global-set-key (kbd "<f3>") 'linum-mode)
