
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Install required packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Enable ELPA - Emacs Lisp Package Archive
(when (>= emacs-major-version 24)
  (require 'package)
  (setq package-archives '(
			   ("gnu" . "http://elpa.gnu.org/packages/")
			   ("melpa" . "http://melpa.milkbox.net/packages/")
			   ("marmalade" . "http://marmalade-repo.org/packages/")))
  (package-initialize))

;; Let ELPA automatically install packages in the list
(defvar my-packages
  '(git-commit-mode git-rebase-mode gitconfig-mode gitignore-mode
		    rich-minority smart-mode-line
		    dash
		    smex
		    undo-tree
		    markdown-mode))

(unless package-archive-contents	; fetch the list of packages available
  (package-refresh-contents))

(dolist (p my-packages)			; install the missing packages
  (unless (package-installed-p p)
    (package-install p)))

;; Set default load-path, for customer elip configuration
(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; General
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Use y/n instead of yes/no
(fset 'yes-or-no-p 'y-or-n-p)

;; Wrap long lines
(global-visual-line-mode)

;; Remember last edit position
(setq save-place-file "~/.emacs.d/saveplace")
(setq-default save-place t)
(require 'saveplace)

;; Cleanup trailing whitespace when saving
(add-hook 'before-save-hook 'delete-trailing-whitespace)
;; make whitespace-mode use just basic coloring
(setq whitespace-style (quote (spaces tabs newline space-mark tab-mark newline-mark)))

;; Set current_dir/filename as buffer name, and show in mode line
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; Use UTF-8 encoding
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; Enable Ido - Interactively open files and buffers
(ido-mode 1)

;; Enable Smex - a M-x enhancement for Emacs
(require 'smex)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

;; Read file's vim modeline to set Emacs's file local variables
(require 'vim-modeline)
(add-to-list 'find-file-hook 'vim-modeline/do)

;; Enable Undo Tree
(require 'undo-tree)
(global-undo-tree-mode)

;; Set hippie-expand for auto completion
(global-set-key "\M- " 'hippie-expand)
(setq hippie-expand-try-functions-list
      '(try-expand-dabbrev
	try-expand-dabbrev-visible
	try-expand-dabbrev-all-buffers
	try-expand-dabbrev-from-kill
	try-complete-file-name-partially
	try-complete-file-name
	try-expand-line))

;; Mimic Vim's superTab, try: completion; except: tab-to-tab-stop
(defun my-indent-or-complete ()
  (interactive)
  (if (looking-at "\\>")
      (hippie-expand nil)
    (tab-to-tab-stop)))
(global-set-key (kbd "TAB") 'my-indent-or-complete)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interface
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Solarized color theme
(add-to-list 'custom-theme-load-path "~/.emacs.d/emacs-color-theme-solarized/")
(load-theme 'solarized-dark t)

;; No startup message
(setq inhibit-startup-message t)

;; No menu-bar, tool-bar, scroll-bar
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

;; Show column-number
(setq column-number-mode t)

;; Line number
(global-linum-mode t)
(setq linum-format "%4d ")

;; Show matching parenthesis.
(show-paren-mode 1)

;; Automatically set screen title
;; ref http://vim.wikia.com/wiki/Automatically_set_screen_title
;; FIXME: emacsclient in xterm will have problem if emacs daemon start in screen
(defun update-title ()
  (interactive)
  (if (getenv "STY")	; check whether in GNU screen
      (send-string-to-terminal (concat "\033k\033\134\033k" "Emacs("(buffer-name)")" "\033\134"))
    (send-string-to-terminal (concat "\033]2; " "Emacs("(buffer-name)")" "\007"))))
(add-hook 'post-command-hook 'update-title)


;; Smart mode-line
(setq sml/name-width		40
      sml/line-number-format	"%4l"
      sml/mode-width		'full
      sml/theme 		'dark
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
;; Backup
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Put autosave files (ie #foo#) and backup files (ie foo~) in ~/.emacs.d/.
(custom-set-variables
 '(auto-save-file-name-transforms '((".*" "~/.emacs.d/autosaves/\\1" t)))
 '(backup-directory-alist '((".*" . "~/.emacs.d/backups/"))))

;; Create the autosave dir if necessary, since emacs won't.
(make-directory "~/.emacs.d/autosaves/" t)

(setq backup-by-copying t)
(setq delete-old-versions t
      kept-new-versions 7
      kept-old-versions 3
      version-control t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Programming & Editing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Treat underscore as word character
(add-hook 'text-mode-hook #'(lambda () (modify-syntax-entry ?_ "w")))
(add-hook 'prog-mode-hook #'(lambda () (modify-syntax-entry ?_ "w")))

;; Automatically indent new line according to its context
(electric-indent-mode 1)

;; C-indenting: use Linux kernel coding style
(setq c-default-style "linux")

;; Shell-script: use tab to indent
(setq sh-basic-offset 8)

;; Disable bash here document completion, when typing <<
(add-hook 'sh-mode-hook
	  (lambda ()
	    (sh-electric-here-document-mode -1)))

;; Org-mode
(setq org-startup-indented t)
(setq org-hide-leading-stars t)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done 'time)

;; Mail with mutt
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

;; Markdown mode
(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;; Markdown previewer
(custom-set-variables '(markdown-command "/usr/bin/markdown_py"))

;; Diable markdown-mode’s auto-indent and trailing whitespace clean
(add-hook 'markdown-mode-hook
	  (lambda ()
	    (remove-hook 'before-save-hook 'delete-trailing-whitespace)
	    (set (make-local-variable 'electric-indent-mode) nil)
	    (define-key markdown-mode-map (kbd "RET") 'newline)
	    ;; Fix M-RET not working in terminal mode
	    (define-key markdown-mode-map (kbd "M-RET") 'markdown-insert-list-item)
	    ;; Restore TAB behavior
	    (define-key markdown-mode-map (kbd "TAB") 'my-indent-or-complete)
	    (define-key evil-normal-state-map (kbd "TAB") 'markdown-cycle)
	    ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Evil
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-to-list 'load-path "~/.emacs.d/evil")

;; Enable evil
(setq evil-toggle-key "")	; remove default evil-toggle-key C-z, manually setup later
(setq evil-want-C-i-jump nil)	; don't bind [tab] to evil-jump-forward
(require 'evil)
(evil-mode 1)

;; Setup evil leader key
(require 'evil-leader)
(global-evil-leader-mode)
(evil-leader/set-leader ",")

;; Remove all keybindings from insert-state keymap, use emacs-state when editing
(setcdr evil-insert-state-map nil)

;; ESC to switch back normal-state
(define-key evil-insert-state-map [escape] 'evil-normal-state)

;; TAB to indent in normal-state
(define-key evil-normal-state-map (kbd "TAB") 'indent-for-tab-command)

;; Use j/k to move one visual line insted of gj/gk
(define-key evil-normal-state-map (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
(define-key evil-normal-state-map (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)
(define-key evil-motion-state-map (kbd "<remap> <evil-next-line>") 'evil-next-visual-line)
(define-key evil-motion-state-map (kbd "<remap> <evil-previous-line>") 'evil-previous-visual-line)

;; Enter specified state for some mode
(loop for (mode . state) in '((inferior-emacs-lisp-mode . emacs)
			      (git-commit-mode . insert)
			      (git-rebase-mode . emacs)
			      (dired-mode . emacs))
      do (evil-set-initial-state mode state))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Misc
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Mimic Vim's set paste
;; From http://stackoverflow.com/questions/18691973/is-there-a-set-paste-option-in-emacs-to-paste-paste-from-external-clipboard
(defvar ttypaste-mode nil)
(add-to-list 'minor-mode-alist '(ttypaste-mode " Paste"))
(defun ttypaste-mode ()
  (interactive)
  (let ((buf (current-buffer))
	(ttypaste-mode t))
    (with-temp-buffer
      (let ((stay t)
	    (text (current-buffer)))
	(redisplay)
	(while stay
	  (let ((char (let ((inhibit-redisplay t)) (read-event nil t 0.1))))
	    (unless char
	      (with-current-buffer buf (insert-buffer-substring text))
	      (erase-buffer)
	      (redisplay)
	      (setq char (read-event nil t)))
	    (cond
	     ((not (characterp char)) (setq stay nil))
	     ((eq char ?\r) (insert ?\n))
	     ((eq char ?\e)
	      (if (sit-for 0.1 'nodisp) (setq stay nil) (insert ?\e)))
	     (t (insert char)))))
	(insert-buffer-substring text)))))


;; Remove the prompt for killing emacsclient buffers
(defun server-remove-kill-buffer-hook ()
  (remove-hook
   'kill-buffer-query-functions 'server-kill-buffer-query-function))
(add-hook 'server-visit-hook 'server-remove-kill-buffer-hook)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Hotkeys
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Fix alt-<arrow> not working in terminal mode
(define-key input-decode-map "\e\eOA" [(meta up)])
(define-key input-decode-map "\e\eOB" [(meta down)])
(define-key input-decode-map "\e\eOD" [(meta left)])
(define-key input-decode-map "\e\eOC" [(meta right)])

;; Keybind with evil-leader
(evil-leader/set-key
  "," 'other-window
  "f" 'ido-find-file
  "b" 'ido-switch-buffer
  "k" 'kill-buffer
  "w" 'save-buffer
  "p" 'browse-url-of-file)

;; Keybind with function keys
(global-set-key (kbd "<f2>") 'save-buffer)
(global-set-key (kbd "<f11>") 'linum-mode)
(global-set-key (kbd "<f12>") 'ttypaste-mode)
