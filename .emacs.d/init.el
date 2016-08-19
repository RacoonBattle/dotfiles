
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Install required packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Enable ELPA - Emacs Lisp Package Archive
(when (>= emacs-major-version 24)
  (require 'package)
  (setq package-archives '(("melpa-cn" . "http://elpa.zilongshanren.com/melpa/")
			   ("org-cn"   . "http://elpa.zilongshanren.com/org/")
			   ("gnu-cn"   . "http://elpa.zilongshanren.com/gnu/")))
    (package-initialize))

;; Let ELPA automatically install packages in the list
(defvar my-packages
  '(magit git-commit
    rich-minority smart-mode-line
    color-theme-solarized
    evil evil-leader
    helm helm-swoop
    company elpy
    flycheck
    dash
    undo-tree
    multi-term
    use-package
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

;; Start seperate daemons for gui and tty
(require 'server)
(if window-system
    (progn
      (setq server-name "gui")
      (unless (server-running-p)
	(server-start)
	(global-set-key (kbd "C-x C-c") '(lambda () ;Avoid C-x C-c close the gui daemon
					   (interactive)
					   (server-edit)
					   (make-frame-invisible nil t)))))
      (progn
	(setq server-name "server")
	(unless (server-running-p)
	  (server-start))))

;; Automatically update file if the buffer is modified outside emacs
(global-auto-revert-mode 1)

;; Use y/n instead of yes/no
(fset 'yes-or-no-p 'y-or-n-p)

;; Enable cua-mode by default
(cua-mode t)

;; Use shift + arrow keys to select text
(global-unset-key (vector (list 'shift 'left)))
(global-unset-key (vector (list 'shift 'right)))
(global-unset-key (vector (list 'shift 'up)))
(global-unset-key (vector (list 'shift 'down)))
(setq shift-selection-mode t)

;; Wrap long lines
(global-visual-line-mode 0)

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

;; Helm - incremental completion and selection narrowing framework for Emacs.
;; refer to http://tuhdo.github.io/helm-intro.html

(require 'helm)
(require 'helm-swoop)
(require 'helm-config)

(global-set-key (kbd "C-c h") 'helm-command-prefix) ; change default helm prefix
(global-unset-key (kbd "C-x c"))
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-c s") 'helm-swoop)
(global-set-key (kbd "C-c r") 'helm-resume)
(global-set-key (kbd "C-c /") 'helm-grep-do-git-grep)
(global-set-key (kbd "C-c m") 'helm-semantic-or-imenu)
(global-set-key (kbd "C-c j") 'helm-all-mark-rings) ; help jump back in mark rings

(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
(define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z

(define-key helm-map (kbd "C-r") 'helm-previous-line) ; Move up and down like isearch
(define-key helm-map (kbd "C-s") 'helm-next-line)
(define-key helm-map (kbd "C-l") 'helm-execute-persistent-action) ; to jump to the search result without colse the helm buffer

(helm-autoresize-mode 1)                      ; resize its buffer automatically to fit with the number of candidates
(setq helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
      helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
      helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
      helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
      helm-M-x-fuzzy-match                  t ; fuzzy match candidates
      helm-buffers-fuzzy-matching           t
      helm-locate-fuzzy-match               t
      helm-recentf-fuzzy-match              t
      helm-semantic-fuzzy-match             t
      helm-imenu-fuzzy-match                t
      helm-ff-file-name-history-use-recentf t)

(helm-mode 1)

;; Git modes
(require 'git-commit)
(require 'magit)
(global-set-key (kbd "C-x g") 'magit-status)

;; Read file's vim modeline to set Emacs's file local variables
(require 'vim-modeline)
(add-to-list 'find-file-hook 'vim-modeline/do)

;; Enable Undo Tree
(require 'undo-tree)
(global-undo-tree-mode)

;; Jump ring
(global-set-key (kbd "C-o") 'recent-jump-jump-backward)
(global-set-key (kbd "M-o") 'recent-jump-jump-forward)
(require 'recent-jump)

;; company-mode
(add-hook 'after-init-hook 'global-company-mode)
(global-set-key "\t" 'company-indent-or-complete-common)
(setq company-selection-wrap-around t)
(setq company-minimum-prefix-length 2)
(setq company-require-match 'never) ; to cancel selections by typing non-matching characters
(setq company-dabbrev-other-buffers t)
(setq company-dabbrev-downcase nil)
(setq company-dabbrev-ignore-case nil)
(setq company-tooltip-align-annotations t)

(eval-after-load 'company
  '(progn
     (setq company-backends
	   '((company-files
	      company-keywords
	      company-dabbrev-code)
	      (company-abbrev company-dabbrev)))
     (define-key company-active-map (kbd "TAB") 'company-complete)
     (define-key company-active-map (kbd "M-n") nil)
     (define-key company-active-map (kbd "M-p") nil)
     (define-key company-active-map (kbd "C-n") #'company-select-next)
     (define-key company-active-map (kbd "C-p") #'company-select-previous)))

(defun indent-or-complete ()
  (interactive)
  (if (looking-at "\\_>")
      (company-complete-common)
    (indent-for-tab-command)))

;; Set hippie-expand for auto completion
(global-set-key "\M- " 'hippie-expand)
(setq hippie-expand-try-functions-list
      '(try-expand-dabbrev
	try-expand-dabbrev-visible
	try-expand-dabbrev-all-buffers
	try-expand-dabbrev-from-kill
	try-complete-file-name-partially
	try-complete-file-name))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interface
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Color theme: Auto use light frames in GUI and dark frames in terminal
(load-theme 'solarized t)
(load-theme 'adwaita t)
(disable-theme 'solarized)
(disable-theme 'adwaita)

(if window-system		   	; for emacs startup
    (enable-theme 'adwaita)
  (progn
    (set-terminal-parameter nil 'background-mode 'dark)
    (set-frame-parameter nil 'background-mode 'dark)
      (enable-theme 'solarized)))

(add-hook 'after-make-frame-functions	; for emacsclient
	  (lambda (frame)
	    (select-frame frame)
	    (if (display-graphic-p frame)
		(progn
		  (disable-theme 'solarized)
		  (enable-theme 'adwaita))
	      (progn
		(disable-theme 'adwaita)
		(set-terminal-parameter frame 'background-mode 'dark)
		(set-frame-parameter frame 'background-mode 'dark)
		(enable-theme 'solarized)))))

;; No scroll bar
(set-scroll-bar-mode nil)

;; GUI font
(if window-system
    (progn
      (set-face-attribute ; Setting English Font
       'default nil :font "Dejavu Sans Mono 12")
      (dolist (charset '(kana han symbol cjk-misc bopomofo)) ;; Chinese Font
	(set-fontset-font (frame-parameter nil 'font)
			  charset
			  (font-spec :family "WenQuanYi Micro Hei Mono" :size 16)))))


;; No menu bar in tty
(unless window-system
  (menu-bar-mode -1))

;; No startup message
(setq inhibit-startup-message t)

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
(add-hook 'after-make-frame-functions-hook 'update-title)


;; Smart mode-line
(setq sml/name-width		40
      sml/line-number-format	"%4l"
      sml/mode-width		'full
      sml/theme 		'respectful
      sml/no-confirm-load-theme t)
(require 'smart-mode-line)
(sml/setup)

;; Hidden minor-mode, by rich-minority
(setq rm-excluded-modes
      '(" Guide"			;; guide-key mode
	" hc"				;; hardcore mode
	" vl"				;; global visual line mode enabled
	" Wrap"				;; shows up if visual-line-mode is enabled for that buffer
	" Omit"				;; omit mode in dired
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
	" Helm"
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

;; Automatically indent new line according to its context
(electric-indent-mode 1)

;; C programming
(setq c-default-style "linux")
(add-hook 'c-mode-common-hook
	  (lambda ()
	    (set (make-local-variable 'company-backends)
		 '(company-clang))))

;; Shell script
(setq sh-basic-offset 8)
(setq sh-indentation 8)
(add-hook 'sh-mode-hook
	  (lambda()
	    ;; unify tab width
	    (setq evil-shift-width sh-indentation)
	    ;; Disable bash here document completion, when typing <<
	    (sh-electric-here-document-mode -1)))

;; TCL script
(setq tcl-indent-level 8)		; use tab to indent
(setq tcl-continued-indent-level 8)
(add-hook 'tcl-mode-hook
	  (lambda ()
	    ;; overwrite tcl-indent-command
	    (define-key tcl-mode-map "\t" 'indent-or-complete)
	    ))

;; Python
(elpy-enable)
(add-hook 'python-mode-hook
	  (lambda ()
	    (remove-hook 'elpy-modules 'elpy-module-flymake)
	    (remove-hook 'elpy-modules 'elpy-module-highlight-indentation)))

;; Org-mode
(setq org-log-done 'time
      org-list-description-max-indent 5 ; set maximum indentation for description lists
      org-adapt-indentation nil	 ; prevent demoting heading also shifting text inside sections
      org-startup-truncated nil	 ; wraps the lines in org-mode
      org-confirm-babel-evaluate nil
      org-src-fontify-natively t
      org-src-tab-acts-natively t
      org-src-preserve-indentation nil
      org-edit-src-content-indentation 0)

(define-key global-map "\C-ca" 'org-agenda)
(add-hook 'org-mode-hook
	  (lambda()
	    ;; TAB to org-cycle in evil normal-state
	    (define-key evil-normal-state-map (kbd "TAB") 'org-cycle)
	    ;; Auto-indent text content
	    (local-set-key (kbd "RET") 'newline-and-indent)))

;; make org mode allow eval of some langs
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (C . t)
   (python . t)
   (sh . t)))


;; Org Capture
(global-set-key "\C-cc" 'org-capture)
(setq org-directory "~/Dropbox/Personal/org")
(setq org-capture-templates
      '(("t" "Todo" entry (file+datetree (concat org-directory "/daily.org"))
	 "* TODO %?\n")
	("l" "Long Todo" entry (file+headline (concat org-directory "/long-to-do.org") "Long Todo")
	 "* %?\nAdded: %U\n")
	("d" "Daily" entry (file+datetree (concat org-directory "/daily.org"))
	 "* %?\n")
	("n" "Notes, Ideas, Topics" entry (file (concat org-directory "/notes.org"))
	 "* %?\nAdded: %U")
	("s" "Scratch" plain (file "~/Dropbox/scratch.org")
	"* %?\n")
	("p" "Pastebin" plain (file "~/Dropbox/scratch.org")
	 "* %U\n#+BEGIN_SRC\n%x%?\n#+END_SRC")
	))


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

;; Text mode
(add-hook 'text-mode-hook
	  (lambda ()
	    (define-key text-mode-map (kbd "TAB") 'indent-or-complete)))

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
	    ;; overwrite TAB behavior
	    (define-key markdown-mode-map (kbd "TAB") 'indent-or-complete)
	    (define-key evil-normal-state-map (kbd "TAB") 'markdown-cycle)
	    ))

;; highlight trailing whitespace
(setq-default show-trailing-whitespace t)

;; Spell Checker
(defun turn-spell-checking-on ()
  "Turn speck-mode or flyspell-mode on."
  ;; (speck-mode 1)
  (flyspell-mode 1)
  )

(add-hook 'text-mode-hook 'turn-spell-checking-on)
(add-hook 'message-mode-hook 'turn-spell-checking-on)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Evil
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Enable evil
(setq evil-toggle-key "")	; remove default evil-toggle-key C-z, manually setup later
(setq evil-want-C-i-jump nil)	; don't bind [tab] to evil-jump-forward
(require 'evil)
(evil-mode 1)

;; Setup evil leader key
(require 'evil-leader)
(global-evil-leader-mode)
(evil-leader/set-leader ",")

;; Treat underscore as part of the word when searching
(setq-default evil-symbol-word-search 'symbol)

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
			      (undo-tree-visualizer-mode . emacs)
			      (dired-mode . emacs))
      do (evil-set-initial-state mode state))

;; Set default state into insert
(setq evil-default-state 'insert)

;; Function to insert date
(defun insert-date ()
  "Insert current date yyyy-mm-dd."
  (interactive)
  (when (use-region-p)
    (delete-region (region-beginning) (region-end) )
    )
  (insert (format-time-string "%Y-%m-%d"))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Misc
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Enable mouse support for emacs-nox
;; only non-modified single clicks are supported
;; Can use Shift-mouse for workaround
(defun enable-mouse-in-terminal (&optional frame)
  (unless window-system
    (require 'mouse)
    (xterm-mouse-mode t)
    (global-set-key [mouse-2] 'mouse-yank-at-click)
    (setq select-active-regions nil)
    (setq mouse-drag-copy-region t)
    (defun track-mouse (e))
    (setq mouse-sel-mode t)
    ))

(enable-mouse-in-terminal)
(add-hook 'after-make-frame-functions 'enable-mouse-in-terminal) ; for emacsclient

;; Access X clipboard by xsel
;; From https://stackoverflow.com/questions/64360/how-to-copy-text-from-emacs-to-another-application-on-linux/19625063#19625063

(defun copy-to-x-clipboard ()
  (interactive)
  (if (display-graphic-p)
      (progn
        (message "Yanked region to x-clipboard!")
        (call-interactively 'clipboard-kill-ring-save)
        )
    (if (region-active-p)
        (progn
          (shell-command-on-region (region-beginning) (region-end) "xsel -i -b")
          (message "Yanked region to clipboard!")
          (deactivate-mark))
      (message "No region active; can't yank to clipboard!")))
  )

(defun paste-from-x-clipboard ()
  (interactive)
  (if (display-graphic-p)
      (progn
        (clipboard-yank)
        (message "graphics active")
        )
    (insert (shell-command-to-string "xsel -o -b"))
    )
  )

;; Use xsel to copy/paste in emacs-nox
;; From https://hugoheden.wordpress.com/2009/03/08/copypaste-with-emacs-in-terminal/
(unless window-system
  (when (getenv "DISPLAY")
    (defun xsel-cut-function (text &optional push)
      (with-temp-buffer
	(insert text)
	(call-process-region (point-min) (point-max) "xsel" nil 0 nil "--clipboard" "--input")))
    (defun xsel-paste-function()
      (let ((xsel-output (shell-command-to-string "xsel --clipboard --output")))
	(unless (string= (car kill-ring) xsel-output)
	  xsel-output )))
    (setq interprogram-cut-function 'xsel-cut-function)
    (setq interprogram-paste-function 'xsel-paste-function)
    (defun toggle-xsel-copy-paste()
      "Toggle if use xsel as default clipboard"
      (interactive)
      (if (eq interprogram-cut-function nil)
	  (progn
	    (setq interprogram-cut-function 'xsel-cut-function)
	    (setq interprogram-paste-function 'xsel-paste-function)
	    (message "Using X PRIMARY selection to copy/paste by xsel"))
	(progn
	  (setq interprogram-cut-function nil)
	  (setq interprogram-paste-function nil)
	  (message "Using Emacs internal clipboard to copy/paste"))
	))
    ))

;; Mimic Vim's set paste
;; From http://stackoverflow.com/questions/18691973/is-there-a-set-paste-option-in-emacs-to-paste-paste-from-external-clipboard
(defvar ttypaste-mode nil)
(add-to-list 'minor-mode-alist '(ttypaste-mode " Paste"))
(defun ttypaste-mode ()
  (interactive)
  (message "Start ttypaste-mode")
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

;; When cursor is on edge, move to the other side, as in a toroidal space
(setq windmove-wrap-around t )

;; Setup multi-term
(global-set-key (kbd "<C-next>") 'multi-term-next) ; C-PgUp/PgDn to siwtch mutli-term
(global-set-key (kbd "<C-prior>") 'multi-term-prev)
(when (require 'term nil t) ; only if term can be loaded..
  (setq term-bind-key-alist ; let history behavior like bash read-line
	(list (cons "C-c C-c"	'term-interrupt-subjob)
	      (cons "C-c C-j"	'term-line-mode)
	      (cons "C-c C-k"	'term-char-mode)
	      (cons "M-."	'term-send-raw-meta)
	      (cons "M-#"	'term-send-raw-meta)
	      (cons "C-p"	'term-send-up)
	      (cons "C-n"	'term-send-down)
	      (cons "M-f"	'term-send-forward-word)
	      (cons "M-b"	'term-send-backward-word)
	      (cons "M-DEL"	'term-send-backward-kill-word)
	      (cons "M-d"	'term-send-forward-kill-word)
	      (cons "<C-left>"	'term-send-backward-word)
	      (cons "<C-right>" 'term-send-forward-word)
	      (cons "C-r"	'term-send-reverse-search-history)
	      (cons "C-y"	'term-send-raw))))
(add-hook 'term-mode-hook '(lambda () (linum-mode 0))) ; turn-off line number

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Hotkeys
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Fix alt-<arrow> not working in terminal mode
(define-key input-decode-map "\e\eOA" [(meta up)])
(define-key input-decode-map "\e\eOB" [(meta down)])
(define-key input-decode-map "\e\eOD" [(meta left)])
(define-key input-decode-map "\e\eOC" [(meta right)])

;; Mark function
(global-unset-key (kbd "C-SPC"))
(global-set-key (kbd "M-SPC") 'set-mark-command)

;; Keybind with evil-leader
(evil-leader/set-key
  "," 'other-window
  "k" 'kill-buffer
  "w" 'save-buffer
  "p" 'browse-url-of-file)

;; Keybind with global-set-key
(global-set-key (kbd "C-c id") 'insert-date)
(global-set-key (kbd "<f2>") 'save-buffer)
(global-set-key (kbd "<f3>") 'org-capture)
(global-set-key (kbd "<f4>") '(lambda () ; split-window and open multi-term
				(interactive)
				(split-window-right)
				(other-window 1)
				(multi-term)))
(global-set-key (kbd "<f9>") '(lambda () ; Recursive grep in directory
				(interactive)
				(let ((current-prefix-arg 't))
				  (call-interactively 'helm-grep-do-git-grep))))
(global-set-key (kbd "<f11>") 'toggle-xsel-copy-paste)
(global-set-key (kbd "<f12>") 'ttypaste-mode)


;; Keybinding to override all minor modes, provided by use-package
(require 'bind-key)
(bind-keys*
 ("M-TAB" . other-window))
