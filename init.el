;; init.el --- Where all the magic begins

;;; Commentary:

;;; Code:

;; Set PATH
(setenv "PATH"
  (concat "/usr/local/bin:" (getenv "PATH"))
)
(setq exec-path (append exec-path '("/usr/local/bin")))

;; Backup settings
(setq
  backup-by-copying t
  backup-directory-alist
    '(("." . "~/.saves"))
  delete-old-versions t
  kept-new-versions 6
  kept-old-versions 2
  version-control t)

;; Save Place in Opened Files
(setq-default save-place t)
(defvar save-place-file (concat user-emacs-directory ".saved-places"))
(require 'saveplace)

;; Remove newline insertion at end of file
(setq mode-require-final-newline nil)

;; sudo when necessary
(defadvice ido-find-file (after find-file-sudo activate)
  "Find file as root if necessary."
  (unless (and buffer-file-name (file-writable-p buffer-file-name))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

;; No startup screen
(setq inhibit-startup-screen t)

;; Remove menubar, toolbar and scrollbar
(when (fboundp 'menu-bar-mode)
     (menu-bar-mode -1))
(dolist (mode '(tool-bar-mode scroll-bar-mode))
  (when (fboundp mode) (funcall mode -1)))

;; Auto-fill Mode
(setq-default fill-column 79)
(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'prog-mode-hook 'turn-on-auto-fill)

;; show parenthesis match
(defvar show-paren-delay 0)
(defvar show-paren-style 'expression)
(show-paren-mode 1)

;; Toggle line highlighting in all buffers
(global-hl-line-mode t)

;; Delete by moving to trash
(setq delete-by-moving-to-trash t)

;; Setting English font
(if (member "Monaco" (font-family-list))
    (set-face-attribute
      'default nil :font "Monaco 14"))

;; Setting CJK font
(if (member "PingFang TC" (font-family-list))
    (dolist (charset '(kana han symbol cjk-misc bopomofo))
      (set-fontset-font (frame-parameter nil 'font)
			charset
			(font-spec :family "PingFang TC" :size 14))))

;; Load init files
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; Install packages
(require 'init-package)

;; Flycheck
(defvar flycheck-emacs-lisp-load-path 'inherit)

;; Mac OS X related configurations
(defconst *is-a-mac* (eq system-type 'darwin))
(when *is-a-mac*
  (require 'init-osx))

;; material-theme
(load-theme 'material t)

(dolist (mode '(column-number-mode line-number-mode))
  (when (fboundp mode) (funcall mode t)))

(dolist (mode-hook '(text-mode-hook prog-mode-hook conf-mode-hook))
        (add-hook mode-hook
          (lambda ()
            (linum-mode 1)
            (evil-mode 1)
            (flycheck-mode 1))))

;; neotree
(global-set-key [f8] 'neotree-toggle)
(add-hook 'neotree-mode-hook
	(lambda ()
	    (define-key evil-normal-state-local-map (kbd "TAB") 'neotree-enter)
	    (define-key evil-normal-state-local-map (kbd "SPC") 'neotree-enter)
	    (define-key evil-normal-state-local-map (kbd "q") 'neotree-hide)
	    (define-key evil-normal-state-local-map (kbd "RET") 'neotree-enter)))

;; Auto-Complete
(ac-config-default)

;; shell-pop
(custom-set-variables
  '(shell-pop-shell-type (quote ("ansi-term" "*ansi-term*" (lambda nil (ansi-term shell-pop-term-shell)))))
  '(shell-pop-universal-key "C-x C-t")
  '(shell-pop-full-span t))
(add-hook 'term-mode-hook (lambda() (setq yas-dont-activate t)))

;; Git-Gutter
(require 'init-git)

;; Lua
(require 'init-lua)

;; Web
(require 'init-web)

;; Markdown
(require 'init-markdown)

;; IRC
(require 'init-irc)

;; Tide
(add-hook 'typescript-mode-hook
          (lambda ()
            (tide-setup)
            (flycheck-mode +1)
            (setq flycheck-check-syntax-automatically '(save mode-enabled))
            (eldoc-mode +1)))

(provide 'init)
;;; init.el ends here