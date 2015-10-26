;; init.el --- Where all the magic begins

;;; Commentary:

;;; Code:

;; Set PATH
(setenv "PATH"
  (concat
    "/usr/local/bin:"
    (getenv "PATH")
  )
)

;; Backup settings
(setq
  backup-by-copying t
  backup-directory-alist
    '(("." . "~/.saves"))
  delete-old-versions t
  kept-new-versions 6
  kept-old-versions 2
  version-control t)

;; Flycheck
(defvar flycheck-emacs-lisp-load-path 'inherit)

;; Load init files
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; No startup screen
(setq inhibit-startup-screen t)

;; ELPA
(require 'init-elpa)

;; Remove menubar, toolbar and scrollbar
(when (fboundp 'menu-bar-mode)
     (menu-bar-mode -1))
(dolist (mode '(tool-bar-mode scroll-bar-mode))
  (when (fboundp mode) (funcall mode -1)))

;; Mac only
(require 'init-osx)

;; material-theme
(load-theme 'material t)

;; Auto-fill Mode
(setq-default fill-column 79)
(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'prog-mode-hook 'turn-on-auto-fill)

;; Save Place in Opened Files
(setq-default save-place t)
(defvar save-place-file (concat user-emacs-directory ".saved-places"))
(require 'saveplace)

(dolist (mode '(column-number-mode line-number-mode))
  (when (fboundp mode) (funcall mode t)))

(dolist (mode-hook '(text-mode-hook prog-mode-hook conf-mode-hook))
        (add-hook mode-hook
          (lambda ()
            (linum-mode 1)
            (evil-mode 1)
	    (flycheck-mode 1))))

;; show parenthesis match
(defvar show-paren-delay 0)
(defvar show-paren-style 'expression)
(show-paren-mode 1)

;; Toggle line highlighting in all buffers
(global-hl-line-mode t)

;; frame font
;; Setting English Font
(if (member "Monaco" (font-family-list))
    (set-face-attribute
      'default nil :font "Monaco 12"))

;; Auto-Complete
(ac-config-default)

;; Git-Gutter
(require 'init-git)

;; Web
(require 'init-web)

;; Markdown
(require 'init-markdown)

;; IRC
(require 'init-irc)

(provide 'init)
;;; init.el ends here
