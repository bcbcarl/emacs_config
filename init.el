;; init.el --- Where all the magic begins

;;; Commentary:

;;; Code:

;; Flycheck
(defvar flycheck-emacs-lisp-load-path 'inherit)

;; Load init files
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; ELPA
(require 'init-elpa)

;; No startup screen
(setq inhibit-startup-screen t)

;; Mac only
(require 'init-osx)

;; Remove menubar (execpt OSX), toolbar and scrollbar
(and (fboundp 'menu-bar-mode)
     (not *is-a-mac*)
     (menu-bar-mode -1))
(dolist (mode '(tool-bar-mode scroll-bar-mode))
  (when (fboundp mode) (funcall mode -1)))

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

;; PHP
(require 'init-php)

;; Web
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

(defun my-web-mode-hook ()
  "Hooks for Web mode."
  (setq web-mode-markup-indent-offset 2)
)
(add-hook 'web-mode-hook 'my-web-mode-hook)

(add-hook 'web-mode-hook 'emmet-mode)
(add-hook 'web-mode-hook 'ac-emmet-html-setup)
(add-hook 'css-mode-hook 'ac-emmet-css-setup)

(setq web-mode-ac-sources-alist
  '(("php" . (ac-source-yasnippet ac-source-php-auto-yasnippets))
    ("html" . (ac-source-emmet-html-aliases ac-source-emmet-html-snippets))
    ("css" . (ac-source-css-property ac-source-emmet-css-snippets))))

(add-hook 'web-mode-before-auto-complete-hooks
          '(lambda ()
             (let ((web-mode-cur-language
                    (web-mode-language-at-pos)))
               (if (string= web-mode-cur-language "php")
                   (yas-activate-extra-mode 'php-mode)
                 (yas-deactivate-extra-mode 'php-mode))
               (if (string= web-mode-cur-language "css")
                   (setq emmet-use-css-transform t)
                 (setq emmet-use-css-transform nil)))))

;; Backup settings
(setq
  backup-by-copying t
  backup-directory-alist
    '(("." . "~/.saves"))
  delete-old-versions t
  kept-new-versions 6
  kept-old-versions 2
  version-control t)

(provide 'init)
;;; init.el ends here
