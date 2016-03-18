;; init.el --- Where all the magic begins

;;; Commentary:

;;; Code:

;; Set PATH
(setenv "PATH"
	(concat "/usr/local/bin:" (getenv "PATH")))
(setq exec-path
      (append exec-path '("/usr/local/bin")))

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
(dolist (mode '(menu-bar-mode tool-bar-mode scroll-bar-mode))
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

;; No cursor blinking
(blink-cursor-mode 0)

;; Full path in title bar
(setq-default frame-title-format "%b (%f)")

;; No ring bell
(setq ring-bell-function 'ignore)

;; ido-mode
(ido-mode t)
(setq ido-enable-flex-matching t)

;; ibuffer
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; Load init files
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; Set English and CJK fonts
(require 'init-fonts)

;; Install packages
(require 'init-package)

;; Paredit
(eval-after-load "paredit"
  #'(define-key paredit-mode-map (kbd "C-j") 'eval-last-sexp))
(add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
(add-hook 'ielm-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
(add-hook 'scheme-mode-hook           #'enable-paredit-mode)

;; highlight-parentheses
(add-hook 'prog-mode-hook #'highlight-parentheses-mode)

;; smex
(setq smex-save-file (expand-file-name ".smex-items" user-emacs-directory))
(global-set-key [remap execute-extended-command] 'smex)

;; Recent files
(setq recentf-save-file (concat user-emacs-directory ".recentf"))
(recentf-mode 1)

;; Flycheck
(defvar flycheck-emacs-lisp-load-path 'inherit)

;; flycheck-package
(eval-after-load 'flycheck
  '(flycheck-package-setup))

;; Mac OS X related configurations
(defconst *is-a-mac* (eq system-type 'darwin))
(when *is-a-mac*
  (require 'init-osx))

;; material-theme
(load-theme 'material t)

(dolist (mode '(column-number-mode line-number-mode))
  (when (fboundp mode) (funcall mode t)))

;; linum, evil, flycheck
(dolist (mode-hook '(text-mode-hook prog-mode-hook conf-mode-hook))
        (add-hook mode-hook
          (lambda ()
            (linum-mode 1)
            (evil-mode 1)
            (flycheck-mode 1))))

;; ggtags
(add-hook 'c-mode-common-hook
          (lambda ()
            (when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
              (ggtags-mode 1))))

;; golden-ratio
(golden-ratio-mode 1)
(setq golden-ratio-auto-scale t)
(with-eval-after-load 'golden-ratio
  (add-to-list 'golden-ratio-exclude-buffer-names " *NeoTree*"))

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

(add-hook 'term-mode-hook (lambda() (setq yas-dont-activate t)))
;; Bind key to powershell if using Windows
(cond ((eq system-type 'windows-nt)
       (global-set-key (kbd "C-x C-t") 'powershell))
      ;; Bind key to shell-pop, otherwise
      (t (custom-set-variables
          '(shell-pop-shell-type
            (quote ("ansi-term" "*ansi-term*"
                    (lambda nil (ansi-term shell-pop-term-shell)))))
          '(shell-pop-universal-key "C-x C-t")
          '(shell-pop-full-span t))))

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

;; Clojure
(require 'init-clojure)

;; Google Translate
(require 'init-translate)

;; git-timemachine
;; @see http://blog.binchen.org/posts/use-git-timemachine-with-evil.html
(eval-after-load 'git-timemachine
  '(progn
     (evil-make-overriding-map git-timemachine-mode-map 'normal)
     ;; force update evil keymaps after git-timemachine-mode loaded
     (add-hook 'git-timemachine-mode-hook #'evil-normalize-keymaps)))

;; yaml-mode
(add-hook 'yaml-mode-hook
          (lambda ()
            (define-key yaml-mode-map "\C-m" 'newline-and-indent)))

;; Tide
(add-hook 'typescript-mode-hook
          (lambda ()
            (tide-setup)
            (flycheck-mode +1)
            (setq flycheck-check-syntax-automatically '(save mode-enabled))
            (eldoc-mode +1)))

;; wttrin
(defvar wttrin-default-cities '("Taipei"))
(require 'wttrin)

(provide 'init)
;;; init.el ends here
