;;; package --- Summary
;;; Commentary:
;;; Code:

(require 'package)

(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)

(package-initialize)

(defvar my-packages
  '(ac-emmet
    cider
    clj-refactor
    editorconfig
    esup
    evil
    fish-mode
    flycheck
    git-gutter
    git-timemachine
    golden-ratio
    google-translate
    highlight-parentheses
    js2-mode
    js2-refactor
    lua-mode
    markdown-mode
    material-theme
    neotree
    paredit
    php-auto-yasnippets
    php-mode
    recentf
    smex
    tide
    web-mode
    yasnippet))

(when (not package-archive-contents)
  (package-refresh-contents))

(dolist (package my-packages)
  (unless (package-installed-p package)
    (package-install package)))

(cond ((eq system-type 'windows-nt)
       (unless (package-installed-p 'powershell)
               (package-install 'powershell)))
      (t (unless (package-installed-p 'shell-pop)
                 (package-install 'shell-pop))))

(provide 'init-package)
;;; init-package.el ends here
