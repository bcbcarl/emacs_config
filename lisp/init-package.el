;;; package --- Summary
;;; Commentary:
;;; Code:

(require 'package)

(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-pinned-packages '(cider . "melpa-stable") t)

(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(defvar my-packages
  '(ac-emmet
    cider
    clj-refactor
    editorconfig
    evil
    flycheck
    git-gutter
    js2-mode
    js2-refactor
    lua-mode
    markdown-mode
    material-theme
    neotree
    php-auto-yasnippets
    php-mode
    recentf
    smex
    tide
    web-mode
    yasnippet))

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
