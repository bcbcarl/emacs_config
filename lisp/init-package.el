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
    dockerfile-mode
    editorconfig
    esup
    evil
    fish-mode
    flycheck
    flycheck-package
    ggtags
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
    ranger
    recentf
    restclient
    smex
    tide
    web-mode
    wttrin
    yaml-mode
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
