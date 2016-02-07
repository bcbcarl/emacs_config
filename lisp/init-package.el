;;; package --- Summary
;;; Commentary:
;;; Code:

(dolist (package '(ac-emmet cider editorconfig evil flycheck git-gutter js2-mode
                   js2-refactor lua-mode markdown-mode material-theme neotree
                   php-auto-yasnippets php-mode shell-pop tide web-mode
                   yasnippet))
  (unless (package-installed-p package)
    (package-install package)))

(provide 'init-package)
;;; init-package.el ends here
