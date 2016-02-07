;;; init-osx.el --- init file for Mac OS X

;;; Commentary:

;; Mac OS X related configurations

;;; Code:

;; Make mouse wheel / trackpad scrolling less jerky
(setq mouse-wheel-scroll-amount '(1 ((shift) . 5) ((control))))

;; Display menubar when using Emacs.app
(if (display-graphic-p)
(menu-bar-mode 1))

(provide 'init-osx)

;;; init-osx.el ends here
