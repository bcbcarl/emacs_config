(defconst *is-a-mac* (eq system-type 'darwin))

(when *is-a-mac*
  ;; Make mouse wheel / trackpad scrolling less jerky
  (setq mouse-wheel-scroll-amount '(1
                                    ((shift) . 5)
                                    ((control))))
  ;; Display menubar when using Emacs.app
  (if (display-graphic-p)
    (menu-bar-mode 1)))

(provide 'init-osx)
