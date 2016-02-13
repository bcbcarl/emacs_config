;;; init-clojure.el --- init file for Clojure

;;; Commentary:

;; Clojure environment

;;; Code:

(defun my-clojure-mode-hook ()
  "My clojure-mode hook."
  (clj-refactor-mode 1)
  (yas-minor-mode 1) ; for adding require/use/import statements
  ;; This choice of keybinding leaves cider-macroexpand-1 unbound
  (cljr-add-keybindings-with-prefix "C-c C-m")
  (enable-paredit-mode))

(defun my-cider-repl-mode-hook ()
  "My cider-repl-mode hook."
  (enable-paredit-mode))

(add-hook 'clojure-mode-hook #'my-clojure-mode-hook)
(add-hook 'cider-repl-mode-hook #'my-cider-repl-mode-hook)

(with-eval-after-load 'evil
  (evil-set-initial-state 'cider-repl-mode 'emacs))

(provide 'init-clojure)

;;; init-clojure.el ends here