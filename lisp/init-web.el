(require 'php-mode)
(eval-after-load 'php-mode
  '(require 'php-ext))

(require 'php-auto-yasnippets)
(setq php-auto-yasnippet-php-program "~/.emacs.d/elpa/php-auto-yasnippets-20141128.1411/Create-PHP-YASnippet.php")
(define-key php-mode-map (kbd "C-c C-y") 'yas/create-php-snippet)

(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsx?\\'" . js2-jsx-mode))

(defun my-web-mode-hook ()
  "Hooks for Web mode."
  (setq-default indent-tabs-mode nil)
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-indent-style 2)
)

(defun my-js2-mode-hook ()
  "Hooks for JS2 mode."
  (setq-default indent-tabs-mode nil)
  (setq-default js2-basic-offset 2)
  (setq-default js2-indent-line 2)
)

(add-hook 'web-mode-hook 'my-web-mode-hook)
(add-hook 'web-mode-hook 'emmet-mode)
(add-hook 'web-mode-hook 'ac-emmet-html-setup)
(add-hook 'css-mode-hook 'emmet-mode)
(add-hook 'css-mode-hook 'ac-emmet-css-setup)
(add-hook 'js2-mode-hook 'my-js2-mode-hook)
(add-hook 'js2-mode-hook #'js2-refactor-mode)

(js2r-add-keybindings-with-prefix "C-c C-m")

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

(provide 'init-web)