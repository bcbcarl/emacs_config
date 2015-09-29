(require 'php-mode)

(eval-after-load 'php-mode
  '(require 'php-ext))

(require 'php-auto-yasnippets)
(setq php-auto-yasnippet-php-program "~/.emacs.d/elpa/php-auto-yasnippets-20141128.1411/Create-PHP-YASnippet.php")
(define-key php-mode-map (kbd "C-c C-y") 'yas/create-php-snippet)

(provide 'init-php)
