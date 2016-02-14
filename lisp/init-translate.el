;;; init-translate.el --- init file for google-translate

;;; Commentary:

;; Google translate

;;; Code:

(global-set-key (kbd "C-c t") 'google-translate-smooth-translate)

(defvar google-translate-enable-ido-completion t)
(defvar google-translate-translation-directions-alist
      '(("en" . "zh-TW") ("zh-TW" . "en") ("zh-TW" . "zh-CN")))
(defvar google-translate-minibuffer-keymap
      (let ((map (make-sparse-keymap)))
	(define-key map (kbd "C-k") 'google-translate-previous-translation-direction)
	(define-key map (kbd "C-j") 'google-translate-next-translation-direction)
	(define-key map (kbd "C-l") 'google-translate-clear-minibuffer)
	(set-keymap-parent map minibuffer-local-map)
	map))

(provide 'init-translate)

;;; init-translate.el ends here