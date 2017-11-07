;; http://emacs.rubikitch.com/smart-newline/
(require 'smart-newline)
(global-set-key (kbd "C-m") 'smart-newline)
(add-hook 'ruby-mode-hook 'smart-newline-mode)
(add-hook 'emacs-lisp-mode-hook 'smart-newline-mode)
(add-hook 'org-mode-hook 'smart-newline-mode)

(defadvice smart-newline (around C-u activate)
  "C-uを押したら元のC-mの挙動をするようにした。
org-modeなどで活用。"
  (if (not current-prefix-arg)
      ad-do-it
    (let (current-prefix-arg)
      (let (smart-newline-mode)
        (call-interactively (key-binding (kbd "C-m")))))))

