;; http://skalldan.wordpress.com/2011/11/04/winodws-7-%E3%81%A7-cygwin-ntemacs-%E4%BA%8B%E5%A7%8B%E3%82%81/#sec-5
;; https://www49.atwiki.jp/ntemacs/pages/19.html

(require 'dired)
(require 'wdired)

;; ファイルを other window にコピー
(defun dired-copy-to-other-window (arg)
  "In dired, copy selected file(s) to the other window."
  (interactive "P")
  (let ((dired-dwim-target t))
    (dired-do-copy arg)))

;; ファイルを other window に移動
(defun dired-move-to-other-window (arg)
  "In dired, rename selected file(s) to the other window."
  (interactive "P")
  (let ((dired-dwim-target t))
    (dired-do-rename arg)))

;; ファイルのシンボリックリンクを other window に作成
(defun dired-make-symlinks-to-other-window (arg)
  "In dired, make symbolic link(s) of selected file(s) to the other window."
  (interactive "P")
  (let ((dired-dwim-target t))
    (dired-do-symlink arg)))

;; キー割り当て
(define-key dired-mode-map (kbd "c") 'dired-copy-to-other-window)
(define-key dired-mode-map (kbd "r") 'dired-move-to-other-window)
(define-key dired-mode-map (kbd "l") 'dired-make-symlinks-to-other-window) ; "s" は sort で利用しているので "l" とした
(define-key dired-mode-map (kbd "e") 'wdired-change-to-wdired-mode)
