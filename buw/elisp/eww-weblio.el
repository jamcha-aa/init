;; http://emacs.rubikitch.com/eww-weblio/
(require 'eww)
(defvar eww-data)
(defun eww-current-url ()
  "バージョン間の非互換を吸収する。"
  (if (boundp 'eww-current-url)
      eww-current-url                   ;emacs24.4
    (plist-get eww-data :url)))         ;emacs25

(defun eww-set-start-at (url-regexp search-regexp)
  "URL-REGEXPにマッチするURLにて、SEARCH-REGEXPの行から表示させる"
  (when (string-match url-regexp (eww-current-url))
    (goto-char (point-min))
    (when (re-search-forward search-regexp nil t)
      (recenter 0))))

(defun region-or-read-string (prompt &optional initial history default inherit)
  "regionが指定されているときはその文字列を取得し、それ以外では`read-string'を呼ぶ。"
  (if (not (region-active-p))
      (read-string prompt initial history default inherit)
    (prog1
        (buffer-substring-no-properties (region-beginning) (region-end))
      (deactivate-mark)
      (message ""))))

(defun eww-render--after (&rest _)
  (eww-set-start-at "www.weblio.jp" "^ *Weblio 辞書")
  ;; 他のサイトの設定も同様に追加できる
  ;; weblio-en-ja (added by jamcha (jamcha.aa@gmail.com))
  (eww-set-start-at "ejje.weblio.jp" "^ *Weblio 辞書")
  )
;;; [2017-01-14 Sat]バージョンごとに分岐
(if (boundp 'eww-after-render-hook)     ;25.1
    (add-hook 'eww-after-render-hook 'eww-render--after)
  (advice-add 'eww-render :after 'eww-render--after)) ;24.4

;;; weblio
(defun weblio (str)
  (interactive (list
                (region-or-read-string "Weblio: ")))
  (eww-browse-url (format "http://www.weblio.jp/content/%s"
                      (upcase (url-hexify-string str)))))
;;; wikipedia
(defun wikipedia (str)
  (interactive (list
                (region-or-read-string "Wikipedia: ")))
  (eww-browse-url (format "http://ja.wikipedia.org/wiki/%s"
                      (upcase (url-hexify-string str)))))

;;; weblio-en-ja (added by jamcha (jamcha.aa@gmail.com))
(defun ejje (str)
  (interactive (list
                (region-or-read-string "Weblio: ")))
  (eww-browse-url (format "http://ejje.weblio.jp/content/%s"
                      (upcase (url-hexify-string str)))))
