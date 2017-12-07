;; thanks https://www49.atwiki.jp/ntemacs/pages/19.html
(require 'dired)
(require 'ls-lisp)

;; ls-lisp を使う
(setq ls-lisp-use-insert-directory-program nil)

;; dired の並び順を explorer と同じにする
(setq ls-lisp-ignore-case t)         ; ファイル名の大文字小文字無視でソート
(setq ls-lisp-dirs-first t)          ; ディレクトリとファイルを分けて表示
(setq dired-listing-switches "-alG") ; グループ表示なし

(require 'recentf)

;; OSタイプ を調べる
(defun os-type ()
  (let ((os-type (shell-command-to-string "uname")))
    (cond ((string-match "CYGWIN" os-type)
           "win")
          ((string-match "Linux" os-type)
           "linux")
          ((string-match "Darwin" os-type)
           "mac"))))

;; OS でファイル、ディレクトリ、URL を直接開くためのコマンドを決定する
(defun os-open-command-name (os-type)
  (let ((command-name-list
         (cond ((string= "win" os-type)
                '("cygstart"))
               ((string= "linux" os-type)
                '("wslstart" "xdg-open" "gnome-open"))
               ((string= "mac" os-type)
                '("open")))))
    (catch 'loop
      (dolist (command-name command-name-list)
        (unless (string=  (shell-command-to-string
                           (concat "which " command-name " 2> /dev/null"))
                          "")
          (throw 'loop command-name))))))

;; OS で直接、ファイル、ディレクトリ、URL を開く
(defun os-open-command (filename)
  (interactive)
  (let* ((default-directory (if (file-regular-p filename)
                                (file-name-directory filename)
                              default-directory))
         (localname (if (file-remote-p filename)
                        (tramp-file-name-localname
                         (tramp-dissect-file-name filename))
                      filename))
         (os-type (os-type))
         (os-open-command-name (os-open-command-name os-type)))
    (when os-open-command-name
      (cond ((and (string= os-type "linux")
                  (not (file-remote-p default-directory)))
             ;; 以下の URL の対策を行う
             ;; http://d.hatena.ne.jp/mooz/20100915/p1
             ;; http://i-yt.info/?date=20090829#p01
             (let (process-connection-type)
               (start-process "os-open-command" nil os-open-command-name localname)))
            (t
             ;; リモートでもコマンドを実行できるように、start-process ではなく shell-command系を使う
             (shell-command-to-string (concat os-open-command-name " "
                                              (shell-quote-argument localname) " &"))))
      (message "%s" (concat os-open-command-name " " localname)))))

;; dired で W 押下時に、カーソル位置のファイルを OS で直接起動する
(define-key dired-mode-map (kbd "W")
  (lambda ()
    (interactive)
    (let ((file (dired-get-filename nil t)))
      (recentf-push file) ; recentf に追加する
      (os-open-command file))))

;; dired で E 押下時に、開いているディレクトリを OS で直接開く
(define-key dired-mode-map (kbd "E")
  (lambda ()
    (interactive)
    (os-open-command (dired-current-directory))))

;; OS で起動したいファイルの拡張子一覧
(setq os-open-file-suffixes '("doc" "docx"
                              "xls" "xlsx"
                              "ppt" "pptx"
                              "mdb" "mdbx"
                              "vsd" "vdx" "vsdx"
                              "mpp"
                              "pdf"
                              "bmp" "jpg"
                              "odt" "ott"
                              "odg" "otg"
                              "odp" "otp"
                              "ods" "ots"
                              "odf"
                              ))

;; OS で直接開きたいファイルかどうかを判定する
(defun os-open-file-p (file)
  (when (file-regular-p file)
    (let ((ext (file-name-extension file)))
      (when (and ext
                 (member (downcase ext) os-open-file-suffixes)
                 (os-open-command-name (os-type)))
        t))))

;; dired でファイルを f で開く際に os-open-file-suffixes リストに指定してあるサフィックスのファイルは、
;; OS で直接起動する
(advice-add 'find-file
            :around (lambda (orig-fun &rest args)
                      (let ((file (nth 0 args)))
                        (cond ((os-open-file-p file)
                               (recentf-push file) ; recentf に追加する
                               (os-open-command file))
                              (t
                               (apply orig-fun args))))))


;; http://skalldan.wordpress.com/2011/11/04/winodws-7-%E3%81%A7-cygwin-ntemacs-%E4%BA%8B%E5%A7%8B%E3%82%81/#sec-5

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


