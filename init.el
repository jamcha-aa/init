;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ language - coding system                                      ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

;; デフォルトの文字コード
(set-default-coding-systems 'utf-8-unix)

;; テキストファイル／新規バッファの文字コード
;(if (eq system-type 'gnu/linux)
;    (set-language-environment 'Japanese)
;    (prefer-coding-system 'utf-8))
(prefer-coding-system 'utf-8-unix)

;; ファイル名の文字コード
(set-file-name-coding-system 'utf-8-unix)

;; キーボード入力の文字コード
(set-keyboard-coding-system 'utf-8-unix)

;; サブプロセスのデフォルト文字コード
(setq default-process-coding-system '(undecided-dos . utf-8-unix))

;; windows-nt or gnu/linux files
(add-to-list 'load-path "~/.emacs.d/environment")

;; win input methods
(if (eq system-type 'windows-nt)
    (load "im4win"))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ language - fontset                                            ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

(if (eq system-type 'windows-nt) 
(set-face-attribute 'default nil :family "Myrica M" :height 130) ;; デフォルト フォント
(set-face-attribute 'variable-pitch nil :family "Myrica P" :height 130) ;; プロポーショナル フォント
(set-face-attribute 'fixed-pitch nil :family "Myrica N" :height 130) ;; 等幅フォント
(set-face-attribute 'tooltip nil :family "Myrica M" :height 110) ;; ツールチップ表示フォント
)

(if (eq system-type 'gnu/linux)
(add-to-list 'default-frame-alist '(font . "源ノ角ゴシック Code JP H-16"))
;(add-to-list 'default-frame-alist '(font . "源ノ角ゴシック Heavy-16"))
;(add-to-list 'default-frame-alist '(font . "Rounded\-X Mgen+ 1c heavy-18"))
;(add-to-list 'default-frame-alist '(font . "Myrica M-16"))
)

;; 初期画面の非表示
(setq inhibit-startup-message nil)
(setq inhibit-startup-screen nil)

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ screen - mode line                                            ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

;; 行番号の表示
(line-number-mode t)

;; 列番号の表示
(column-number-mode t)

;; モードライン カスタマイズ
(if (eq system-type 'windows-nt)
    (load "mode-line-win"))
(if (eq system-type 'gnu/linux)
    (load "mode-line-linux"))

;; cp932エンコードの表記変更
(coding-system-put 'cp932 :mnemonic ?P)
(coding-system-put 'cp932-dos :mnemonic ?P)
(coding-system-put 'cp932-unix :mnemonic ?P)
(coding-system-put 'cp932-mac :mnemonic ?P)

;; UTF-8エンコードの表記変更
(coding-system-put 'utf-8 :mnemonic ?U)
(coding-system-put 'utf-8-with-signature :mnemonic ?u)

;; 改行コードの表記追加
(setq eol-mnemonic-dos       ":Dos ")
(setq eol-mnemonic-mac       ":Mac ")
(setq eol-mnemonic-unix      ":Unx ")
(setq eol-mnemonic-undecided ":??? ") 


;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ screen - buffer                                               ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

;; バッファ画面外文字の切り詰め表示
(setq truncate-lines nil)

;; ウィンドウ縦分割時のバッファ画面外文字の切り詰め表示
(setq truncate-partial-width-windows t)

;; 同一バッファ名にディレクトリ付与
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)
(setq uniquify-ignore-buffers-re "*[^*]+*")

;; IME無効／有効時のカーソルカラー定義
(unless (facep 'cursor-ime-off)
  (make-face 'cursor-ime-off)
  (set-face-attribute 'cursor-ime-off nil
                      :background "DarkRed" :foreground "White")
  )
(unless (facep 'cursor-ime-on)
  (make-face 'cursor-ime-on)
  (set-face-attribute 'cursor-ime-on nil
                      :background "DarkGreen" :foreground "White")
  )

;; IME無効／有効時のカーソルカラー設定
(add-hook
 'input-method-inactivate-hook
 '(lambda()
    (if (facep 'cursor-ime-off)
        (let ( (fg (face-attribute 'cursor-ime-off :foreground))
               (bg (face-attribute 'cursor-ime-off :background)) )
          (set-face-attribute 'cursor nil :foreground fg :background bg)
          )
      )
    )
 )
(add-hook
 'input-method-activate-hook
 '(lambda()
    (if (facep 'cursor-ime-on)
        (let ( (fg (face-attribute 'cursor-ime-on :foreground))
               (bg (face-attribute 'cursor-ime-on :background)) )
          (set-face-attribute 'cursor nil :foreground fg :background bg)
          )
      )
    )
 )

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ screen - linum                                                ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

;(require 'linum)

;; 行移動を契機に描画
;(defvar linum-line-number 0)
;(declare-function linum-update-current "linum" ())
;(defadvice linum-update-current
;    (around linum-update-current-around activate compile)
;  (unless (= linum-line-number (line-number-at-pos))
;    (setq linum-line-number (line-number-at-pos))
;    ad-do-it
;    ))

;; バッファ中の行番号表示の遅延設定
;(defvar linum-delay nil)
;(setq linum-delay t)
;(defadvice linum-schedule (around linum-schedule-around () activate)
;  (run-with-idle-timer 1.0 nil #'linum-update-current))

;; 行番号の書式
;(defvar linum-format nil)
;(setq linum-format "%5d")

;; バッファ中の行番号表示
;(global-linum-mode t)

;; 文字サイズ
;(set-face-attribute 'linum nil :height 0.75)

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ file - backup                                                 ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

;; ファイルオープン時のバックアップ（~）
(setq make-backup-files   t)  ;; 自動バックアップの実行有無
(setq version-control     t)  ;; バックアップファイルへの番号付与
(setq kept-new-versions   3)  ;; 最新バックアップファイルの保持数
(setq kept-old-versions   0)  ;; 最古バックアップファイルの保持数
(setq delete-old-versions t)  ;; バックアップファイル削除の実行有無

;; ファイルオープン時のバックアップ（~）の格納ディレクトリ
(setq backup-directory-alist
      (cons (cons "\\.*$" (expand-file-name "/tmp/emacsbk"))
            backup-directory-alist))

;; 編集中ファイルの自動バックアップ
(setq backup-inhibited nil)

;; 終了時に自動バックアップファイルを削除
(setq delete-auto-save-files nil)

;; 編集中ファイルのバックアップ
(setq auto-save-list-file-name nil)
(setq auto-save-list-file-prefix nil)

;; 編集中ファイルのバックアップ間隔（秒）
(setq auto-save-timeout 3)

;; 編集中ファイルのバックアップ間隔（打鍵）
(setq auto-save-interval 100)

;; 編集中ファイル（##）の格納ディレクトリ
(setq auto-save-file-name-transforms
      `((".*" ,(expand-file-name "/tmp/emacsbk") t)))


;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ file - lockfile                                               ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

;; ロックファイルの生成を抑止
(setq create-lockfiles nil)


;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ scroll                                                        ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

;; スクロール時のカーソル位置を維持
(setq scroll-preserve-screen-position t)

;; スクロール開始の残り行数
(setq scroll-margin 0)

;; スクロール時の行数
(setq scroll-conservatively 10000)

;; スクロール時の行数（scroll-marginに影響せず）
(setq scroll-step 0)

;; 画面スクロール時の重複表示する行数
(setq next-screen-context-lines 1)

;; キー入力中の画面更新を抑止
(setq redisplay-dont-pause t)

;; recenter-top-bottomのポジション
(setq recenter-positions '(top bottom))

;; 横スクロール開始の残り列数
(setq hscroll-margin 1)

;; 横スクロール時の列数
(setq hscroll-step 1)

;; スクロールダウン
(global-set-key (kbd "C-z") 'scroll-down)

;; バッファの最後までスクロールダウン
(defadvice scroll-down (around scroll-down activate compile)
  (interactive)
  (let (
        (bgn-num (+ 1 (count-lines (point-min) (point))))
        )
    (if (< bgn-num (window-height))
        (goto-char (point-min))
      ad-do-it) ))

;; バッファの先頭までスクロールアップ
(defadvice scroll-up (around scroll-up activate compile)
  (interactive)
  (let (
        (bgn-num (+ 1 (count-lines (point-min) (point))))
        (end-num nil)
        )
    (save-excursion
      (goto-char (point-max))
      (setq end-num (+ 1 (count-lines (point-min) (point))))
      )
    (if (< (- (- end-num bgn-num) (window-height)) 0)
        (goto-char (point-max))
      ad-do-it) ))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ package manager                                               ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/"))
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)

;; coding: utf-8
;; mode: emacs-lisp

;;; 右から左に読む言語に対応させないことで描画高速化
(setq-default bidi-display-reordering nil)

;;; splash screenを無効にする
(setq inhibit-splash-screen t)

;;; 同じ内容を履歴に記録しないようにする
(setq history-delete-duplicates t)

;; C-u C-SPC C-SPC ...でどんどん過去のマークを遡る
(setq set-mark-command-repeat-pop t)

;;; 複数のディレクトリで同じファイル名のファイルを開いたときのバッファ名を調整する
(require 'uniquify)
;; filename<dir> 形式のバッファ名にする
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)
(setq uniquify-ignore-buffers-re "*[^*]+*")

;;; ファイルを開いた位置を保存する
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file (concat user-emacs-directory "places"))

;;; 釣合う括弧をハイライトする
(show-paren-mode 1)

;;; インデントにTABを使わないようにする
(setq-default indent-tabs-mode nil)

;;; 現在行に色をつける
(global-hl-line-mode 1)

;;; ミニバッファ履歴を次回Emacs起動時にも保存する
(savehist-mode 1)

;;; シェルに合わせるため、C-hは後退に割り当てる
(global-set-key (kbd "C-h") 'delete-backward-char)

;;; モードラインに時刻を表示する
(display-time)

;;; GCを減らして軽くする
(setq gc-cons-threshold (* 10 gc-cons-threshold))

;;; ログの記録行数を増やす
(setq message-log-max 10000)

;;; 履歴をたくさん保存する
(setq history-length 1000)

;;; メニューバーとツールバーとスクロールバーを消す
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;;; solarized-light
;(load-theme 'solarized-light t)

;;; flatui
(load-theme 'flatui t) 

;;; Org-mode
(setq org-latex-classes '(("ltjsarticle"
            "\\documentclass{ltjsarticle}
\\usepackage{graphicx}
\\usepackage[unicode=true,bookmarks=true]{hyperref}
[NO-DEFAULT-PACKAGES]
[PACKAGES]
[EXTRA]"
            ("\\section{%s}" . "\\section*{%s}")
            ("\\subsection{%s}" . "\\subsection*{%s}")
            ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
            ("\\paragraph{%s}" . "\\paragraph*{%s}")
            ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
               ))

;;; org-mode latex
(setq org-latex-default-class "ltjsarticle")

;;; org-mode latex to pdf
(setq org-latex-pdf-process '("lualatex %b" "lualatex %b"))

(require 'ox-latex)

(add-to-list 'org-latex-classes
             '("ltjsarticle"
               "\\documentclass[12pt,a4paper]{ltjsarticle}
[NO-DEFAULT-PACKAGES]
\\usepackage{amsmath}
% \\usepackage{newtxtext,newtxmath}
\\usepackage{graphicx}
\\hypersetup{pdfencoding=auto}"
  ("\\section{%s}" . "\\section*{%s}")
  ("\\subsection{%s}" . "\\subsection*{%s}")
  ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
  ("\\paragraph{%s}" . "\\paragraph*{%s}")
  ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))



(require 'ox-beamer)

(add-to-list 'org-latex-classes
             '("beamer"
               "\\documentclass[presentation,12pt]{beamer}
[NO-DEFAULT-PACKAGES]
\\usepackage{luatexja}
\\usepackage{hyperref}
\\hypersetup{pdfencoding=auto, linkbordercolor={0 1 0}}
%% Fonts
% mathematical font
\\usepackage{fontspec}
% Japanese
\\usepackage{luacode}
\\usepackage{luatexja-otf}
\\usepackage[ipaex]{luatexja-preset}
\\renewcommand{\\kanjifamilydefault}{\\gtdefault}
%%
%\\setbeamercovered{transparent}
\\setbeamertemplate{navigation symbols}{}"
  ("\\section{%s}" . "\\section*{%s}")
  ("\\subsection{%s}" . "\\subsection*{%s}")
  ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
  ("\\paragraph{%s}" . "\\paragraph*{%s}")
  ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

;;; eww google
(setq eww-search-prefix "https://www.google.co.jp/search?q=")

;; ----------------------------------------
;; ミニバッファで日本語入力可にする
;; NTEmacs 23.3.92以降で有効、らしい
(if (eq system-type 'windows-nt) 
(defun w32-isearch-update ()
  (interactive)
  (isearch-update))
(define-key isearch-mode-map [compend] 'w32-isearch-update)
(define-key isearch-mode-map [kanji] 'isearch-toggle-input-method)

(add-hook 'isearch-mode-hook
          (lambda () (setq w32-ime-composition-window (minibuffer-window))))
(add-hook 'isearch-mode-end-hook
          (lambda () (setq w32-ime-composition-window nil))))

;; skk
;; sudo apt install ddskk skkdic for linux
(require 'skk-auto)
(global-set-key (kbd "C-x C-j") 'skk-mode)

;; skk hokan
(setq skk-dcomp-activate t)

;; skk-jisho
(if (eq system-type 'gnu/linux)
(setq skk-large-jisyo "/usr/share/skk/SKK-JISYO.L"))

(if (eq system-type 'windows-nt)
    (setq skk-large-jisyo "~/.emacs.d/SKK-JISYO.L"))

;; skk sticky-key
(setq skk-sticky-key ";")

;; skk kutouten
(setq-default skk-kutouten-type 'jp-en)

;; mozc
;; sudo apt install emacs-mozc emacs-mozc-bin
;(require 'mozc)
;(set-language-environment "Japanese")
;(setq default-input-method "japanese-mozc")

;; org-mode & org-capture
;; (save-window-excursion (shell-command (format "emacs-test -l test-minimum -l %s %s &" buffer-file-name buffer-file-name)))
(require 'org-install)
(setq org-startup-truncated nil)
(setq org-return-follows-link t)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(if (eq system-type 'gnu/linux)
    (setq org-directory "~/OneDrive/org/"))
(if (eq system-type 'windows-nt)
    (setq org-directory "i:/OneDrive/org/"))
(setq org-default-notes-file (concat org-directory "agenda.org"))
;; アジェンダ表示の対象ファイル
(setq org-agenda-files (list org-directory))
;;(setq hl-line-face 'underline)
;; 標準の祝日を利用しない
(setq calendar-holidays nil)
;;org-capture
(setq org-capture-templates
      '(("t" "Todo" entry
         (file+headline nil "Inbox")
         "** TODO %?\n   %i\n   %a\n") ;; todoに%tは要らない
        ("b" "Bug" entry
         (file+headline nil "Inbox")
         "** TODO %?   :bug:\n   %i\n   %a\n   %t")
        ("n" "Memo" entry
         (file+datetree "text.org")
         "** %U %?\n   %i\n   %a\n")))
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c b") 'org-iswitchb)

;; ----------------------------------------
;; MIGEMO
(when (and (executable-find "cmigemo")
           (require 'migemo nil t))
  (setq migemo-command "cmigemo")
  (setq migemo-options '("-q" "--emacs" "-i" "\g"))
  ;; (setq migemo-options '("-q" "--emacs" "-i" "\a"))
  ;; (setq migemo-options '("-q" "--emacs"))

  ;; migemo-dict のパスを指定
  ;; (setq migemo-dictionary "C~/.emacs.d/migemo-dict/utf-8")
  (setq migemo-dictionary (expand-file-name "~/.emacs.d/dict/utf-8/migemo-dict"))
  (setq migemo-user-dictionary nil)
  (setq migemo-regex-dictionary nil)
  
  ;; 辞書の文字コードを指定．
  (setq migemo-coding-system 'utf-8-unix)
  ;; (setq migemo-coding-system 'euc-jp-unix)
  
  ;; キャッシュ機能を利用する
  (setq migemo-use-pattern-alist t)
  (setq migemo-use-frequent-pattern-alist t)
  (setq migemo-pattern-alist-length 1024)

  (load-library "migemo")
  
  ;; 起動時に初期化も行う
  (migemo-init)

  ;; emacs 24.3 で C-s を働かせるための設定
  (setq search-whitespace-regexp nil)
  
  ;; (eval-after-load "migemo"
  ;;   '(defadvice isearch-search (around migemo-search-ad activate)
  ;; 	 "adviced by migemo."
  ;; 	 (let ((saved-isearch-lax-whitespace isearch-lax-whitespace))
  ;; 	   (when migemo-isearch-enable-p
  ;; 		 (setq migemo-do-isearch t)
  ;; 		 (setq isearch-lax-whitespace nil))
  ;; 	   (unwind-protect
  ;; 		   ad-do-it
  ;; 		 (setq migemo-do-isearch nil)
  ;; 		 (setq isearch-lax-whitespace saved-isearch-lax-whitespace))))
  ;;   )
  
  ;; 起動時に off にする
  ;; (setq migemo-isearch-enable-p nil)
  )

;;; hiwin-mode
(hiwin-activate)                           ;; hiwin-modeを有効化
(set-face-background 'hiwin-face "#eee8d5") ;; 非アクティブウィンドウの背景色を設定

;; カーソルの点滅を止める
(blink-cursor-mode 0)

;; yatex
(setq auto-mode-alist
      (cons (cons "\\.tex$" 'yatex-mode) auto-mode-alist))
(autoload 'yatex-mode "yatex" "Yet Another LaTeX mode" t)
(setq tex-command "lualatex")
(setq YaTeX-kanji-code nil)
(setq YaTeX-latex-message-code 'utf-8)

;; org lighter
;(setq jit-lock-context-time 5.0)
;(setq jit-lock-defer-time 1.0)
;;(setq jit-lock-stealth-time ;5.0
                                        ;20.0)
;(setq jit-lock-stealth-time 20.0)
;(setq jit-lock-stealth-verbose nil)
;(setq jit-lock-stealth-nice 0.5)
;;(setq jit-lock-stealth-load ;50
;300)
;;(setq jit-lock-chunk-size ;100
;20)
;(setq jit-lock-stealth-load 300)
;(setq jit-lock-chunk-size 20)


;(setq font-lock-maximum-decoration '((c-mode . 1) (c++-mode . 1)(org-mode . 1)))

;; wl
;(require 'mime-setup)
;(autoload 'wl "wl" "Wanderlust" t)
;(autoload 'wl-draft "wl" "Write draft with Wanderlust." t)

;; twittering-mode
(require 'twittering-mode)
(setq twittering-use-master-password t)

;; eww function
(setq browse-url-browser-function 'eww-browse-url)

;; navi2ch
(if (eq system-type 'gnu/linux)
    (setq navi2ch-net-http-proxy "localhost:8080"))

;; magit
(require 'magit)

;; org-markdown
(eval-after-load "org"
  '(require 'ox-md nil t))

;; org-license
(add-to-list 'load-path "~/.emacs.d/elisp")
(load "org-license")
(load "xah-dicts")

;; dict tools
(setq xah-lookup-browser-function 'eww)

;; emms
(cond ((eq system-type 'gnu/linux)
  (require 'emms-setup)
  (emms-standard)
  (emms-default-players)))

;; org-octopress
;(require 'org-octopress)

;; mew
(if (eq system-type 'gnu/linux)
    (load "mew"))

;; conf el
(if (eq system-type 'gnu/linux)
    (setq load-path
          (append '(
                    "~/.emacs.d/conf"
                    ) load-path)))

;; conf files
(cond ((eq system-type 'gnu/linux)
  (load "org-feeds")
  (load "mewconf")))

;; Helm
(setq dired-bind-jump nil) ;;skkとの競合を回避する
(require 'helm-config)
(helm-mode 1)

;; helm ミニバッファでC-hをバックスペースに割り当て
(define-key helm-read-file-map (kbd "C-h") 'delete-backward-char)

;; helm TABで補完
(define-key helm-read-file-map (kbd "<tab>") 'helm-execute-persistent-action)

;; helm-migemo
;(require 'helm-migemo)

;; helm-swoop
;(require 'helm-swoop)

;; win git settings
(if (eq system-type 'windows-nt)
    (progn
      (setq exec-path (add-to-list 'exec-path "I:/Program Files/Git/bin"))
      (setenv "PATH" (concat "I:\\Program Files\\Git\\bin;" (getenv "PATH")))))
(setenv "GIT_ASKPASS" "git-gui--askpass")

