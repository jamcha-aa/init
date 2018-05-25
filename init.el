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
(add-to-list 'load-path "~/.emacs.d/elisp")

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ language - fontset                                            ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

(if (eq system-type 'windows-nt) 
    (add-to-list 'default-frame-alist '(font . "Meiryo-12"))
  )

(if (eq system-type 'gnu/linux)
    (add-to-list 'default-frame-alist '(font . "源ノ角ゴシック Code JP R-12"))
  )

;; http://extra-vision.blogspot.jp/2016/07/emacs.html
;; If you want to use san-serif only for ascii, choose below.
;(create-fontset-from-ascii-font "源ノ角ゴシック Code JP R-12" nil "SourceHanMix")
;(set-fontset-font "fontset-SourceHanMix" '(?ぁ . ?ヿ) "霧明朝:Bold-14") ; Kana replace
;(add-to-list 'default-frame-alist '(font . "fontset-SourceHanMix"))

;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ screen - buffer                                               ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

;; 初期画面の非表示
(setq inhibit-startup-message nil)
(setq inhibit-startup-screen nil)

;; バッファ画面外文字の切り詰め表示
(setq truncate-lines nil)

;; ウィンドウ縦分割時のバッファ画面外文字の切り詰め表示
(setq truncate-partial-width-windows nil) ; use toggle-truncate-lines

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
;; choose this if powerline were heavy
;(if (eq system-type 'gnu/linux)
;    (load "gp-mode-line-linux"))
;    (load "wr-mode-line-linux")) ; very simple

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

;;; warm-night-mode
;(load-theme 'warm-night t) 

;;; spacemacs-dark theme
;(load-theme 'spacemacs-dark t)
;(require 'spaceline-config)
;(spaceline-spacemacs-theme)

;;; Org-mode
;;; org-mode latex
(setq org-latex-default-class "ltjsarticle")

;;; org-mode latex to pdf
(setq org-latex-pdf-process '("lualatex %b" "lualatex %b"))

(require 'ox-latex)

;; https://texwiki.texjp.org/?Emacs%2FOrg%20mode
(add-to-list 'org-latex-classes
             '("ltjsarticle"
               "\\documentclass[12pt,a4paper]{ltjsarticle}
[NO-DEFAULT-PACKAGES]
\\usepackage{amsmath}
% \\usepackage{newtxtext,newtxmath}
\\usepackage{graphicx}
\\usepackage[pdfencoding=auto]{hyperref}
\\usepackage{luatexja-fontspec}
\\setmainfont{Linux Libertine O}
\\setmainjfont{Noto Serif CJK JP}
\\setsansjfont{Noto Sans CJK JP}
\\ltjsetparameter{jacharrange={-2}}"
  ("\\section{%s}" . "\\section*{%s}")
  ("\\subsection{%s}" . "\\subsection*{%s}")
  ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
  ("\\paragraph{%s}" . "\\paragraph*{%s}")
  ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

(require 'ox-beamer)

;; https://texwiki.texjp.org/?Emacs%2FOrg%20mode
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
%\\usepackage{luacode}
%\\usepackage{luatexja-otf}
%\\usepackage[ipaex]{luatexja-preset}
\\usepackage{luatexja-fontspec}
\\setmainfont{Linux Libertine O}
\\setmainjfont{Noto Serif CJK JP}
\\setsansjfont{Noto Sans CJK JP}
\\ltjsetparameter{jacharrange={-2}}
\\renewcommand{\\kanjifamilydefault}{\\gtdefault}
%%
%\\setbeamercovered{transparent}
\\setbeamertemplate{navigation symbols}{}"
  ("\\section{%s}" . "\\section*{%s}")
  ("\\subsection{%s}" . "\\subsection*{%s}")
  ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
  ("\\paragraph{%s}" . "\\paragraph*{%s}")
  ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

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

;; require utf-8 SKK-JISYO
;(setq skk-jisyo-code 'utf-8)

;; skk hokan
(setq skk-dcomp-activate t)

;; skk-jisho
(if (eq system-type 'gnu/linux)
(setq skk-large-jisyo "/usr/share/skk/SKK-JISYO.L"))

(if (eq system-type 'windows-nt)
    (setq skk-large-jisyo "~/.emacs.d/SKK-JISYO.L"))

;; skk sticky-key
; win
;(setq skk-sticky-key [non-convert])
; linux
;(setq skk-sticky-key [muhenkan])

;; skk kutouten
(setq-default skk-kutouten-type 'jp-en)

;; skk function key
(setq skk-j-mode-function-key-usage 'conversion)

;; skk annotation
(setq skk-show-annotation t)

;; auto close brackets
(setq-default skk-auto-insert-paren t)

;; sekka
;(require 'sekka)
;(global-sekka-mode 1)
;(setq sekka-sticky-shift t)

;; mozc
;; sudo apt install emacs-mozc emacs-mozc-bin
;(require 'mozc)
;(set-language-environment "Japanese")
;(setq default-input-method "japanese-mozc")

;; org-mode & org-capture
;; https://d.hatena.ne.jp/tamura70/20100208/org
;; (save-window-excursion (shell-command (format "emacs-test -l test-minimum -l %s %s &" buffer-file-name buffer-file-name)))
(require 'org-install)
(setq org-startup-truncated nil)
(setq org-return-follows-link t)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(if (eq system-type 'gnu/linux)
    ;(setq org-directory "~/OneDrive/org/"))
    (setq org-directory "~/.emacs.d/org/"))
(if (eq system-type 'windows-nt)
    ;(setq org-directory "i:/OneDrive/org/"))
    (setq org-directory "~/.emacs.d/org/"))
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
  (if (eq system-type 'gnu/linux)
      (setq migemo-dictionary (expand-file-name "/usr/share/cmigemo/utf-8/migemo-dict")))
  (if (eq system-type 'windows-nt)
      (setq migemo-dictionary (expand-file-name "~/.emacs.d/dict/utf-8/migemo-dict")))
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
;(set-face-background 'hiwin-face "#eee8d5") ;; 非アクティブウィンドウの背景色を設定
(set-face-background 'hiwin-face "#d3d3d3") ;; 非アクティブウィンドウの背景色を設定

;; カーソルの点滅を止める
(blink-cursor-mode 0)

;; yatex
;; https://texwiki.texjp.org/?YaTeX
(setq auto-mode-alist
      (cons (cons "\\.tex$" 'yatex-mode) auto-mode-alist))
(autoload 'yatex-mode "yatex" "Yet Another LaTeX mode" t)
(setq tex-command "lualatex")
(setq YaTeX-kanji-code nil)
(setq YaTeX-latex-message-code 'utf-8)

;(setq font-lock-maximum-decoration '((c-mode . 1) (c++-mode . 1)(org-mode . 1)))

;; twittering-mode
(require 'twittering-mode)
(setq twittering-use-master-password t)

;;; eww google
(setq eww-search-prefix "https://www.google.co.jp/search?q=")

;; eww function
(setq browse-url-browser-function 'eww-browse-url)

;; eww background color rejection
(setq-local eww-disable-colorize t)

;; eww-disable-image and suppress white background
(load "eww-preferences")

;; eww set default-font
(setq shr-use-fonts nil)

;; navi2ch
(if (eq system-type 'gnu/linux)
   ; once you get board.txt, remove comment below
   ;(setq navi2ch-net-http-proxy "localhost:8080")
    (setq navi2ch-article-max-buffers 5)
    (setq navi2ch-article-auto-expunge t)
    ;; https://mevius.5ch.net/test/read.cgi/unix/1405127170/838
    (setq navi2ch-list-valid-host-regexp
          (concat "\\("
                  (regexp-opt '(".2ch.net" ".5ch.net" ".bbspink.com" ".machibbs.com" ".machi.to"))
                  "\\)\\'"))
    (setq navi2ch-list-bbstable-url "http://menu.5ch.net/bbstable.html"))

;; magit
(require 'magit)
(if (eq system-type 'windows-nt)
    (setq magit-git-executable "C:/Program Files/Git/bin/git.exe"))

;; http://www.clear-code.com/blog/2012/4/3.html
;; 文字単位での変更箇所は色を反転して強調
(set-face-attribute 'diff-refine-change nil
                    :foreground nil :background nil
                    :weight 'bold :inverse-video t)

;; diffを表示したらすぐに文字単位での強調表示も行う
(defun diff-mode-refine-automatically ()
  (diff-auto-refine-mode t))
(add-hook 'diff-mode-hook 'diff-mode-refine-automatically)

;; diffを表示しているときに文字単位での変更箇所も強調表示する
;; 'allではなくtにすると現在選択中のhunkのみ強調表示する
(setq magit-diff-refine-hunk 'all)

;(if (eq system-type 'gnu/linux)
;    (require 'exec-path-from-shell)
;  (exec-path-from-shell-copy-env "SSH_AGENT_PID")
;  (exec-path-from-shell-copy-env "SSH_AUTH_SOCK"))

;; org-markdown
(eval-after-load "org"
  '(require 'ox-md nil t))

;; org-license
(load "org-license")

;; eww-weblio
(load "eww-weblio")

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
(setq load-path
      (append '(
                "~/.emacs.d/conf"
                ) load-path))

;; conf files
(cond ((eq system-type 'gnu/linux)
       (load "mewconf")))
; if org-feeds did not get entries, try org-feeds.dispo
;(load "org-feeds.dispo")
(load "org-feeds")

;; http://suzuki.tdiary.net/20140813.html#c04
(if (eq system-type 'gnu/linux)
(when (and (fboundp 'shr-render-region)
           ;; \\[shr-render-region] requires Emacs to be compiled with libxml2.
           (fboundp 'libxml-parse-html-region))
  (setq mew-prog-text/html 'shr-render-region))) ;; 'mew-mime-text/html-w3m

;; wl
(if (eq system-type 'windows-nt)
    (autoload 'wl "wl" "Wanderlust" t)
  (autoload 'wl-draft "wl" "Write draft with Wanderlust." t)

  ;; http://www.otacky.jp/otaku_comm-14Q4.html
  (autoload 'w3m "w3m" "Interface for w3m on Emacs." t)
  (setq w3m-home-page "https://github.com/jamcha-aa")
  (setq w3m-key-binding 'info)
  (setq w3m-fill-column 80)
  
  (setq mime-setup-enable-inline-html 'shr)
  (require 'mime-setup)
  (eval-after-load 'shr
    '(defun shr-colorize-region (start end fg &optional bg)
       nil)))

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
;; http://stackoverflow.com/questions/16884377/magit-is-very-slow-when-committing-on-windows
(if (eq system-type 'windows-nt)
    (progn
      (setq exec-path (add-to-list 'exec-path "C:/Program Files/Git/bin"))
      (setenv "PATH" (concat "C:\\Program Files\\Git\\bin;" (getenv "PATH")))
      (setenv "GIT_ASKPASS" "git-gui--askpass")
      (setenv "SSH_ASKPASS" "git-gui--askpass")))

;; disable-mouse
;(require 'disable-mouse)
;(global-disable-mouse-mode)

;; like mc
(load "likemc")

;; yasnippet
;; https://gist.github.com/tado/3b032649615b75cb2d0b
;; 自分用・追加用テンプレート -> mysnippetに作成したテンプレートが格納される
(require 'yasnippet)
(setq yas-snippet-dirs
      '("~/.emacs.d/mysnippets"
        "~/.emacs.d/yasnippets"
        ))

;; https://fukuyama.co/yasnippet
;; 既存スニペットを挿入する
(define-key yas-minor-mode-map (kbd "C-x i i") 'yas-insert-snippet)
;; 新規スニペットを作成するバッファを用意する
(define-key yas-minor-mode-map (kbd "C-x i n") 'yas-new-snippet)
;; 既存スニペットを閲覧・編集する
(define-key yas-minor-mode-map (kbd "C-x i v") 'yas-visit-snippet-file)

(yas-global-mode 1)

;; https://qiita.com/sawa-@github/items/0efc5b43b78d0695eb0e
;; <C-t>でウィンドウ切り替え
(global-set-key (kbd "C-t") 'other-window)

;; https://qiita.com/sawa-@github/items/0efc5b43b78d0695eb0e
;; Dired用にウィンドウ切り替え設定
(add-hook 'dired-mode-hook
      (lambda ()
        (define-key dired-mode-map (kbd "C-t") 'other-window)))

; dired-tar
(if (eq system-type 'gnu/linux)
    (setq dired-guess-shell-gnutar "tar")
  (load "dired-tar"))

;; Suppress C-J as Enter on Windows
(if (eq system-type 'windows-nt)
    (load "newline-settings"))

;; reduce memory consumption
(setq eww-history-limit 5)

;; OS-independent fullscreen option
; (toggle-frame-fullscreen)

;; exwm
;(require 'exwm)
;(require 'exwm-config)
;(exwm-config-default)

;; exwm: show window title
;; https://github.com/ch11ng/exwm/issues/198
;(defun exwm-rename-buffer ()
;  (interactive)
;  (exwm-workspace-rename-buffer
;   (concat exwm-class-name ":"
;           (if (<= (length exwm-title) 50) exwm-title
;             (concat (substring exwm-title 0 49) "...")))))
;; Add these hooks in a suitable place (e.g., as done in exwm-config-default)
;(add-hook 'exwm-update-class-hook 'exwm-rename-buffer)
;(add-hook 'exwm-update-title-hook 'exwm-rename-buffer)

;; switch-window for exwm
;(setq switch-window-shortcut-style 'qwerty)
;(global-set-key (kbd "C-x o") 'switch-window)
