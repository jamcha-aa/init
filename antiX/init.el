(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(blink-cursor-mode -1)

(add-to-list 'default-frame-alist '(font . "Noto Sans CJK JP Bold-12"))

;;; シェルに合わせるため、C-hは後退に割り当てる
(global-set-key (kbd "C-h") 'delete-backward-char)

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/"))
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; magit
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'magit)

;; http://www.clear-code.com/blog/2012/4/3.html
;; 文字単位での変更箇所は色を反転して強調
(set-face-attribute 'diff-refine-changed nil
                    :foreground nil :background nil
                    :weight 'bold :inverse-video t)

;; diffを表示したらすぐに文字単位での強調表示も行う
(defun diff-mode-refine-automatically ()
  (diff-auto-refine-mode t))
(add-hook 'diff-mode-hook 'diff-mode-refine-automatically)

;; diffを表示しているときに文字単位での変更箇所も強調表示する
;; 'allではなくtにすると現在選択中のhunkのみ強調表示する
(setq magit-diff-refine-hunk 'all)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; skk
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; sudo apt install ddskk skkdic for linux
(require 'skk-auto)
(global-set-key (kbd "C-x C-j") 'skk-mode)

;; require utf-8 SKK-JISYO
;(setq skk-jisyo-code 'utf-8)

;; skk sticky-key 
(setq skk-sticky-key [muhenkan])  
;; skk hokan
(setq skk-dcomp-activate t)

(setq skk-large-jisyo "~/.emacs.d/SKK-JISYO.L")

;; skk function key
(setq skk-j-mode-function-key-usage 'conversion)

;; skk annotation
(setq skk-show-annotation t)

;; auto close brackets
(setq-default skk-auto-insert-paren t)


(setq x-select-enable-clipboard t)

;;; wc-mode  
(require 'wc-mode)  

;; Suggested setting  
(global-set-key "\C-cw" 'wc-mode)  
