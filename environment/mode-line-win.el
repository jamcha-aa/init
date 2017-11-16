;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;
;;; @ language - input method                                       ;;;
;;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ;;;

;; IME初期化
(w32-ime-initialize)

;; デフォルトIME
(setq default-input-method "W32-IME")

;; IME変更
(global-set-key (kbd "C-\\") 'toggle-input-method)
(global-set-key [kanji] 'toggle-input-method)

;; 漢字/変換キー入力時のエラーメッセージ抑止
(global-set-key (kbd "<A-kanji>") 'ignore)
(global-set-key (kbd "<M-kanji>") 'ignore)
;(global-set-key (kbd "<kanji>") 'ignore)

;; ime on/off color
(add-hook 'w32-ime-on-hook (function (lambda () (set-cursor-color "#8b0000"))))
(add-hook 'w32-ime-off-hook (function (lambda () (set-cursor-color "#34495e"))))

(require 'powerline)
(powerline-default-theme)

(set-face-attribute 'powerline-active1 nil
                    :foreground "#0a74b9"
                    :background "#2c3e50"
                    :inherit 'mode-line)
(set-face-attribute 'powerline-active2 nil
                    :foreground "#e67e22"
                    :background "#34495e"
                    :inherit 'mode-line)
(set-face-attribute 'powerline-inactive1 nil
                    :foreground "#7f8c8d"
                    :background "#34495e"
                    :inherit 'mode-line)
(set-face-attribute 'powerline-inactive2 nil
                    :foreground "#95a5a6"
                    :background "#2c3e50"
                    :inherit 'mode-line)

;; バッファ切り替え時の状態引継ぎ設定
(setq w32-ime-buffer-switch-p nil)

