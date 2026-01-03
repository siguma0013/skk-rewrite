;;; skk-rewrite.el --- SKK 再実装 (最小構成) -*- lexical-binding: t; -*-

;; Author: Your Name
;; Version: 0.1
;; Package-Requires: ()
;; Keywords: input, japanese
;; URL: https://github.com/siguma0013/skk-rewrite

;;; Commentary:


;;; Code:
(require 'reskk-convert-rule)

(defgroup skk-rewrite nil
  "SKK 再実装用の設定グループ."
  :group 'input)

(defcustom skk-rewrite-dummy t
  "何もしないダミー変数."
  :type 'boolean
  :group 'skk-rewrite)

;; 変換中バッファ
(defvar-local reskk-convert-buffer nil)
;; 変換中オーバーレイ
(defvar-local reskk-overlay nil)

(defun reskk-observer ()
  (interactive)

  (let* ((key last-command-event)
          (char (char-to-string key)))
    (message "HIT: %d => %s" key char)

    ;; 変換中バッファと新しい入力を結合
    (setq-local reskk-convert-buffer (concat reskk-convert-buffer char))

    ;; 変換テーブルを検索
    (setq-local hiragana (cdr (assoc reskk-convert-buffer reskk-hiragana-convert-table)))

    (if hiragana
      ;; マッチ時
      (progn
        (message "CONVERT:%s" hiragana)
        (insert hiragana)
        ;; 変換中バッファのリセット
        (setq-local reskk-convert-buffer nil)
        )
      )

    ;; 変換中オーバーレイ更新のために一旦削除
    (when reskk-overlay
      (delete-overlay reskk-overlay)
      )

    (when reskk-convert-buffer
      (setq-local reskk-overlay (make-overlay (point) (point)))
      (setq-local styled-text (propertize reskk-convert-buffer
                                'face '(:background "orange" :foreground "red" :weight bold)))
      (overlay-put reskk-overlay 'after-string styled-text)
      )
    )
  )

(defun reskk-backward-char ()
  "delete-backward-charのオーバーライド関数"
  (interactive)

  (if reskk-convert-buffer
    ;; 変換中バッファに文字列がある時
    (progn
      (setq-local reskk-convert-buffer (substring reskk-convert-buffer 0 -1))
      ;; 変換中オーバーレイ更新のために一旦削除
      (when reskk-overlay
        (delete-overlay reskk-overlay)
        )

      (setq-local reskk-overlay (make-overlay (point) (point)))
      (setq-local styled-text (propertize reskk-convert-buffer
                                'face '(:background "orange" :foreground "red" :weight bold)))
      (overlay-put reskk-overlay 'after-string styled-text)
      )

    ;; 変換中バッファが空の時
    (call-interactively #'delete-backward-char)
    )
  )

(defvar reskk-capture-map
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap [remap self-insert-command] #'reskk-observer)

    ;; 削除系コマンドのオーバーライド
    (define-key keymap [remap delete-backward-char] #'reskk-backward-char)
    (define-key keymap [remap backward-delete-char-untabify] #'reskk-backward-char)
    keymap)
  "コマンド以外を捕捉するキーマップ")

;;;###autoload
(defun skk-rewrite-start ()
  "SKK 再実装を開始します (現在は何もしない)."
  (interactive)
  (message "skk-rewrite loaded!")
  )

(define-minor-mode reskk-mode
  ""
  :lighter "RE:SKK"
  :keymap reskk-capture-map
  (setq reskk-convert-buffer "")
  )

(provide 'skk-rewrite)

;;; skk-rewrite.el ends here
