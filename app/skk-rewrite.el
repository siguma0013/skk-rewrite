;;; skk-rewrite.el --- SKK 再実装 (最小構成) -*- lexical-binding: t; -*-

;; Author: Your Name
;; Version: 0.1
;; Package-Requires: ()
;; Keywords: input, japanese
;; URL: https://github.com/siguma0013/skk-rewrite

;;; Commentary:


;;; Code:
(require 'reskk-convert-rule)
(require 'reskk-overlay)

(defgroup skk-rewrite nil
  "SKK 再実装用の設定グループ."
  :group 'input)

(defcustom skk-rewrite-dummy t
  "何もしないダミー変数."
  :type 'boolean
  :group 'skk-rewrite)

;; 変換中バッファ
(defvar-local reskk-convert-buffer nil)

(defun reskk-observer ()
  (interactive)

  (let* ((key last-command-event)
          (char (char-to-string key)))
    (message "HIT: %d => %s" key char)

    ;; 変換中バッファと新しい入力を結合
    (setq-local reskk-convert-buffer (concat reskk-convert-buffer char))

    (when-let* ((node (reskk-find-node reskk-convert-buffer))
                 (is-leaf (reskk-tree-is-leaf node)))
      ;; ノードが取得可能 かつ 末端のとき
      (let ((value (reskk-tree-get-value node))
             (pending (reskk-tree-get-pending node)))

        (message "CONVERT:%s" value)
        (insert value)
        ;; 変換中バッファのリセット
        (setq-local reskk-convert-buffer pending)
        )
      )

    (reskk-display-overlay reskk-convert-buffer)
    )
  )

(defun reskk-backward-char ()
  "delete-backward-charのオーバーライド関数"
  (interactive)

  (if (> (length reskk-convert-buffer) 0)
    ;; 変換中バッファに文字列がある時
    (progn
      (setq-local reskk-convert-buffer (substring reskk-convert-buffer 0 -1))
      (reskk-display-overlay reskk-convert-buffer)
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
