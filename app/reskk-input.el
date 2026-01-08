;;; reskk-input.el --- SKK入力ロジック -*- lexical-binding: t; -*-

;;; Commentary:


;;; Code:
(require 'reskk-tree)

(defvar-local reskk--convert-buffer nil
  "変換中バッファ")

(defun reskk-observer ()
  (interactive)

  (let* ((key last-command-event)
          (char (char-to-string key))
          (buffer (concat reskk--convert-buffer char))
          (node (reskk-find-node buffer)))
    (message "HIT: %d => %s" key char)

    (cond
      ((null node)                      ; ノードが取得できなかったとき
        (setq-local reskk--convert-buffer char))
      ((reskk-tree-is-leaf node)        ; ノード末端のとき
        (insert (reskk-tree-get-value node))
        (setq-local reskk--convert-buffer (reskk-tree-get-pending node)))
      (t                                ; ノード途中のとき
        (setq-local reskk--convert-buffer buffer))
      )
    )
  (reskk-display-overlay reskk--convert-buffer)
  )

(defun reskk-backward-char ()
  "delete-backward-charのオーバーライド関数"
  (interactive)

  (if (> (length reskk--convert-buffer) 0)
    ;; 変換中バッファに文字列がある時
    (progn
      (setq-local reskk--convert-buffer (substring reskk--convert-buffer 0 -1))
      (reskk-display-overlay reskk--convert-buffer)
      )

    ;; 変換中バッファが空の時
    (call-interactively #'delete-backward-char)
    )
  )

(provide 'reskk-input)
