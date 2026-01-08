;;; reskk-input.el --- SKK入力ロジック -*- lexical-binding: t; -*-

;;; Commentary:


;;; Code:
(require 'reskk-tree)
(require 'reskk-overlay)

;; かな変換中バッファ
(defvar-local reskk-convert-buffer nil)

(defun reskk-observer ()
  (interactive)

  (let* ((key last-command-event)       ; キーコード
          (char (char-to-string key))   ; 入力文字列
          (buffer (concat reskk-convert-buffer char)) ; かな変換中文字列
          (node (reskk-find-node buffer)))            ; 木構造の検索結果
    (message "HIT: %d => %s" key char)

    (cond
      ((null node)                      ; ノードが取得できなかったとき
        (setq-local reskk-convert-buffer char))
      ((reskk-tree-is-leaf node)        ; ノードが末端のとき
        (insert (reskk-tree-node-value node))
        (setq-local reskk-convert-buffer (reskk-tree-node-pending node)))
      (t                                ; ノードが途中のとき
        (setq-local reskk-convert-buffer buffer))
      )
    )
  (reskk-display-overlay reskk-convert-buffer)
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

(provide 'reskk-input)
