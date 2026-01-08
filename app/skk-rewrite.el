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
          (char (char-to-string key))
          (buffer (concat reskk-convert-buffer char))
          (node (reskk-find-node buffer)))
    (message "HIT: %d => %s" key char)

    (cond
      ((null node)                      ; ノードが取得できなかったとき
        (setq-local reskk-convert-buffer char))
      ((reskk-tree-is-leaf node)        ; ノード末端のとき
        (insert (reskk-tree-get-value node))
        (setq-local reskk-convert-buffer (reskk-tree-get-pending node)))
      (t                                ; ノード途中のとき
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
