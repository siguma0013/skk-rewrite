;;; reskk-input.el --- SKK入力ロジック -*- lexical-binding: t; -*-

;;; Commentary:


;;; Code:
(require 'reskk-tree)
(require 'reskk-overlay)
(require 'reskk-dictionary)

;; かな変換中バッファ
(defvar-local reskk-convert-buffer nil)

(defun reskk-insert-hiragana ()
  "ひらがな入力コマンド"
  (interactive)
  (reskk-insert-kana
    (lambda (node)
      (insert (reskk-tree-node-value node))
      )))

(defun reskk-insert-katakana ()
  "カタカナ入力コマンド"
  (interactive)
  (reskk-insert-kana
    (lambda (node)
      ;; ひらがな => カタカナ変換のために一度変数で受ける
      (let ((value (reskk-tree-node-value node)))
        ;; 変換して出力
        (insert (reskk-convert-hiragana-to-katakana value))
        ))))

;; ひらがなorカタカナ入力関数
(defun reskk-insert-kana (call-back)
  (let* ((keycode last-command-event)       ; キーコード
          (char (char-to-string keycode))   ; 入力文字列
          (buffer (concat reskk-convert-buffer char)) ; かな変換中文字列
          (node (reskk-find-node buffer)))            ; 木構造の検索結果
    (message "HIT: %d => %s" keycode char)

    (cond
      ((null node)                      ; ノードが取得できなかったとき
        (setq-local reskk-convert-buffer char))
      ((reskk-tree-is-leaf node)        ; ノードが末端のとき
        (funcall call-back node)
        (setq-local reskk-convert-buffer (reskk-tree-node-pending node)))
      (t                                ; ノードが途中のとき
        (setq-local reskk-convert-buffer buffer))
      )
    )
  (reskk-display-overlay reskk-convert-buffer)
  )

;; ひらがなをカタカナに変換する関数
(defun reskk-convert-hiragana-to-katakana (hiragana)
  (apply 'string
    (mapcar
      (lambda (char)
        (if (and (>= char #x3041) (<= char #x3096))
          (+ char (- #x30A1 #x3041))
          char
          )
        )
      (string-to-list hiragana))
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

(provide 'reskk-input)
