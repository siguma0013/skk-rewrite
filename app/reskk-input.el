;;; reskk-input.el --- SKK入力ロジック -*- lexical-binding: t; -*-

;;; Commentary:


;;; Code:
(require 'reskk-tree)
(require 'reskk-overlay)
(require 'reskk-dictionary)
(require 'reskk-state)

;; かな漢字変換中バッファ
(defvar-local reskk-convert-kanji-buffer nil)

;; フラグメントバッファ
(defvar-local reskk-input-fragment nil)

(defun reskk-insert ()
  "単純変換入力コマンド"
  (interactive)
  (let* ((keycode last-command-event)
          (char (char-to-string keycode))
          (node (reskk-find-node char)))
    ;; バッファを完全リセット
    (setq reskk-input-fragment nil)
    (reskk-display-overlay-fragment nil)
    (if node
      ;; ノードが取得できたとき
      (insert (reskk-tree-node-value node))
      ;; ノードが取得できなかったとき
      (insert char))
    ))

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
          (fragment (concat reskk-input-fragment char)) ; かな変換中文字列
          (node (reskk-find-node fragment))) ; 木構造の検索結果
    (message "HIT: %d => %s" keycode char)

    (setq reskk-input-fragment
      (cond
        ((null node)                    ; ノードが取得できなかったとき
          char)
        ((reskk-tree-is-leaf node)      ; ノードが末端のとき
          (funcall call-back node)
          (reskk-tree-node-pending node))
        (t                              ; ノードが途中のとき
          fragment)))
    )

  (reskk-display-overlay-fragment reskk-input-fragment)
  )

(defun reskk-insert-convert ()
  (interactive)
  (let* ((keycode last-command-event)   ; キーコード
          (char (char-to-string (reskk-convert-upper-to-lower keycode))) ; 入力文字列
          (fragment (concat (reskk-get-overlay-fragment) char)) ; かな変換中文字列
          (node (reskk-find-node fragment))) ; 木構造の検索結果

    (setq reskk-input-fragment
      (cond
        ((null node)       ; ノードが取得できなかったとき
          char)
        ((reskk-tree-is-leaf node)      ; ノードが末端のとき
          (insert (reskk-tree-node-value node))
          (reskk-tree-node-pending node))
        (t                   ; ノードが途中のとき
          fragment)))
    )

  (reskk-display-overlay-fragment reskk-input-fragment)
  (reskk-display-overlay-marker (reskk-get-marker))
  )

(defun reskk-insert-convert-start ()
  (interactive)
  ;; 試作のため、固定で差し替え
  (let* ((kana (buffer-substring-no-properties (reskk-get-overlay-start) (point)))
          (kanji "漢字"))
    (message "%s" kanji)
    (setq reskk-convert-kanji-buffer kanji)

    (reskk-display-overlay-fragment nil)
    (reskk-display-overlay-marker (reskk-get-marker))
    (reskk-display-overlay-option kanji)
    )
  )

(defun reskk-insert-convert-confirm ()
  (interactive)

  (let* ((end-marker (copy-marker (reskk-get-overlay-start))))
    ;; カーソルの挙動定義
    (set-marker-insertion-type end-marker t)

    ;; 文字列置換
    (replace-region-contents (reskk-get-overlay-start) (point)
      (lambda ()
        (set-marker end-marker (point))
        reskk-convert-kanji-buffer))
    ;; カーソル移動
    (goto-char end-marker))

  (setq reskk-convert-kanji-buffer nil)

  (reskk-reset-convert-state)
  (reskk-reset-overlay)
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

;; アルファベットの大文字を小文字に変換する関数
(defun reskk-convert-upper-to-lower (char)
  (if (and (>= char ?A) (<= char ?Z))
    (+ char (- ?a ?A))
    char))

(defun reskk-backward-char ()
  "delete-backward-charのオーバーライド関数"
  (interactive)

  (if (> (length reskk-input-fragment) 0)
    ;; 変換中バッファに文字列がある時
    (let* ((fragment (substring reskk-input-fragment 0 -1)))
      (setq reskk-input-fragment fragment)
      (reskk-display-overlay-fragment reskk-input-fragment))
    ;; 変換中バッファが空の時
    (call-interactively #'delete-backward-char)
    )
  )

;; 変換中バッファ削除関数
(defun reskk-clear-buffer ()
  (setq reskk-input-fragment nil)
  (reskk-display-overlay-fragment nil)
  )

(provide 'reskk-input)
