;;; reskk-input.el --- SKK入力ロジック -*- lexical-binding: t; -*-

;;; Commentary:


;;; Code:
(require 'reskk-tree)
(require 'reskk-overlay)
(require 'reskk-dictionary)
(require 'reskk-state)

;; かな漢字変換中バッファ
(defvar-local reskk-convert-kanji-buffer nil)

;; 漢字変換選択肢リスト
(defvar-local reskk-input-options nil)

;; 漢字変換選択肢インデックス
(defvar-local reskk-input-option-index nil)

;; 漢字変換選択肢
(defvar-local reskk-input-option nil)

;; フラグメントバッファ
(defvar-local reskk-input-fragment nil)

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

;; オーバーレイ更新処理
(defun reskk-update-overlay ()
  (reskk-display-overlay-marker (reskk-get-marker))

  (reskk-display-overlay-option reskk-input-option)

  (let* ((fragment (concat (reskk-get-separater) reskk-input-fragment)))
    (message "DISPLAY-FRAGMENT:%s" fragment)
    ;; 表示するfragmentがseparaterと一緒だったら表示すべきものはない
    (reskk-display-overlay-fragment (if (string= fragment (reskk-get-separater)) nil fragment)))
  )

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

  (reskk-update-overlay)
  )

;; 変換モード向け入力関数
(defun reskk-convert-insert ()
  (interactive)
  (message "[Re:skk-mode] reskk-convert-insert")

  (let* ((char (char-to-string last-command-event))
          (old-fragment reskk-input-fragment)
          (new-fragment (concat reskk-input-fragment char))
          (node (reskk-find-node new-fragment)))

    (message "OLD-FRAGMENT:%s" old-fragment)
    (message "NEW-FRAGMENT:%s" new-fragment)

    (setq reskk-input-fragment
      (cond
        ((null node)                    ; ノードが取得できなかったとき
          char)
        ((reskk-tree-is-leaf node)      ; ノードが末端のとき
          (insert (reskk-tree-node-value node))
          (reskk-tree-node-pending node))
        (t                              ; ノードが途中のとき
          new-fragment)))

    (when-let* ((is-auto-convert (and (reskk-is-convert-okurigana) (null reskk-input-fragment)))
                 (display-word (buffer-substring-no-properties (reskk-get-overlay-start) (point)))
                 (search-word (concat (substring display-word 0 -1) old-fragment)))
      ;; 自動変換のタイミング
      (message "AUTO-CONVERT")
      (message "DISPLAY-WORD:%s" display-word)
      (message "SEARCH-WORD:%s" search-word)

      ;; 状態遷移
      (reskk-state-select-event)

      ;; 辞書検索
      (setq reskk-input-options (reskk-search-dictionary search-word))

      (when reskk-input-options
        ;; 変換リストのインデックスの初期化
        (setq reskk-input-option-index 0)
        (setq reskk-input-option
          (concat
            (nth reskk-input-option-index reskk-input-options)
            (substring display-word -1 (length display-word))
            )
          )
        )
      )
    )

  (reskk-update-overlay)
  )

(defun reskk-shift-insert ()
  (interactive)
  (message "[Re:skk-mode] reskk-shift-insert")

  (reskk-state-shift-event)

  (let* ((keycode last-command-event)       ; キーコード
          (char (char-to-string (reskk-convert-upper-to-lower keycode))) ; 入力文字列
          (fragment (concat reskk-input-fragment char)) ; かな変換中文字列
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

    (message "KEYCODE:%s" keycode)
    (message "CHAR:%s" char)
    (message "FRAGMENT:%s" reskk-input-fragment)
    )

  (reskk-update-overlay)
  )

(defun reskk-insert-convert-start ()
  (interactive)
  ;; 試作のため、固定で差し替え
  (let* ((kana (buffer-substring-no-properties (reskk-get-overlay-start) (point)))
          (kanji "漢字"))
    (message "%s" kanji)
    (setq reskk-input-fragment nil)
    (setq reskk-convert-kanji-buffer kanji)

    (reskk-update-overlay)
    (reskk-display-overlay-option kanji)
    )
  )

;; 漢字変換確定関数
(defun reskk-insert-convert-confirm ()
  (interactive)

  (let* ((end-marker (copy-marker (reskk-get-overlay-start))))
    ;; カーソルの挙動定義
    (set-marker-insertion-type end-marker t)

    ;; 文字列置換
    (replace-region-contents (reskk-get-overlay-start) (reskk-get-overlay-end)
      (lambda ()
        (set-marker end-marker (reskk-get-overlay-end))
        reskk-input-option))
    ;; カーソル移動
    (goto-char end-marker))

  (setq reskk-input-option nil)

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
      (reskk-update-overlay))
    ;; 変換中バッファが空の時
    (call-interactively #'delete-backward-char)
    )
  )

;; 変換中バッファ削除関数
(defun reskk-clear-buffer ()
  (setq reskk-input-fragment nil)
  (reskk-update-overlay)
  )

(provide 'reskk-input)
