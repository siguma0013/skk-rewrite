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
(require 'reskk-state)

(defgroup skk-rewrite nil
  "SKK 再実装用の設定グループ."
  :group 'input)

(defcustom skk-rewrite-dummy t
  "何もしないダミー変数."
  :type 'boolean
  :group 'skk-rewrite)

;; 変換中バッファ
(defvar-local reskk-convert-buffer nil)

;; HALF-ALPHABET:半角英数
;; FULL-ALPHABET:全角英数
;; HIRAGANA:ひらがな
;; KATAKANA:カタカナ
(defvar-local reskk-state 'HALF-ALPHABET
  "SKKモード")

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

(defun reskk--mode-line ()
  "モードライン文字列決定関数"
  (when reskk-mode
    (pcase reskk-state
      ('HALF-ALPHABET " SKK[aA]")
      ('FULL-ALPHABET " SKK[Ａ]")
      ('HIRAGANA " SKK[あ]")
      ('KATAKANA " SKK[ア]"))))

(declare-function reskk-activate-hiragana nil)
(declare-function reskk-activate-half-alphabet nil)

(defvar-keymap reskk-half-alphabet-keymap
  :doc "SKKモードが半角英数モードの際に使用するキーマップ"
  "C-j" #'reskk-activate-hiragana)

(defvar-keymap reskk-hiragana-keymap
  :doc "SKKモードがひらがなモードの際に使用するキーマップ"
  "C-j" #'reskk-activate-half-alphabet)

(define-key reskk-hiragana-keymap [remap self-insert-command] #'reskk-observer)
;; 削除系コマンドのオーバーライド
(define-key reskk-hiragana-keymap [remap delete-backward-char] #'reskk-backward-char)
(define-key reskk-hiragana-keymap [remap backward-delete-char-untabify] #'reskk-backward-char)

(defun reskk--keymap ()
  "キーマップ決定関数"
  (pcase reskk-state
    ('HALF-ALPHABET reskk-half-alphabet-keymap)
    ('HIRAGANA reskk-hiragana-keymap)))

(defun reskk--update-keymap ()
  "キーマップ更新関数"
  (let ((entry (assq 'skk-rewrite-mode minor-mode-overriding-map-alist)))
    ;; minor-mode-overriding-map-alistに〜
    (if entry
      ;; 登録済のとき
      (setcdr entry (reskk--keymap))
      ;; 未登録のとき
      (push `(reskk-mode . ,(reskk--keymap)) minor-mode-overriding-map-alist)
      )
    )
  )

(defun reskk--set-state (state)
  "SKKモード変更関数"
  (setq-local reskk-state state)
  (reskk--update-keymap)
  (force-mode-line-update))

(defun reskk-activate-half-alphabet ()
  "SKKモードを半角英数モードに変更するコマンド"
  (interactive)
  (reskk--set-state 'HALF-ALPHABET))

(defun reskk-activate-hiragana ()
  "SKKモードをひらがなモードに変更するコマンド"
  (interactive)
  (reskk--set-state 'HIRAGANA))

;;;###autoload
(defun skk-rewrite-start ()
  "SKK 再実装を開始します (現在は何もしない)."
  (interactive)
  (message "skk-rewrite loaded!")
  )

(define-minor-mode reskk-mode
  ""
  :lighter (:eval (reskk--mode-line))
  (if reskk-mode
    ;; マイナーモード有効化時
    (reskk--update-keymap)
    ;; マイナーモード無効化時
    (setq minor-mode-overriding-map-alist (assq-delete-all 'reskk-mode minor-mode-overriding-map-alist))
    )
  (setq reskk-convert-buffer "")
  )

(provide 'skk-rewrite)

;;; skk-rewrite.el ends here
