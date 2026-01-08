;;; reskk.el --- SKK 再実装 (最小構成) -*- lexical-binding: t; -*-

;; Author: Your Name
;; Version: 0.1
;; Package-Requires: ()
;; Keywords: input, japanese
;; URL: https://github.com/siguma0013/skk-rewrite

;;; Commentary:


;;; Code:
(require 'reskk-overlay)
(require 'reskk-input)

(defgroup skk-rewrite nil
  "SKK 再実装用の設定グループ."
  :group 'input)

(defcustom skk-rewrite-dummy t
  "何もしないダミー変数."
  :type 'boolean
  :group 'skk-rewrite)

;; HALF-ALPHABET:半角英数
;; FULL-ALPHABET:全角英数
;; HIRAGANA:ひらがな
;; KATAKANA:カタカナ
(defvar-local reskk-state 'HALF-ALPHABET
  "SKKモード")

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
(define-minor-mode reskk-mode
  ""
  :lighter (:eval (reskk--mode-line))
  (if reskk-mode
    ;; マイナーモード有効化時
    (reskk--update-keymap)
    ;; マイナーモード無効化時
    (setq minor-mode-overriding-map-alist (assq-delete-all 'reskk-mode minor-mode-overriding-map-alist))
    )
  )

(provide 'reskk)
