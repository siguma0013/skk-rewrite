;;; reskk.el --- SKK 再実装 (最小構成) -*- lexical-binding: t; -*-

;; Author: Your Name
;; Version: 0.1
;; Package-Requires: ()
;; Keywords: input, japanese
;; URL: https://github.com/siguma0013/skk-rewrite

;;; Commentary:


;;; Code:
(require 'reskk-input)

(defgroup reskk-mode nil
  "Re:SKKの設定グループ."
  :group 'input)

(defcustom skk-rewrite-dummy t
  "何もしないダミー変数."
  :type 'boolean
  :group 'reskk-mode)

;; SKKモード
;; HALF-ALPHABET:半角英数
;; FULL-ALPHABET:全角英数
;; HIRAGANA:ひらがな
;; KATAKANA:カタカナ
(defvar-local reskk-state 'HALF-ALPHABET)

;; モードライン文字列決定関数
(defun reskk-mode-line ()
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
  :doc "SKKモードがひらがなモードの際に使用するキーマップ")

(defvar-keymap reskk-katakana-keymap
  :doc "SKKモードがカタカナモードの際に使用するキーマップ")

(define-key reskk-hiragana-keymap [remap self-insert-command] #'reskk-insert)
;; 削除系コマンドのオーバーライド
(define-key reskk-hiragana-keymap [remap delete-backward-char] #'reskk-backward-char)
(define-key reskk-hiragana-keymap [remap backward-delete-char-untabify] #'reskk-backward-char)


(cl-loop for count from ?a to ?z do
  (message "KEY:%d => %s" count (char-to-string count))
  (define-key reskk-hiragana-keymap (char-to-string count) #'reskk-insert-hiragana)
  (define-key reskk-katakana-keymap (char-to-string count) #'reskk-insert-katakana)
  )
(cl-loop for count from ?A to ?Z do
  (message "KEY:%d => %s" count (char-to-string count))
  )

(progn
  (keymap-set reskk-hiragana-keymap "l" #'reskk-activate-half-alphabet)
  (keymap-set reskk-hiragana-keymap "q" #'reskk-activate-katakana)
  )

(progn
  (keymap-set reskk-katakana-keymap "l" #'reskk-activate-half-alphabet)
  (keymap-set reskk-katakana-keymap "q" #'reskk-activate-katakana)
  )

;; キーマップ決定関数
(defun reskk-keymap ()
  (pcase reskk-state
    ('HALF-ALPHABET reskk-half-alphabet-keymap)
    ('HIRAGANA reskk-hiragana-keymap)
    ('KATAKANA reskk-katakana-keymap)))

;; キーマップ更新関数
(defun reskk-update-keymap ()
  (let ((entry (assq 'reskk-mode minor-mode-overriding-map-alist)))
    ;; minor-mode-overriding-map-alistに〜
    (if entry
      ;; 登録済のとき
      (setcdr entry (reskk-keymap))
      ;; 未登録のとき
      (push `(reskk-mode . ,(reskk-keymap)) minor-mode-overriding-map-alist)
      )
    )
  )

;; SKKモード変更関数
(defun reskk-set-state (state)
  (setq-local reskk-state state)
  (reskk-update-keymap)
  (force-mode-line-update))

(defun reskk-activate-half-alphabet ()
  "SKKモードを半角英数モードに変更するコマンド"
  (interactive)
  (reskk-set-state 'HALF-ALPHABET))

(defun reskk-activate-hiragana ()
  "SKKモードをひらがなモードに変更するコマンド"
  (interactive)
  (reskk-set-state 'HIRAGANA))

(defun reskk-activate-katakana ()
  "SKKモードをカタカナモードに変更するコマンド"
  (interactive)
  (reskk-set-state 'KATAKANA))

;;;###autoload
(define-minor-mode reskk-mode
  ""
  :lighter (:eval (reskk-mode-line))
  (if reskk-mode
    ;; マイナーモード有効化時
    (reskk-update-keymap)
    ;; マイナーモード無効化時
    (setq minor-mode-overriding-map-alist (assq-delete-all 'reskk-mode minor-mode-overriding-map-alist))
    )
  )

(provide 'reskk)
