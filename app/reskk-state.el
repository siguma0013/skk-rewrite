;;; reskk-state.el --- 状態管理 -*- lexical-binding: t; -*-

;;; Commentary:


;;; Code:


;; SKKモード
;; HALF-ALPHABET:半角英数
;; FULL-ALPHABET:全角英数
;; HIRAGANA:ひらがな
;; KATAKANA:カタカナ
(defvar-local reskk-state 'HALF-ALPHABET)

;; SKK変換モード
;; NONE
;; CONVERT
;; CONVERT-OKURIGANA
;; SELECT
(defvar-local reskk-convert-state 'NONE)

(defvar reskk-half-alphabet-keymap (make-sparse-keymap)
  "SKKモードが半角英数モードの際に使用するキーマップ")

(defvar reskk-hiragana-keymap (make-sparse-keymap)
  "SKKモードがひらがなモードの際に使用するキーマップ")

(defvar reskk-katakana-keymap (make-sparse-keymap)
  "SKKモードがカタカナモードの際に使用するキーマップ")

(defvar reskk-hiragana-convert-keymap (make-sparse-keymap)
  "かな変換中の際に使用するひらがな用キーマップ")

(defvar reskk-katakana-convert-keymap (make-sparse-keymap)
  "かな変換中の際に使用するカタカナ用キーマップ")

(defvar reskk-hiragana-select-keymap (make-sparse-keymap)
  "かな変換選択中に使用するひらがな用キーマップ")

(defvar reskk-katakana-select-keymap (make-sparse-keymap)
  "かな変換選択中に使用するカタカナ用キーマップ")

;; SKKモードカラー決定関数
(defun reskk-get-color ()
  (pcase reskk-state
    ('HALF-ALPHABET reskk-half-alphabet-color)
    ('HIRAGANA reskk-hiragana-color)
    ('KATAKANA reskk-katakana-color)))

;; キーマップ決定関数
(defun reskk-get-keymap ()
  (pcase reskk-state
    ('HALF-ALPHABET reskk-half-alphabet-keymap)
    ('HIRAGANA
      (pcase reskk-convert-state
        ('NONE reskk-hiragana-keymap)
        ('CONVERT reskk-hiragana-convert-keymap)
        ('SELECT reskk-hiragana-select-keymap)
        )
      )
    ('KATAKANA
      (pcase reskk-convert-state
        ('NONE reskk-katakana-keymap)
        ('CONVERT reskk-katakana-convert-keymap)
        ('SELECT reskk-katakana-select-keymap)
        )
      )
    )
  )

;; モードライン文字列決定関数
(defun reskk-get-lighter ()
  (pcase reskk-state
    ('HALF-ALPHABET " SKK[aA]")
    ('FULL-ALPHABET " SKK[Ａ]")
    ('HIRAGANA " SKK[あ]")
    ('KATAKANA " SKK[ア]")))

(provide 'reskk-state)
