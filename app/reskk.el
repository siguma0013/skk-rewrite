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

(defcustom reskk-half-alphabet-color "gray"
  "SKKモードが半角英数モードの際に使用する色"
  :type 'color
  :group 'reskk-mode)

(defcustom reskk-hiragana-color "pink"
  "SKKモードがひらがなモードの際に使用する色"
  :type 'color
  :group 'reskk-mode)

(defcustom reskk-katakana-color "green"
  "SKKモードがカタカナモードの際に使用する色"
  :type 'color
  :group 'reskk-mode)

;; デフォカラー保持変数
(defvar reskk-default-color "red")

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

(define-key reskk-hiragana-keymap [remap self-insert-command] #'reskk-insert)
;; 削除系コマンドのオーバーライド
(define-key reskk-hiragana-keymap [remap delete-backward-char] #'reskk-backward-char)
(define-key reskk-hiragana-keymap [remap backward-delete-char-untabify] #'reskk-backward-char)

(keymap-set reskk-half-alphabet-keymap "C-j" #'reskk-activate-hiragana)
(keymap-set reskk-hiragana-convert-keymap "C-j" #'reskk-insert-convert-start)

(define-key reskk-hiragana-convert-keymap [remap self-insert-command] #'reskk-insert-convert)

(cl-loop for count from ?a to ?z do
  (message "KEY:%d => %s" count (char-to-string count))
  (define-key reskk-hiragana-keymap (char-to-string count) #'reskk-insert-hiragana)
  (define-key reskk-katakana-keymap (char-to-string count) #'reskk-insert-katakana)
  )
(cl-loop for count from ?A to ?Z do
  (message "KEY:%d => %s" count (char-to-string count))
  (define-key reskk-hiragana-keymap (char-to-string count) #'reskk-activate-convert)
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
  "現在の `reskk-state' と `reskk-convert-state' に基づいて適切なキーマップを返す。"
  (if (eq reskk-convert-state 'NONE)
    ;; 変換中でなければ通常のキーマップ
    (pcase reskk-state
      ('HALF-ALPHABET reskk-half-alphabet-keymap)
      ('HIRAGANA reskk-hiragana-keymap)
      ('KATAKANA reskk-katakana-keymap))
    ;; かな変換中は変換用キーマップを優先する
    (pcase reskk-state
      ('HALF-ALPHABET reskk-half-alphabet-keymap)
      ('HIRAGANA reskk-hiragana-convert-keymap)
      ('KATAKANA reskk-katakana-convert-keymap)
      (_ reskk-half-alphabet-keymap))
    ))

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

;; SKKモードカラー決定関数
(defun reskk-color ()
  (pcase reskk-state
    ('HALF-ALPHABET reskk-half-alphabet-color)
    ('HIRAGANA reskk-hiragana-color)
    ('KATAKANA reskk-katakana-color)))

;; カーソル色更新関数
(defun reskk-update-cursor-color ()
  (if reskk-mode
    ;; マイナーモード有効時
    (set-cursor-color (reskk-color))
    ;; マイナーモード非有効時
    (set-cursor-color reskk-default-color)
    )
  )

;; self-insert-command以外が実行された時に、変換中バッファを強制削除する関数
(defun reskk-convert-force-cancel ()
  (unless (memq this-command '(reskk-insert
                                reskk-insert-hiragana
                                reskk-insert-katakana
                                reskk-backward-char
                                reskk-insert-convert
                                reskk-activate-convert
                                ))
    (reskk-clear-buffer))
  )

;; SKKモード変更関数
(defun reskk-set-state (state)
  (setq reskk-state state)
  (reskk-update-keymap)
  (reskk-update-cursor-color)
  (force-mode-line-update)
  (pcase reskk-state
    ((or 'HIRAGANA 'KATAKANA)
      (add-hook 'pre-command-hook #'reskk-convert-force-cancel nil t))
    (_
      (remove-hook 'pre-command-hook #'reskk-convert-force-cancel t))
    )
  )

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

(defun reskk-activate-convert ()
  (interactive)
  (setq reskk-convert-state 'CONVERT)
  (reskk-set-state 'HIRAGANA)
  (reskk-insert-convert)
  )

;; デフォカラー取得
(add-hook 'after-init-hook
  #'(lambda ()
      (setq reskk-default-color (frame-parameter nil 'cursor-color))))

;;;###autoload
(define-minor-mode reskk-mode
  ""
  :lighter (:eval (reskk-mode-line))
  (if reskk-mode
    ;; マイナーモード有効化時
    (progn
      (reskk-set-state 'HALF-ALPHABET)
      (add-hook 'window-selection-change-functions #'(lambda (_window)
                                                       (with-current-buffer (window-buffer)
                                                         (reskk-update-cursor-color)
                                                         )
                                                       ))
      )
    ;; マイナーモード無効化時
    (setq minor-mode-overriding-map-alist (assq-delete-all 'reskk-mode minor-mode-overriding-map-alist))
    )
  )

(provide 'reskk)
