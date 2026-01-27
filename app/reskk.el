;;; reskk.el --- SKK 再実装 (最小構成) -*- lexical-binding: t; -*-

;; Author: Your Name
;; Version: 0.1
;; Package-Requires: ()
;; Keywords: input, japanese
;; URL: https://github.com/siguma0013/skk-rewrite

;;; Commentary:


;;; Code:
(require 'reskk-state)
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

(declare-function reskk-activate-hiragana nil)
(declare-function reskk-activate-half-alphabet nil)

(define-key reskk-hiragana-keymap [remap self-insert-command] #'reskk-insert)
;; 削除系コマンドのオーバーライド
(define-key reskk-hiragana-keymap [remap delete-backward-char] #'reskk-backward-char)
(define-key reskk-hiragana-keymap [remap backward-delete-char-untabify] #'reskk-backward-char)

(keymap-set reskk-half-alphabet-keymap "C-j" #'reskk-activate-hiragana)
(keymap-set reskk-hiragana-convert-keymap "C-j" #'reskk-activate-start)
(keymap-set reskk-hiragana-select-keymap "C-j" #'reskk-insert-convert-confirm)

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

;; キーマップ更新関数
(defun reskk-update-keymap ()
  (let ((entry (assq 'reskk-mode minor-mode-overriding-map-alist)))
    ;; minor-mode-overriding-map-alistに〜
    (if entry
      ;; 登録済のとき
      (setcdr entry (reskk-get-keymap))
      ;; 未登録のとき
      (push `(reskk-mode . ,(reskk-get-keymap)) minor-mode-overriding-map-alist)
      )
    )
  )

;; カーソル色更新関数
(defun reskk-update-cursor-color ()
  (if reskk-mode
    ;; マイナーモード有効時
    (set-cursor-color (reskk-get-color))
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
                                reskk-activate-start
                                reskk-insert-convert-confirm
                                ))
    (reskk-clear-buffer))
  )

(add-hook
  'reskk-update-state-hook
  (lambda ()
    (reskk-update-keymap)
    (reskk-update-cursor-color)
    (force-mode-line-update)
    (pcase reskk-state
      ((or 'HIRAGANA 'KATAKANA)
        (add-hook 'pre-command-hook #'reskk-convert-force-cancel nil t))
      (_
        (remove-hook 'pre-command-hook #'reskk-convert-force-cancel t))
      )
    ))

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

(defun reskk-activate-start ()
  (interactive)
  (setq reskk-convert-state 'SELECT)
  (reskk-set-state 'HIRAGANA)
  (reskk-insert-convert-start)
  )

;; デフォカラー取得
(add-hook 'after-init-hook
  #'(lambda ()
      (setq reskk-default-color (frame-parameter nil 'cursor-color))))

;;;###autoload
(define-minor-mode reskk-mode
  ""
  :lighter (:eval (reskk-get-lighter))
  (if reskk-mode
    ;; マイナーモード有効化時
    (progn
      (setq reskk-convert-state 'NONE)
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
