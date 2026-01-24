;;; reskk-overlay.el --- オーバーレイ管理 -*- lexical-binding: t; -*-

;;; Commentary:


;;; Code:

;; 変換中オーバーレイ
(defvar-local reskk-overlay nil)

;; オーバーレイアクセサ：開始位置
(defun reskk-get-overlay-start ()
  (when (overlayp reskk-overlay)
    (overlay-start reskk-overlay)))

;; オーバーレイアクセサ：'marker'
(defun reskk-get-overlay-marker ()
  (when (overlayp reskk-overlay)
    (overlay-get reskk-overlay 'before-string)))

;; オーバーレイアクセサ：'option'
(defun reskk-get-overlay-option ()
  (when (overlayp reskk-overlay)
    (overlay-get reskk-overlay 'display)))

;; オーバーレイアクセサ：'fragment'
(defun reskk-get-overlay-fragment ()
  (when (overlayp reskk-overlay)
    (overlay-get reskk-overlay 'after-string)))

;; ユーティリティ関数：文字有り判定
;; t：非空文字の文字列
;; nil：非文字列、空文字
(defun reskk-is-string (string)
  (and (stringp string) (not (string-empty-p string))))

;; ユーティリティ関数：表示確認
;; t：表示されている
;; nil：非表示
(defun reskk-is-display-overlay ()
  (or
    (reskk-is-string (reskk-get-overlay-marker))
    (reskk-is-string (reskk-get-overlay-option))
    (reskk-is-string (reskk-get-overlay-fragment))))

;; オーバーレイ表示管理関数
(defun reskk-make-overlay ()
  (if  (reskk-is-display-overlay)
    (move-overlay reskk-overlay (reskk-get-overlay-start) (point))
    (if (and
          (overlayp reskk-overlay)
          (overlay-start reskk-overlay)
          (overlay-end reskk-overlay))
      ;; 次回以降移動
      (move-overlay reskk-overlay (point) (point))
      ;; 初回だけ作成
      (setq reskk-overlay (make-overlay (point) (point))))
    )
  )

;; オーバーレイリセット関数
(defun reskk-reset-overlay ()
  (when (overlayp reskk-overlay)
    (overlay-put reskk-overlay 'before-string nil)
    (overlay-put reskk-overlay 'display nil)
    (overlay-put reskk-overlay 'after-string nil)
    ))

(defun reskk-display-overlay-marker (text)
  (reskk-make-overlay)
  (overlay-put reskk-overlay 'before-string text))

(defun reskk-display-overlay-option (text)
  (reskk-make-overlay)
  (overlay-put reskk-overlay 'display (propertize (or text "") 'face `(:background "cyan"))))

(defun reskk-display-overlay-fragment (text)
  (reskk-make-overlay)
  (overlay-put reskk-overlay 'after-string (propertize (or text "") 'face `(:foreground ,(reskk-get-color)))))

(provide 'reskk-overlay)

;;; reskk-overlay.el ends here
