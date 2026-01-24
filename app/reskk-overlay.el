;;; reskk-overlay.el --- オーバーレイ管理 -*- lexical-binding: t; -*-

;;; Commentary:


;;; Code:

;; 変換中オーバーレイ
(defvar-local reskk-overlay nil)

;; ユーティリティ関数：文字無し判定
;; t：非文字列、空文字
;; nil：それ以外
(defun reskk-is-empty-string (string)
  (or (not (stringp string)) (string-empty-p string)))

;; オーバーレイ作成関数
(defun reskk-make-overlay (start-point end-point)
  (unless (and
            (overlayp reskk-overlay)
            (overlay-start reskk-overlay)
            (overlay-end reskk-overlay))
    ;; オーバーレイが生存していなければ作成
    (setq reskk-overlay (make-overlay start-point end-point))))

;; オーバーレイ削除関数
(defun reskk-delete-overlay ()
  (when (overlayp reskk-overlay)
    (let* ((display (overlay-get reskk-overlay 'display))
            (after-string (overlay-get reskk-overlay 'after-string)))
      (unless (and
                (reskk-is-empty-string display)
                (reskk-is-empty-string after-string))
        ;; オーバーレイの'display'と'after-string'が空であれば削除
        (delete-overlay reskk-overlay)
        (overlay-put reskk-overlay 'display nil)
        (overlay-put reskk-overlay 'after-string nil)
        (overlay-put reskk-overlay 'before-string nil)))))

(defun reskk-display-overlay-fragment (text)
  (reskk-make-overlay (point) (point))
  (overlay-put reskk-overlay 'after-string (propertize text 'face `(:foreground ,(reskk-get-color)))))


;; オーバーレイ表示関数
(defun reskk-display-overlay (text)
  ;; 変換中オーバーレイ更新のために一旦削除
  (when reskk-overlay
    (delete-overlay reskk-overlay))

  (when text
    ;; オーバーレイ作成
    (setq reskk-overlay (make-overlay (point) (point)))
    ;; オーバーレイ表示文字列の構成
    (let* ((color (reskk-get-color))
            (styled-text (propertize text 'face `(:foreground ,color))))
      ;; オーバーレイ表示
      (overlay-put reskk-overlay 'after-string styled-text)
      )
    )
  )

;; オーバーレイ表示関数
(defun reskk-display-convert-overlay (target-point text)
  ;; 変換中オーバーレイ更新のために一旦削除
  (when reskk-overlay
    (delete-overlay reskk-overlay))

  ;; オーバーレイ作成
  (setq reskk-overlay (make-overlay target-point (point)))

  ;; オーバーレイ表示文字列の構成
  (let* ((color (reskk-get-color))
         (display-text (or text ""))
         (styled-text (propertize display-text 'face `(:foreground ,color :background "red"))))
    ;; オーバーレイ表示
    (overlay-put reskk-overlay 'after-string styled-text)
    (overlay-put reskk-overlay 'before-string "▽")
     )
   )

(defun reskk-display-select-overlay (target-point text)
  ;; 変換中オーバーレイ更新のために一旦削除
  (when reskk-overlay
    (delete-overlay reskk-overlay))

  ;; オーバーレイ作成
  (setq reskk-overlay (make-overlay target-point (point)))

  ;; オーバーレイ表示文字列の構成
  (let* ((color (reskk-get-color))
          (display-text (or text ""))
          (styled-text (propertize display-text 'face `(:background "blue"))))
    ;; オーバーレイ表示
    (overlay-put reskk-overlay 'display styled-text)
    (overlay-put reskk-overlay 'before-string "▽")
     )
  )

(provide 'reskk-overlay)

;;; reskk-overlay.el ends here
