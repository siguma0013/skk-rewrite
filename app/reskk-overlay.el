;;; reskk-overlay.el --- オーバーレイ管理 -*- lexical-binding: t; -*-

;;; Commentary:


;;; Code:

;; 変換中オーバーレイ
(defvar-local reskk-overlay nil)

;; オーバーレイ表示関数
(defun reskk-display-overlay (text)
  ;; 変換中オーバーレイ更新のために一旦削除
  (when reskk-overlay
    (delete-overlay reskk-overlay))

  (when text
    ;; オーバーレイ作成
    (setq reskk-overlay (make-overlay (point) (point)))
    ;; オーバーレイ表示文字列の構成
    (let* ((color (reskk-color))
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
  (let* ((color (reskk-color))
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
  (let* ((color (reskk-color))
          (display-text (or text ""))
          (styled-text (propertize display-text 'face `(:background "blue"))))
    ;; オーバーレイ表示
    (overlay-put reskk-overlay 'display styled-text)
    (overlay-put reskk-overlay 'before-string "▽")
     )
  )

(provide 'reskk-overlay)

;;; reskk-overlay.el ends here
