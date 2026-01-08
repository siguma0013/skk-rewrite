;; 最低限のキーバインド変更
(use-package emacs
  :bind (("C-h" . delete-backward-char)
          ("C-x C-b" . bs-show))
  :custom (make-backup-files . nil)
            (auto-save-default . nil)
            (auto-save-list-file-prefix . nil)
  )


;; 開発コードへload-pathを通す
(add-to-list 'load-path (expand-file-name "../app" user-emacs-directory))


;; パッケージの読み込み
(require 'reskk)
