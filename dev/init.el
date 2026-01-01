;; 最低限のキーバインド変更
(use-package emacs
  :bind (("C-h" . delete-backward-char)
          ("C-x C-b" . bs-show)))


;; 開発コードへload-pathを通す
(add-to-list 'load-path (expand-file-name "../app" user-emacs-directory))


;; パッケージの読み込み
(require 'skk-rewrite)

(skk-rewrite-start)
