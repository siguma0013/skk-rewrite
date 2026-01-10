;;; reskk-dictionary.el --- 辞書管理 -*- lexical-binding: t; -*-

;;; Commentary:


;;; Code:
(require 'json)

(defcustom reskk-dictionary-file-path "~/.emacs.d/reskk-dictionary.jsonl"
  "辞書ファイルのパス"
  :type 'file
  :group 'reskk-mode)

(defvar reskk-dictionary (make-hash-table :test 'equal))

;; 辞書ファイルのパース関数
(defun reskk-dictionary-parse (file-path call-back)
  (with-temp-buffer
    (insert-file-contents file-path)
    (goto-char (point-min))

    ;; ファイル末尾に到達するまで繰り返し処理
    (while (not (eobp))
      (let ((line (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
        ;; 空行でなければ処理
        (unless (string-empty-p line)
          (let ((obj (json-parse-string line :object-type 'alist :array-type 'list :null-object nil :false-object :json-false)))
            (funcall call-back obj)
            )
          )

        )

      (forward-line 1)
      )
    )
  )

(reskk-dictionary-parse reskk-dictionary-file-path
  (lambda (obj)
    (let ((key (alist-get 'key obj))
           (values (alist-get 'value obj)))
      (puthash key values reskk-dictionary)
      )
    )
  )

(provide 'reskk-dictionary)
