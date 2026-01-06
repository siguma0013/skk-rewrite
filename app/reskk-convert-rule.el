;; reskk-convert-rule.el --- skk変換ルール -*- lexical-binding: t; -*-

;;; Code:

(require 'cl-lib)

;; trieノード
;; child 子ノード
;; value 出力文字列
;; pending 未消費文字列
(cl-defstruct reskk-trie
  children
  value
  pending)

;; ノード作成関数
(defun reskk-make-trie ()
  (make-reskk-trie
    :children (make-hash-table :test 'equal)
    :value nil
    :pending nil)
  )

;; キー登録関数
(defun reskk-insert-trie (trie key value pending)
  (let ((node trie))
    (dolist (char (string-to-list key))
      (setq node
        (or
          (gethash char (reskk-trie-children node))
          (puthash char
            (reskk-make-trie)
            (reskk-trie-children node)
            )
          )
        )
      )

    ;; 終端登録
    (setf (reskk-trie-value node) value)
    (setf (reskk-trie-pending node) pending)
    )
  )

;; 完全一致検索
(defun reskk-find-trie (trie key)
  (let ((node trie))
    (catch 'fail
      (dolist (char (string-to-list key))
        (setq node (gethash char (reskk-trie-children node)))
        (unless node (throw 'fail nil))
        )
      node)
    )
  )

;; 一致判定関数
(defun reskk-state-trie (trie key)
  (let ((node trie))
    (catch 'fail
      (dolist (char (string-to-list key))
        (setq node (gethash char (reskk-trie-children node)))
        (unless node (throw 'fail :invalid))
        )

      (cond
        ;; 値あり＆ノードあり
        ((and (reskk-trie-value node)
           (> (hash-table-count (reskk-trie-children node)) 0)
           )
          :commit-or-wait)
        ;; 値あり＆ノードなし
        ((reskk-trie-value node)
          :commit)
        ;; 値なし＆ノードあり
        (t :wait)
        )
      )
    )
  )

(setq reskk-convert-trie (reskk-make-trie))

(reskk-insert-trie reskk-convert-trie "ka" "か" nil)
(reskk-insert-trie reskk-convert-trie "ki" "き" nil)
(reskk-insert-trie reskk-convert-trie "ku" "く" nil)
(reskk-insert-trie reskk-convert-trie "ke" "け" nil)
(reskk-insert-trie reskk-convert-trie "ko" "こ" nil)
(reskk-insert-trie reskk-convert-trie "kk" "っ" "k")

(reskk-insert-trie reskk-convert-trie "na" "な" nil)
(reskk-insert-trie reskk-convert-trie "ni" "に" nil)
(reskk-insert-trie reskk-convert-trie "nu" "ぬ" nil)
(reskk-insert-trie reskk-convert-trie "ne" "ね" nil)
(reskk-insert-trie reskk-convert-trie "no" "の" nil)
(reskk-insert-trie reskk-convert-trie "nya" "にゃ" nil)
(reskk-insert-trie reskk-convert-trie "nyu" "にゅ" nil)
(reskk-insert-trie reskk-convert-trie "nyo" "にょ" nil)




(provide 'reskk-convert-rule)

;;; reskk-convert-rule.el ends here
