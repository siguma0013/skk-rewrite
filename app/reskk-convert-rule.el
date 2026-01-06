;; reskk-convert-rule.el --- skk変換ルール -*- lexical-binding: t; -*-

;;; Code:

(require 'cl-lib)

;; trieノード
;; child 子ノード
;; value 出力文字列
;; pending 未消費文字列
(cl-defstruct reskk-convert-tree
  "木構造(trie)による変換テーブル"
  children
  value
  pending)

(defun reskk-make-trie ()
  "木構造(trie)のノード作成関数"
  (make-reskk-convert-tree
    :children (make-hash-table :test 'equal)
    :value nil
    :pending nil)
  )

(defun reskk-insert-trie (trie key value pending)
  "木構造(trie)のキー登録関数"
  (let ((node trie))
    (dolist (char (string-to-list key))
      (setq node
        (or
          (gethash char (reskk-convert-tree-children node))
          (puthash char
            (reskk-make-trie)
            (reskk-convert-tree-children node)
            )
          )
        )
      )

    ;; 終端登録
    (setf (reskk-convert-tree-value node) value)
    (setf (reskk-convert-tree-pending node) pending)
    )
  )

(defun reskk--find-node (root key)
  "木構造(trie)の完全一致検索関数"
  (cl-block find-node
    (let ((node root))
      (dolist (char (string-to-list key))
        (setq node (gethash char (reskk-convert-tree-children node)))
        (unless node (cl-return-from find-node nil)))

      (cl-return-from find-node node)
      )
    )
  )

(defun reskk-find-node (key)
  "規定の完全一致検索関数"
  (reskk--find-node reskk-convert-trie key))

(defun reskk-tree-is-leaf (node)
  "ノードが末端であるかの判定関数"
  (zerop (hash-table-count (reskk-convert-tree-children node))))

(defun reskk-tree-get-value (leaf)
  "ノードから出力文字列を取得するインターフェース"
  (reskk-convert-tree-value leaf))

(defun reskk-tree-get-pending (leaf)
  "ノードから未消費文字列を取得するインターフェース"
  (reskk-convert-tree-pending leaf))

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
