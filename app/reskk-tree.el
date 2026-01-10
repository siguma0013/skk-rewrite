;;; reskk-tree.el --- skkかな変換木構造 -*- lexical-binding: t; -*-

;;; Code:
(require 'cl-lib)

;; かな変換用木構造(trie)
(defvar reskk-tree nil)

;; reskk-treeのノード定義
;; [children] 子ノード
;; [value]    出力文字列
;; [pending]  未消費文字列
(cl-defstruct reskk-tree-node
  children
  value
  pending)

;; reskk-tree-node作成関数
(defun reskk-make-node ()
  (make-reskk-tree-node
    :children (make-hash-table :test 'equal)
    :value nil
    :pending nil)
  )

;; reskk-treeへのノード登録関数
(defun reskk-insert-trie (trie key value pending)
  (let ((node trie))
    (dolist (char (string-to-list key))
      (setq node
        (or
          (gethash char (reskk-tree-node-children node))
          (puthash char
            (reskk-make-node)
            (reskk-tree-node-children node)
            )
          )
        )
      )

    ;; 終端登録
    (setf (reskk-tree-node-value node) value)
    (setf (reskk-tree-node-pending node) pending)
    )
  )

;; 木構造の検索関数(完全一致)
(defun reskk-find-something-node (root key)
  (cl-block nil
    (let ((node root))
      ;; 木構造の探査
      (dolist (char (string-to-list key))
        (setq node (gethash char (reskk-tree-node-children node)))
        ;; 最後までノードが発見できなかった時
        (unless node (cl-return nil)))

      ;; ノードが発見できた時
      (cl-return node))))

;; 規定の木構造検索関数(完全一致)
(defun reskk-find-node (key)
  (reskk-find-something-node reskk-tree key))

;; ノードが末端であるかの判定関数
(defun reskk-tree-is-leaf (node)
  (zerop (hash-table-count (reskk-tree-node-children node))))

(setq reskk-tree (reskk-make-node))

;; あ行
(reskk-insert-trie reskk-tree "a" "あ" nil)
(reskk-insert-trie reskk-tree "i" "い" nil)
(reskk-insert-trie reskk-tree "u" "う" nil)
(reskk-insert-trie reskk-tree "e" "え" nil)
(reskk-insert-trie reskk-tree "o" "お" nil)

;; か行
(reskk-insert-trie reskk-tree "ka" "か" nil)
(reskk-insert-trie reskk-tree "ki" "き" nil)
(reskk-insert-trie reskk-tree "ku" "く" nil)
(reskk-insert-trie reskk-tree "ke" "け" nil)
(reskk-insert-trie reskk-tree "ko" "こ" nil)
;; 促音
(reskk-insert-trie reskk-tree "kk" "っ" "k")
;; 拗音
(reskk-insert-trie reskk-tree "kya" "きゃ" nil)
(reskk-insert-trie reskk-tree "kyu" "きゅ" nil)
(reskk-insert-trie reskk-tree "kyo" "きょ" nil)

;; さ行
(reskk-insert-trie reskk-tree "sa" "さ" nil)
(reskk-insert-trie reskk-tree "si" "し" nil)
(reskk-insert-trie reskk-tree "su" "す" nil)
(reskk-insert-trie reskk-tree "se" "せ" nil)
(reskk-insert-trie reskk-tree "so" "そ" nil)
;; 促音
(reskk-insert-trie reskk-tree "ss" "っ" "s")
;; 拗音
(reskk-insert-trie reskk-tree "sya" "しゃ" nil)
(reskk-insert-trie reskk-tree "syu" "しゅ" nil)
(reskk-insert-trie reskk-tree "syo" "しょ" nil)

;; た行
(reskk-insert-trie reskk-tree "ta" "た" nil)
(reskk-insert-trie reskk-tree "ti" "ち" nil)
(reskk-insert-trie reskk-tree "tu" "つ" nil)
(reskk-insert-trie reskk-tree "te" "て" nil)
(reskk-insert-trie reskk-tree "to" "と" nil)
;; 促音
(reskk-insert-trie reskk-tree "tt" "っ" "t")
;; 拗音
(reskk-insert-trie reskk-tree "tya" "ちゃ" nil)
(reskk-insert-trie reskk-tree "tyu" "ちゅ" nil)
(reskk-insert-trie reskk-tree "tyo" "ちょ" nil)

;; な行
(reskk-insert-trie reskk-tree "na" "な" nil)
(reskk-insert-trie reskk-tree "ni" "に" nil)
(reskk-insert-trie reskk-tree "nu" "ぬ" nil)
(reskk-insert-trie reskk-tree "ne" "ね" nil)
(reskk-insert-trie reskk-tree "no" "の" nil)
;; ん行
(reskk-insert-trie reskk-tree "nn" "ん" nil)
;; 拗音
(reskk-insert-trie reskk-tree "nya" "にゃ" nil)
(reskk-insert-trie reskk-tree "nyu" "にゅ" nil)
(reskk-insert-trie reskk-tree "nyo" "にょ" nil)

;; は行
(reskk-insert-trie reskk-tree "ha" "は" nil)
(reskk-insert-trie reskk-tree "hi" "ひ" nil)
(reskk-insert-trie reskk-tree "hu" "ふ" nil)
(reskk-insert-trie reskk-tree "he" "へ" nil)
(reskk-insert-trie reskk-tree "ho" "ほ" nil)
;; 促音
(reskk-insert-trie reskk-tree "hh" "っ" "h")
;; 拗音
(reskk-insert-trie reskk-tree "hya" "ひゃ" nil)
(reskk-insert-trie reskk-tree "hyu" "ひゅ" nil)
(reskk-insert-trie reskk-tree "hyo" "ひょ" nil)

;; ま行
(reskk-insert-trie reskk-tree "ma" "ま" nil)
(reskk-insert-trie reskk-tree "mi" "み" nil)
(reskk-insert-trie reskk-tree "mu" "む" nil)
(reskk-insert-trie reskk-tree "me" "め" nil)
(reskk-insert-trie reskk-tree "mo" "も" nil)
;; 促音
(reskk-insert-trie reskk-tree "mm" "っ" "m")
;; 拗音
(reskk-insert-trie reskk-tree "mya" "みゃ" nil)
(reskk-insert-trie reskk-tree "myu" "みゅ" nil)
(reskk-insert-trie reskk-tree "myo" "みょ" nil)

;; や行
(reskk-insert-trie reskk-tree "ya" "や" nil)
(reskk-insert-trie reskk-tree "yu" "ゆ" nil)
(reskk-insert-trie reskk-tree "yo" "よ" nil)
;; 促音
(reskk-insert-trie reskk-tree "yy" "っ" "y")

;; ら行
(reskk-insert-trie reskk-tree "ra" "ら" nil)
(reskk-insert-trie reskk-tree "ri" "り" nil)
(reskk-insert-trie reskk-tree "ru" "る" nil)
(reskk-insert-trie reskk-tree "re" "れ" nil)
(reskk-insert-trie reskk-tree "ro" "ろ" nil)
;; 促音
(reskk-insert-trie reskk-tree "rr" "っ" "r")
;; 拗音
(reskk-insert-trie reskk-tree "rya" "りゃ" nil)
(reskk-insert-trie reskk-tree "ryu" "りゅ" nil)
(reskk-insert-trie reskk-tree "ryo" "りょ" nil)

;; わ行
(reskk-insert-trie reskk-tree "wa" "わ" nil)
(reskk-insert-trie reskk-tree "wo" "を" nil)
;; 促音
(reskk-insert-trie reskk-tree "ww" "っ" "w")

;; が行
(reskk-insert-trie reskk-tree "ga" "が" nil)
(reskk-insert-trie reskk-tree "gi" "ぎ" nil)
(reskk-insert-trie reskk-tree "gu" "ぐ" nil)
(reskk-insert-trie reskk-tree "ge" "げ" nil)
(reskk-insert-trie reskk-tree "go" "ご" nil)
;; 促音
(reskk-insert-trie reskk-tree "gg" "っ" "g")
;; 拗音
(reskk-insert-trie reskk-tree "gya" "ぎゃ" nil)
(reskk-insert-trie reskk-tree "gyu" "ぎゅ" nil)
(reskk-insert-trie reskk-tree "gyo" "ぎょ" nil)

;; ざ行(z)
(reskk-insert-trie reskk-tree "za" "ざ" nil)
(reskk-insert-trie reskk-tree "zi" "じ" nil)
(reskk-insert-trie reskk-tree "zu" "ず" nil)
(reskk-insert-trie reskk-tree "ze" "ぜ" nil)
(reskk-insert-trie reskk-tree "zo" "ぞ" nil)
;; 促音
(reskk-insert-trie reskk-tree "zz" "っ" "z")
;; 拗音
(reskk-insert-trie reskk-tree "zya" "じゃ" nil)
(reskk-insert-trie reskk-tree "zyu" "じゅ" nil)
(reskk-insert-trie reskk-tree "zyo" "じょ" nil)

;; ざ行(j)
(reskk-insert-trie reskk-tree "ja" "じゃ" nil)
(reskk-insert-trie reskk-tree "ji" "じ" nil)
(reskk-insert-trie reskk-tree "ju" "じゅ" nil)
(reskk-insert-trie reskk-tree "je" "じぇ" nil)
(reskk-insert-trie reskk-tree "jo" "じょ" nil)
;; 促音
(reskk-insert-trie reskk-tree "jj" "っ" "j")

;; だ行
(reskk-insert-trie reskk-tree "da" "だ" nil)
(reskk-insert-trie reskk-tree "di" "ぢ" nil)
(reskk-insert-trie reskk-tree "du" "づ" nil)
(reskk-insert-trie reskk-tree "de" "で" nil)
(reskk-insert-trie reskk-tree "do" "ど" nil)
;; 促音
(reskk-insert-trie reskk-tree "dd" "っ" "d")
;; 拗音
(reskk-insert-trie reskk-tree "dya" "ぢゃ" nil)
(reskk-insert-trie reskk-tree "dyu" "ぢゅ" nil)
(reskk-insert-trie reskk-tree "dyo" "ぢょ" nil)

;; ば行
(reskk-insert-trie reskk-tree "ba" "ば" nil)
(reskk-insert-trie reskk-tree "bi" "び" nil)
(reskk-insert-trie reskk-tree "bu" "ぶ" nil)
(reskk-insert-trie reskk-tree "be" "べ" nil)
(reskk-insert-trie reskk-tree "bo" "ぼ" nil)
;; 促音
(reskk-insert-trie reskk-tree "bb" "っ" "b")
;; 拗音
(reskk-insert-trie reskk-tree "bya" "びゃ" nil)
(reskk-insert-trie reskk-tree "byu" "びゅ" nil)
(reskk-insert-trie reskk-tree "byo" "びょ" nil)

;; ぱ行
(reskk-insert-trie reskk-tree "pa" "ぱ" nil)
(reskk-insert-trie reskk-tree "pi" "ぴ" nil)
(reskk-insert-trie reskk-tree "pu" "ぷ" nil)
(reskk-insert-trie reskk-tree "pe" "ぺ" nil)
(reskk-insert-trie reskk-tree "po" "ぽ" nil)
;; 促音
(reskk-insert-trie reskk-tree "pp" "っ" "p")
;; 拗音
(reskk-insert-trie reskk-tree "pya" "ぴゃ" nil)
(reskk-insert-trie reskk-tree "pyu" "ぴゅ" nil)
(reskk-insert-trie reskk-tree "pyo" "ぴょ" nil)


;; 記号
(reskk-insert-trie reskk-tree "," "、" nil)
(reskk-insert-trie reskk-tree "." "。" nil)
(reskk-insert-trie reskk-tree "-" "ー" nil)
(reskk-insert-trie reskk-tree "!" "！" nil)
(reskk-insert-trie reskk-tree "?" "？" nil)

(provide 'reskk-tree)
