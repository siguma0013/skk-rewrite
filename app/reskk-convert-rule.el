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

;; あ行
(reskk-insert-trie reskk-convert-trie "a" "あ" nil)
(reskk-insert-trie reskk-convert-trie "i" "い" nil)
(reskk-insert-trie reskk-convert-trie "u" "う" nil)
(reskk-insert-trie reskk-convert-trie "e" "え" nil)
(reskk-insert-trie reskk-convert-trie "o" "お" nil)

;; か行
(reskk-insert-trie reskk-convert-trie "ka" "か" nil)
(reskk-insert-trie reskk-convert-trie "ki" "き" nil)
(reskk-insert-trie reskk-convert-trie "ku" "く" nil)
(reskk-insert-trie reskk-convert-trie "ke" "け" nil)
(reskk-insert-trie reskk-convert-trie "ko" "こ" nil)
;; 促音
(reskk-insert-trie reskk-convert-trie "kk" "っ" "k")
;; 拗音
(reskk-insert-trie reskk-convert-trie "kya" "きゃ" nil)
(reskk-insert-trie reskk-convert-trie "kyu" "きゅ" nil)
(reskk-insert-trie reskk-convert-trie "kyo" "きょ" nil)

;; さ行
(reskk-insert-trie reskk-convert-trie "sa" "さ" nil)
(reskk-insert-trie reskk-convert-trie "si" "し" nil)
(reskk-insert-trie reskk-convert-trie "su" "す" nil)
(reskk-insert-trie reskk-convert-trie "se" "せ" nil)
(reskk-insert-trie reskk-convert-trie "so" "そ" nil)
;; 促音
(reskk-insert-trie reskk-convert-trie "ss" "っ" "s")
;; 拗音
(reskk-insert-trie reskk-convert-trie "sya" "しゃ" nil)
(reskk-insert-trie reskk-convert-trie "syu" "しゅ" nil)
(reskk-insert-trie reskk-convert-trie "syo" "しょ" nil)

;; た行
(reskk-insert-trie reskk-convert-trie "ta" "た" nil)
(reskk-insert-trie reskk-convert-trie "ti" "ち" nil)
(reskk-insert-trie reskk-convert-trie "tu" "つ" nil)
(reskk-insert-trie reskk-convert-trie "te" "て" nil)
(reskk-insert-trie reskk-convert-trie "to" "と" nil)
;; 促音
(reskk-insert-trie reskk-convert-trie "tt" "っ" "t")
;; 拗音
(reskk-insert-trie reskk-convert-trie "tya" "ちゃ" nil)
(reskk-insert-trie reskk-convert-trie "tyu" "ちゅ" nil)
(reskk-insert-trie reskk-convert-trie "tyo" "ちょ" nil)

;; な行
(reskk-insert-trie reskk-convert-trie "na" "な" nil)
(reskk-insert-trie reskk-convert-trie "ni" "に" nil)
(reskk-insert-trie reskk-convert-trie "nu" "ぬ" nil)
(reskk-insert-trie reskk-convert-trie "ne" "ね" nil)
(reskk-insert-trie reskk-convert-trie "no" "の" nil)
;; ん行
(reskk-insert-trie reskk-convert-trie "nn" "ん" nil)
;; 拗音
(reskk-insert-trie reskk-convert-trie "nya" "にゃ" nil)
(reskk-insert-trie reskk-convert-trie "nyu" "にゅ" nil)
(reskk-insert-trie reskk-convert-trie "nyo" "にょ" nil)

;; は行
(reskk-insert-trie reskk-convert-trie "ha" "は" nil)
(reskk-insert-trie reskk-convert-trie "hi" "ひ" nil)
(reskk-insert-trie reskk-convert-trie "hu" "ふ" nil)
(reskk-insert-trie reskk-convert-trie "he" "へ" nil)
(reskk-insert-trie reskk-convert-trie "ho" "ほ" nil)
;; 促音
(reskk-insert-trie reskk-convert-trie "hh" "っ" "h")
;; 拗音
(reskk-insert-trie reskk-convert-trie "hya" "ひゃ" nil)
(reskk-insert-trie reskk-convert-trie "hyu" "ひゅ" nil)
(reskk-insert-trie reskk-convert-trie "hyo" "ひょ" nil)

;; ま行
(reskk-insert-trie reskk-convert-trie "ma" "ま" nil)
(reskk-insert-trie reskk-convert-trie "mi" "み" nil)
(reskk-insert-trie reskk-convert-trie "mu" "む" nil)
(reskk-insert-trie reskk-convert-trie "me" "め" nil)
(reskk-insert-trie reskk-convert-trie "mo" "も" nil)
;; 促音
(reskk-insert-trie reskk-convert-trie "mm" "っ" "m")
;; 拗音
(reskk-insert-trie reskk-convert-trie "mya" "みゃ" nil)
(reskk-insert-trie reskk-convert-trie "myu" "みゅ" nil)
(reskk-insert-trie reskk-convert-trie "myo" "みょ" nil)

;; や行
(reskk-insert-trie reskk-convert-trie "ya" "や" nil)
(reskk-insert-trie reskk-convert-trie "yu" "ゆ" nil)
(reskk-insert-trie reskk-convert-trie "yo" "よ" nil)
;; 促音
(reskk-insert-trie reskk-convert-trie "yy" "っ" "y")

;; ら行
(reskk-insert-trie reskk-convert-trie "ra" "ら" nil)
(reskk-insert-trie reskk-convert-trie "ri" "り" nil)
(reskk-insert-trie reskk-convert-trie "ru" "る" nil)
(reskk-insert-trie reskk-convert-trie "re" "れ" nil)
(reskk-insert-trie reskk-convert-trie "ro" "ろ" nil)
;; 促音
(reskk-insert-trie reskk-convert-trie "rr" "っ" "r")
;; 拗音
(reskk-insert-trie reskk-convert-trie "rya" "りゃ" nil)
(reskk-insert-trie reskk-convert-trie "ryu" "りゅ" nil)
(reskk-insert-trie reskk-convert-trie "ryo" "りょ" nil)

;; わ行
(reskk-insert-trie reskk-convert-trie "wa" "わ" nil)
(reskk-insert-trie reskk-convert-trie "wo" "を" nil)
;; 促音
(reskk-insert-trie reskk-convert-trie "ww" "っ" "w")

;; が行
(reskk-insert-trie reskk-convert-trie "ga" "が" nil)
(reskk-insert-trie reskk-convert-trie "gi" "ぎ" nil)
(reskk-insert-trie reskk-convert-trie "gu" "ぐ" nil)
(reskk-insert-trie reskk-convert-trie "ge" "げ" nil)
(reskk-insert-trie reskk-convert-trie "go" "ご" nil)
;; 促音
(reskk-insert-trie reskk-convert-trie "gg" "っ" "g")
;; 拗音
(reskk-insert-trie reskk-convert-trie "gya" "ぎゃ" nil)
(reskk-insert-trie reskk-convert-trie "gyu" "ぎゅ" nil)
(reskk-insert-trie reskk-convert-trie "gyo" "ぎょ" nil)

;; ざ行(z)
(reskk-insert-trie reskk-convert-trie "za" "ざ" nil)
(reskk-insert-trie reskk-convert-trie "zi" "じ" nil)
(reskk-insert-trie reskk-convert-trie "zu" "ず" nil)
(reskk-insert-trie reskk-convert-trie "ze" "ぜ" nil)
(reskk-insert-trie reskk-convert-trie "zo" "ぞ" nil)
;; 促音
(reskk-insert-trie reskk-convert-trie "zz" "っ" "z")
;; 拗音
(reskk-insert-trie reskk-convert-trie "zya" "じゃ" nil)
(reskk-insert-trie reskk-convert-trie "zyu" "じゅ" nil)
(reskk-insert-trie reskk-convert-trie "zyo" "じょ" nil)

;; ざ行(j)
(reskk-insert-trie reskk-convert-trie "ja" "じゃ" nil)
(reskk-insert-trie reskk-convert-trie "ji" "じ" nil)
(reskk-insert-trie reskk-convert-trie "ju" "じゅ" nil)
(reskk-insert-trie reskk-convert-trie "je" "じぇ" nil)
(reskk-insert-trie reskk-convert-trie "jo" "じょ" nil)
;; 促音
(reskk-insert-trie reskk-convert-trie "jj" "っ" "j")

;; だ行
(reskk-insert-trie reskk-convert-trie "da" "だ" nil)
(reskk-insert-trie reskk-convert-trie "di" "ぢ" nil)
(reskk-insert-trie reskk-convert-trie "du" "づ" nil)
(reskk-insert-trie reskk-convert-trie "de" "で" nil)
(reskk-insert-trie reskk-convert-trie "do" "ど" nil)
;; 促音
(reskk-insert-trie reskk-convert-trie "dd" "っ" "d")
;; 拗音
(reskk-insert-trie reskk-convert-trie "dya" "ぢゃ" nil)
(reskk-insert-trie reskk-convert-trie "dyu" "ぢゅ" nil)
(reskk-insert-trie reskk-convert-trie "dyo" "ぢょ" nil)

;; ば行
(reskk-insert-trie reskk-convert-trie "ba" "ば" nil)
(reskk-insert-trie reskk-convert-trie "bi" "び" nil)
(reskk-insert-trie reskk-convert-trie "bu" "ぶ" nil)
(reskk-insert-trie reskk-convert-trie "be" "べ" nil)
(reskk-insert-trie reskk-convert-trie "bo" "ぼ" nil)
;; 促音
(reskk-insert-trie reskk-convert-trie "bb" "っ" "b")
;; 拗音
(reskk-insert-trie reskk-convert-trie "bya" "びゃ" nil)
(reskk-insert-trie reskk-convert-trie "byu" "びゅ" nil)
(reskk-insert-trie reskk-convert-trie "byo" "びょ" nil)

;; ぱ行
(reskk-insert-trie reskk-convert-trie "pa" "ぱ" nil)
(reskk-insert-trie reskk-convert-trie "pi" "ぴ" nil)
(reskk-insert-trie reskk-convert-trie "pu" "ぷ" nil)
(reskk-insert-trie reskk-convert-trie "pe" "ぺ" nil)
(reskk-insert-trie reskk-convert-trie "po" "ぽ" nil)
;; 促音
(reskk-insert-trie reskk-convert-trie "pp" "っ" "p")
;; 拗音
(reskk-insert-trie reskk-convert-trie "pya" "ぴゃ" nil)
(reskk-insert-trie reskk-convert-trie "pyu" "ぴゅ" nil)
(reskk-insert-trie reskk-convert-trie "pyo" "ぴょ" nil)

(provide 'reskk-convert-rule)

;;; reskk-convert-rule.el ends here
