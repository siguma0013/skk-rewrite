;;; reskk-tree.el --- かな変換木構造 -*- lexical-binding: t; -*-

;;; Code:
(require 'cl-lib)

;; かな変換木構造のノード定義
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

;; かな変換木構造
(defconst reskk-tree (reskk-make-node))

;; 木構造へのノード登録関数
(defun reskk-insert-something-node (trie key value pending)
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

;; ノード登録関数のショートハンド
(defun reskk-insert-node (key value)
  (reskk-insert-something-node reskk-tree key value nil))

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

;; 木構造のデータエントリ
(defconst reskk-tree-entries
  '(
     ;; 五十音
     ( "a" "あ") ( "i" "い") ( "u" "う") ( "e" "え") ( "o" "お")
     ("ka" "か") ("ki" "き") ("ku" "く") ("ke" "け") ("ko" "こ")
     ("sa" "さ") ("si" "し") ("su" "す") ("se" "せ") ("so" "そ")
     ("ta" "た") ("ti" "ち") ("tu" "つ") ("te" "て") ("to" "と")
     ("na" "な") ("ni" "に") ("nu" "ぬ") ("ne" "ね") ("no" "の")
     ("ha" "は") ("hi" "ひ") ("hu" "ふ") ("he" "へ") ("ho" "ほ")
     ("ma" "ま") ("mi" "み") ("mu" "む") ("me" "め") ("mo" "も")
     ("ya" "や")             ("yu" "ゆ")             ("yo" "よ")
     ("ra" "ら") ("ri" "り") ("ru" "る") ("re" "れ") ("ro" "ろ")
     ("wa" "わ")                                     ("wo" "を")
     ("nn" "ん")

     ;; 濁音、半濁音
     ("ga" "が") ("gi" "ぎ") ("gu" "ぐ") ("ge" "げ") ("go" "ご")
     ("za" "ざ") ("zi" "じ") ("zu" "ず") ("ze" "ぜ") ("zo" "ぞ")
     ("da" "だ") ("di" "ぢ") ("du" "づ") ("de" "で") ("do" "ど")
     ("ba" "ば") ("bi" "び") ("bu" "ぶ") ("be" "べ") ("bo" "ぼ")
     ("pa" "ぱ") ("pi" "ぴ") ("pu" "ぷ") ("pe" "ぺ") ("po" "ぽ")

     ;; 拗音
     ("kya" "きゃ") ("kyi" "きぃ") ("kyu" "きゅ") ("kye" "きぇ") ("kyo" "きょ")
     ("sya" "しゃ") ("syi" "しぃ") ("syu" "しゅ") ("sye" "しぇ") ("syo" "しょ")
     ("tya" "ちゃ") ("tyi" "ちぃ") ("tyu" "ちゅ") ("tye" "ちぇ") ("tyo" "ちょ")
     ("nya" "にゃ") ("nyi" "にぃ") ("nyu" "にゅ") ("nye" "にぇ") ("nyo" "にょ")
     ("hya" "ひゃ") ("hyi" "ひぃ") ("hyu" "ひゅ") ("hye" "ひぇ") ("hyo" "ひょ")
     ("mya" "みゃ") ("myi" "みぃ") ("myu" "みゅ") ("mye" "みぇ") ("myo" "みょ")
     ("rya" "りゃ") ("ryi" "りぃ") ("ryu" "りゅ") ("rye" "りぇ") ("ryo" "りょ")

     ( "fa" "ふぁ") ( "fi" "ふぃ") ( "fu" "ふ"  ) ( "fe" "ふぇ") ( "fo" "ふぉ")

     ;; 濁音、半濁音 + 拗音
     ("gya" "ぎゃ") ("gyi" "ぎぃ") ("gyu" "ぎゅ") ("gye" "ぎぇ") ("gyo" "ぎょ")
     ("zya" "じゃ") ("zyi" "じぃ") ("zyu" "じゅ") ("zye" "じぇ") ("zyo" "じょ")
     ( "ja" "じゃ") ( "ji" "じ"  ) ( "ju" "じゅ") ( "je" "じぇ") ( "jo" "じょ")
     ("dya" "ぢゃ") ("dyi" "ぢぃ") ("dyu" "ぢゅ") ("dye" "ぢぇ") ("dyo" "ぢょ")
     ("bya" "びゃ") ("byi" "びぃ") ("byu" "びゅ") ("bye" "びぇ") ("byo" "びょ")
     ("pya" "ぴゃ") ("pyi" "ぴぃ") ("pyu" "ぴゅ") ("pye" "ぴぇ") ("pyo" "ぴょ")

     ( "va" "ゔぁ") ( "vi" "ゔぃ") ( "vu" "ゔ"  ) ( "ve" "ゔぇ") ( "vo" "ゔぉ")

     ;; 小書き文字
     ( "xa" "ぁ") ( "xi" "ぃ") ( "xu" "ぅ") ( "xe" "ぇ") ( "xo" "ぉ")
     ("xka" "ゕ")              ("xke" "ゖ")
                               ("xtu" "っ")
     ("xya" "ゃ")              ("xyu" "ゅ")              ("xyo" "ょ")

     ;; 記号
     ("," "、")
     ("." "。")
     ("-" "ー")
     ("~" "〜")
     ("!" "！")
     ("?" "？")
     ("/" "・")
     ("[" "「")
     ("]" "」")
     ))

;; 木構造構築
(mapc (lambda (entry)
        (apply #'reskk-insert-node entry))
  reskk-tree-entries)

(dolist (char '("k" "s" "t" "h" "m" "y" "r" "w" "g" "z" "d" "b" "p" "f" "j" "v"))
  (reskk-insert-something-node reskk-tree (concat char char) "っ" char)
  )

(provide 'reskk-tree)
