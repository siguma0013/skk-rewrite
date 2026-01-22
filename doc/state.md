# 内部ステータス

## SKKモード

``` mermaid
stateDiagram-v2
  half_alphabet : HALF-ALPHABET

  half_alphabet --> HIRAGANA

  HIRAGANA --> half_alphabet
  HIRAGANA --> KATAKANA

  KATAKANA --> half_alphabet
  KATAKANA --> HIRAGANA
```

## SKK変換モード

どこからでも離脱可能

``` mermaid
stateDiagram-v2
  convert_okurigana : CONVERT-OKURIGANA

  [*] --> CONVERT

  CONVERT --> convert_okurigana
  CONVERT --> SELECT

  convert_okurigana --> SELECT

  SELECT --> [*]
```
