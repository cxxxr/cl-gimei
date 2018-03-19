# cl-gimei
Common Lisp port of [gimei](https://github.com/willnet/gimei)

## Usage
```common-lisp
(let ((name (gimei:make-name)))
  (hiragana name)               ;=> "さいとう はるな"
  (katakana name)               ;=> "サイトウ ハルナ"
  (kanji (last-name name))      ;=> "斎藤"
  (hiragana (last-name name))   ;=> "さいとう"
  (katakana (last-name name))   ;=> "サイトウ"
  (kanji (first-name name))     ;=> "陽菜"
  (hiragana (first-name name))  ;=> "はるな"
  (katakana (first-name name))) ;=> "ハルナ"

(let ((male (gimei:make-male)))
  (malep male)    ;=> t
  (femalep male)) ;=> nil

(let ((female (gimei:make-female)))
  (malep female)    ;=> nil
  (femalep female)) ;=> t

(let ((address (gimei:make-address)))
  (kanji address)                  ;=> 岡山県大島郡大和村稲木町
  (hiragana address)               ;=> おかやまけんおおしまぐんやまとそんいなぎちょう
  (katakana address)               ;=> オカヤマケンオオシマグンヤマトソンイナギチョウ
  (kanji (prefecture address))     ;=> 岡山県
  (hiragana (prefecture address))  ;=> おかやまけん
  (katakana (prefecture address))  ;=> オカヤマケン
  (kanji (city address))           ;=> 大島郡大和村
  (hiragana (city address))        ;=> おおしまぐんやまとそん
  (katakana (city address))        ;=> オオシマグンヤマトソン
  (kanji (town address))           ;=> 稲木町
  (hiragana (town address))        ;=> いなぎちょう
  (katakana (town address)))       ;=> イナギチョウ
```

## License
MIT
