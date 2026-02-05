# ファーストクラス関数：JavaScriptとHaskellの比較

プログラミング言語における重要な概念の一つに「ファーストクラス関数（first-class functions）」があります。
これは特に関数型プログラミングを理解する上で欠かせない考え方であり、HaskellだけでなくJavaScriptでも中心的な役割を持っています。

本記事では、ファーストクラス関数とは何かを説明し、JavaScriptとHaskellでどのように使われているかを比較します。

---

## 1. ファーストクラス関数とは何か

ファーストクラス関数とは、

> 関数を「値」として扱える性質

のことです。

具体的には、次のことができる関数を指します：

* 変数に代入できる
* 引数として渡せる
* 戻り値として返せる
* データ構造に格納できる

この性質があると、プログラムの抽象化や再利用性が大きく向上します。

---

## 2. JavaScriptにおけるファーストクラス関数

JavaScriptでは関数はオブジェクトの一種であり、完全にファーストクラスです。

### 変数に代入

```javascript
const inc = function(x) {
  return x + 1;
};

console.log(inc(5)); // 6
```

またはアロー関数：

```javascript
const inc = x => x + 1;
```

---

### 引数として渡す

```javascript
function apply(f, x) {
  return f(x);
}

apply(inc, 5); // 6
```

ここでは、`inc` が「値」として `apply` に渡されています。

---

### 戻り値として返す

```javascript
function makeAdder(n) {
  return function(x) {
    return x + n;
  };
}

const add3 = makeAdder(3);
console.log(add3(5)); // 8
```

関数が関数を生成しています。

---

### 特徴（JavaScript）

* 関数はオブジェクト
* 状態（クロージャ）を持てる
* イベント処理や非同期処理で多用される

例：

```javascript
setTimeout(() => console.log("Hello"), 1000);
```

---

## 3. Haskellにおけるファーストクラス関数

Haskellでは、関数は「特別な存在」ではなく、完全に通常の値として扱われます。
むしろ、言語の中心そのものです。

### 変数に代入

```haskell
inc :: Int -> Int
inc = \x -> x + 1

inc 5  -- 6
```

---

### 引数として渡す

```haskell
apply :: (Int -> Int) -> Int -> Int
apply f x = f x

apply inc 5  -- 6
```

---

### 戻り値として返す

```haskell
makeAdder :: Int -> (Int -> Int)
makeAdder n = \x -> x + n

add3 = makeAdder 3
add3 5  -- 8
```

---

### 特徴（Haskell）

* すべての関数がファーストクラス
* 副作用が基本的にない（純粋関数）
* 関数を組み合わせる文化が非常に強い
* 部分適用が自然に起きる

例：

```haskell
map (*2) [1,2,3]
-- [2,4,6]
```

ここでは `(*2)` という「関数そのもの」を渡しています。

---

## 4. JavaScriptとHaskellの違い

| 観点        | JavaScript | Haskell |
| --------- | ---------- | ------- |
| 関数の扱い     | オブジェクト     | 純粋な値    |
| ファーストクラス性 | あり         | 完全にあり   |
| 副作用       | あり         | 基本なし    |
| 型         | 動的型付け      | 静的型付け   |
| 関数の役割     | 機能の一部      | 言語の中心   |

---

## 5. 設計思想の違い

### JavaScript

* ファーストクラス関数は「便利な道具」
* イベント処理・コールバック・非同期処理に活躍
* OOPと関数型の混合スタイル

### Haskell

* ファーストクラス関数は「言語の基盤」
* すべてが関数の組み合わせで構成される
* 抽象化の主役

---

## 6. 共通点：高階関数が使える

両言語とも、ファーストクラス関数のおかげで高階関数が書けます。

### JavaScript

```javascript
[1,2,3].map(x => x * 2);
```

### Haskell

```haskell
map (*2) [1,2,3]
```

同じ発想で書かれていることが分かります。

---

## 7. 重要な理解ポイント

ファーストクラス関数があると：

* 共通パターンを抽象化できる
* コードの重複が減る
* 柔軟な設計ができる

例えばHaskellの例：

```haskell
ifEven :: (Int -> Int) -> Int -> Int
ifEven f x = if even x then f x else x
```

ここで `f` は関数そのものです。
これはファーストクラス関数でなければ実現できません。

---

## 8. まとめ

* ファーストクラス関数とは「関数を値として扱える」性質。
* JavaScriptもHaskellもこの性質を持っている。
* ただし、

  * JavaScript：便利な機能の一つ
  * Haskell：言語の核心

という違いがある。

特にHaskellでは、

> 「関数を渡す」ことが普通のプログラミングスタイル

になります。

この違いを理解すると、Haskellの設計思想や、関数型プログラミングの本質が見えてきます。
