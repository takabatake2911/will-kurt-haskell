# JavaScriptにおけるグローバル汚染とは？その原因と防止方法

JavaScriptでプログラミングしていると、意図せず**グローバル変数**を作ってしまい、他のコードと名前が衝突することがあります。これを**グローバル汚染（Global Pollution）**と呼びます。特にブラウザで複数のスクリプトを組み合わせる場合や、ライブラリ同士を使う場合に問題になりやすいです。

---

## 1. グローバル汚染の例

JavaScriptでグローバル汚染が起きる典型的なパターン：

```javascript
// 何もつけずに変数を宣言 → 自動でグローバル
myVar = 42;

function greet() {
    console.log("Hello, world!");
}

console.log(window.myVar); // 42（ブラウザの場合）
```

* `myVar` は意図せず **`window` オブジェクトのプロパティ** になっています。
* もし他のスクリプトでも `myVar` を使っていたら上書きされ、バグの原因になります。

---

## 2. なぜグローバル汚染は問題か？

1. **名前衝突のリスク**
   複数のスクリプトで同じ名前を使うと、後から読み込んだスクリプトが前の値を上書きしてしまう。

2. **メンテナンスが難しくなる**
   どこで変数が定義されたのか追いづらくなる。

3. **セキュリティ上のリスク**
   グローバルに公開されるため、外部スクリプトから意図せず変更される可能性がある。

---

## 3. グローバル汚染を防ぐ方法

### 3-1. ES6モジュールを使う（最もおすすめ）

```javascript
// module.js
export const name = "Hiroki";
export function greet() {
    console.log(`Hello, ${name}`);
}

// main.js
import { greet } from './module.js';
greet(); // Hello, Hiroki
```

* モジュール内の変数は **自動的にローカルスコープ**。
* 他のスクリプトと名前が衝突する心配なし。

> モダンな開発ではこれが最も安全で一般的な方法です。

---

### 3-2. ブロックスコープで変数を定義する

```javascript
{
    let localVar = 10;
    const localConst = 20;
    console.log(localVar + localConst); // 30
}

console.log(typeof localVar); // undefined
```

* `let` / `const` は **ブロック内だけ有効**。
* 小規模スクリプトでも有効。

---

### 3-3. IIFE（Immediately Invoked Function Expression）を使う

```javascript
(function() {
    var privateVar = "I am private";
    console.log(privateVar);
})();

console.log(typeof privateVar); // undefined
```

* 関数を即時実行することで **スコープを作る**。
* 旧来のブラウザ対応やレガシーコードで使われる。

---

### 3-4. 名前空間パターンを使う

```javascript
const MyApp = {
    version: "1.0",
    greet() { console.log("Hello from MyApp!"); }
};

MyApp.greet(); // Hello from MyApp!
```

* 1つのグローバル変数にまとめることで **衝突を最小化**。

---

## 4. おすすめの習慣

1. **グローバル変数はなるべく使わない**。
2. **ES6モジュールを基本**としてコードを書く。
3. 小さなスクリプトなら `let` / `const` でスコープを限定。
4. レガシーコードでは IIFE や名前空間パターンで安全性を確保。

---

## まとめ

* グローバル汚染は **名前衝突やバグの温床** です。
* 現代のJavaScriptでは **ES6モジュール + `let` / `const`** が最も安全で簡単な方法。
* レガシーコードでは IIFE や名前空間パターンが有効。

