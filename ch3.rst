.. highlight:: cl
   :linenothreshold: 0

Chapter 3 列表 (Lists)
**********************************

列表是 Lisp 中基本的資料結構之一。在最早的 Lisp 方言中，他們是唯一的資料結構： Lisp 這個名字本來代表的是 "LISt Processor" 。但 Lisp 已經超越這個縮寫很久了。 Common Lisp 是一個有著各式各樣資料結構的通用性程式語言 (general-purpose programming language)。

Lisp 程式開發通常呼應著開發 Lisp 語言自身。在最初版本的 Lisp 程式，你可能使用很多列表。然而之後的版本，你可能切換到快速、特定的資料結構。本章描述了你可以用列表所做的很多事情，以及使用它們來演示一些普遍的 Lisp 概念。

3.1 創建 (Conses)
====================

在2.4節我們介紹了 ``cons`` , ``car`` , 以及 ``cdr`` ，基本的 List 操作函數 (list-manipulation fuctions)。 ``cons`` 真正所做的事情是，把兩個物件結合成一個有兩部分的物件，稱之為 *Cons* 物件。概念上來說，一個 cons 是一對指標; 第一個是 car ，第二個是 cdr。

Cons 物件提供了一個方便的表示法來表示任何型態的物件。一個 Cons 物件裡的一對指標可以指向任何種類的物件，包括 Cons 物件本身。它利用到我們之後可以用 ``cons`` 來創建列表的可能性。

我們往往不會把列表想成是成對的，但它們可以這樣被定義。任何非空的列表，都可以被視為一對由列表第一個元素及列表其餘元素所組成的列表。 Lisp 列表體現了這個概念。我們使用 cons 的一半 (car)來指向列表的第一個元素，然後用另一半 (cdr)指向列表其餘的元素 (可能是別的 Cons 物件或 nil)。 Lisp 的慣例是使用 ``car`` 代表列表的第一個元素，而用 ``cdr`` 代表列表的其餘的元素。所以現在 ``car`` 是列表的第一個元素的同義詞，而 ``cdr`` 是列表的其餘的元素的同義詞。列表不是不同的物件，而是像 Cons 這樣的方式連結起來。

當我們想在 ``NIL`` 上面建立東西時，

::

   > (setf x (cons 'a nil))
   (A)

\

.. figure:: https://dl-web.dropbox.com/get/Juanito/acl-images/Figure-3.1.png?w=903fbe8f
    
圖 3.1 一個元素的列表

產生的列表由一個 Cons 所組成，見圖3.1。這種表達 Cons 的方式叫做 箱子標示法 (box notation)，因為每一個 Cons 是用一個箱子表示，內含一個 ``car`` 和 ``cdr`` 的指標。當我們呼叫 ``car`` 與 ``cdr`` 時，我們得到指標指向的地方：

::
   
   > (car x)
   A
   > (cdr x)
   NIL

當我們創建一個多元素的列表時，我們得到一串 Conses (a chain of conses)：

::

   > (setf y (list 'a 'b 'c))
   (A B C)

產生的結構見圖3.2。現在當我們想得到這個列表的 ``cdr`` 時，它是一個兩個元素的列表。

.. figure:: https://dl-web.dropbox.com/get/Juanito/acl-images/Figure-3.2.png?w=b42e4db9
   
圖 3.2 三個元素的列表

::

   > (cdr y)
   (B C)

在一個有多個元素的列表中， ``car`` 指標讓你取得元素，而 ``cdr`` 讓你取得列表內其餘的東西。

一個列表可以有任何種類的物件作為元素，包括另一個列表：

::

   > (setf z (list 'a (list 'b 'c) 'd'))
   (A (B C) D)

當這種情況發生時，它的結構如圖3.3所示; 第二個 Cons 的 ``car`` 指標也指向一個列表：

::

  > (car (cdr z))
  (B C)

.. figure:: https://dl-web.dropbox.com/get/Juanito/acl-images/Figure-3.3.png?w=10d193e0
    
圖 3.3 巢狀列表


前兩個我們創建的列表都有三個元素; 只不過 ``z`` 列表的第二個元素也剛好是一個列表。像這樣的列表稱為 *巢狀* 列表，而像 ``y`` 這樣的列表稱之為 *平坦* 列表 ( *flat* list)。

如果引數是一個 Cons 物件，函數 ``consp`` 回傳真。所以我們可以這樣定義 ``listp`` ：

::

  (defun our-listp (x)
  	(or (null x) (consp x)))

因為所有不是 Cons 物件的東西就是一個原子 (atom)，判斷式 ``atom`` 可以這樣定義：

::

   (defun our-atom (x) (not (consp x)))

注意， ``NIL`` 是一個原子，同時也是一個列表。

3.2 等式 (Equality)
=====================

每一次你呼叫 ``cons`` 時， Lisp 會分配一塊新的記憶體給兩個指標。所以如果我們用同樣的引數呼叫 ``cons`` 兩次，我們得到兩個數值看起來一樣，但實際上是兩個不同的物件：

::

   > (eql (cons 'a nil) (cons 'a nil))
   NIL

如果我們也可以詢問兩個列表是否有相同元素，那就很方便了。 Common Lisp 提供了這種目的另一個判斷式： ``equal`` 。而另一方面 ``eql`` 只有在它的引數是相同物件時才回傳真，

::

   > (setf x (cons 'a nil))
   (A)
   > (eql x x)
   T
  
本質上 ``equal`` 若它的引數列印出的值相同時，回傳真：

::

   > (equal x (cons 'a nil))
   T

這個判斷式對非列表結構的別種物件也有效，但一種僅對列表有效的版本可以這樣定義：

::

   > (defun our-equal (x y)
       (or (eql x y)
           (and (consp x)
                (consp y)
                (our-equal (car x) (car y))
                (our-equal (cdr x) (cdr y)))))

這個定義意味著，如果某個 x 和 y 相等 ( ``eql`` )，那麼他們也相等 ( ``equal`` )。

3.3 為什麼Lisp沒有指標 (Why Lisp Has No Pointers)
=======================================================

一個理解 Lisp 的祕密之一是意識到變數是有值的，就像列表有元素一樣。如同 Conses 物件有指標指向他們的元素，變數有指標指向他們的值。

你可能在別的語言中使用過顯示指標 (explicitly pointer)。在 Lisp ，你永遠不用這麼做，因為語言幫你處理好指標了。我們已經在列表看過這是怎麼達成的。同樣的事情發生在變數身上。舉例來說，假設我們想要把兩個變數設成同樣的列表：

::

   > (setf x '(a b c))
   (A B C)
   > (setf y x)
   (A B C)

.. figure:: https://dl-web.dropbox.com/get/Juanito/acl-images/Figure-3.4.png?w=72840b32
    
圖3.4 兩個變數設為相同的列表

當我們把 x 的值賦給 y 時，究竟發生什麼事呢？記憶體中與 x 有關的位置並沒有包含這個列表，而是一個指標指向它。當我們給 y 賦一個相同的值時， Lisp 複製的是指標，而不是列表。（圖 3.4 顯示賦值 x 給 y 後的結果）所以無論何時你把一個變數賦給另一個變數時，兩個變數會有 ``eql`` 的值。

::

   > (eql x y)
   T

Lisp 沒有指標的原因是因為每一個值，其實概念上來說都是一個指標。當你賦一個值給變數或將這個值存在資料結構中，其實被儲存的是指向這個值的指標。當你要取得變數的值，或是存在資料結構中的內容時， Lisp 回傳指向這個值的指標。但這都在檯面下發生。你可以不加思索地把值放在結構裡，或放 "在" 變數裡。

為了效率的原因， Lisp 有時會選擇一個折衷的表示法，而不是指標。舉例來說，因為一個小整數所需的記憶體空間，少於一個指標所需的空間，一個 Lisp 實現可能會直接處理這個小整數，而不是用指標來處理。但基本要點是，程式設計師，預設上，你可以把任何東西放在任何地方。除非你宣告你不願這麼做，不然你能夠在任何的資料結構，存放任何種類的物件，包括結構本身。

3.4 建立列表 (Building Lists)
=================================

.. figure:: https://dl-web.dropbox.com/get/Juanito/acl-images/Figure-3.5.png?w=d1e830b3

圖 3.5 複製的結果

函數 ``copy-list`` 接受一個列表，然後返回此列表的副本。新的列表會有同樣的元素，但是裝在新的 Conses 物件裡：

::

   > (setf x '(a b c))
           y (copy-list x))
   (A B C)

圖 3.5 展示出結果的結構; 回傳值像是有著相同乘客的新公車。我們可以把 ``copy-list`` 想成是這麼定義的:

::

   (defun our-copy-list (lst)
     (if (atom list)
         lst
         (cons (car lst) (our-copy-list (cdr lst)))))

這個定義暗示著 x 與 (copy-list x) 會永遠 ``equal`` ，並永遠不  ``eql`` ，除非 x 是 ``NIL`` 。

最後，函數 ``append`` 回傳任何數目的列表串接 (concatenation)：

::

   > (append '(a b) '(c d) 'e)
   (A B C D E)

通過這麼做，它複製所有的引數，除了最後一個。

3.5 範例：壓縮 (Example: Compression)
============================================

作為一個範例，這節將演示如何實現簡單形式的列表壓縮。這個演算法有一個令人映像深刻的名字， *遊程編碼* 。

::

   (defun compress (x)
     (if (consp x)
         (compr (car x) 1 (cdr x))
         x))

   (defun compr (elt n lst)
     (if (null lst)
         (list (n-elts elt n))
         (let ((next (car lst)))
           (if (eql next elt)
               (compr elt (+ n 1) (cdr lst))
               (cons (n-elts elt n)
                     (compr next 1 (cdr lst)))))))

   (defun n-elts (elt n)
     (if (> n 1)
         (list n elt)
         elt))

圖 3.6 遊程編碼 (Run-length encoding)：壓縮


在餐廳，這個演算法的工作方式如下。一個女服務生走向有四個客人的桌子。"你們要什麼？" 她問。"我要特餐，" 第一個客人說。
"我也是，" 第二個客人說。"聽起來不錯，" 第三個客人說。每個人看著第四個客人。¨我要一個 "cilantro soufflé，" 他小聲地說。 (譯註：蛋奶酥上面灑香菜跟醬料）

瞬息之間，女服務生就轉身踩著高跟鞋走回櫃檯去了。"三個特餐，" 她大聲對廚師說，"還有一個香菜蛋奶酥。"

圖 3.6 展示了如何實現這個壓縮列表演算法。函數 ``compress`` 接受一個由原子組成的列表，然後回傳一個壓縮的列表：

::

   > (compress '(1 1 1 0 1 0 0 0 0 1))
   ((3 1) 0 1 (4 0) 1)

當相同的元素連續出現好幾次，這個連續出現的序列被一個列表取代，列表指明出現的次數及出現的元素。

主要的工作是由 遞迴的 ``compr`` 所完成。這個函數接受三個引數： ``elt`` ，上一個我們看過的元素; ``n`` ，連續出現的次數， 以及 ``lst`` ，我們還沒檢視過的部分列表。如果沒有東西需要檢視了，我們呼叫 ``n-elts`` 來取得 n elts 的表示法。如果 ``lst`` 的第一個元素還是 ``elt`` ，我們增加出現的次數 ``n``並繼續下去。否則我們得到，到目前為止的一個壓縮的列表，然後 ``cons`` 這個列表在 ``compr`` 處理完剩下的列表所回傳的東西之上。

要復原一個壓縮的列表，我們呼叫 ``uncompress`` (圖 3.7)

::

   > (uncompress '((3 1) 0 1 (4 0) 1))
   (1 1 1 0 1 0 0 0 0 1)

::

   (defun uncompress (lst)
     (if (null lst)
         nil
         (let ((elt (car lst))
               (rest (uncompress (cdr lst))))
           (if (consp elt)
               (append (apply #'list-of elt)
                       rest)
               (cons elt rest)))))

   (defun list-of (n elt)
     (if (zerop n)
         nil
         (cons elt (list-of (- n 1) elt))))

圖 3.7 遊程編碼 (Run-length encoding)：解壓縮


這個函數遞迴地遍歷這個壓縮列表，逐字複製原子並呼叫 ``list-of`` ，展開成列表。

::

   > (list-of 3 'ho)
   (HO HO HO)

我們其實不需要自己寫 ``list-of`` 。內建的 ``make-list`` 可以辦到一樣的事情─但它使用了我們還沒介紹到的關鍵字引數 (keyword argument)。

圖 3.6 跟 3.7 這種寫法不是一個有經驗的 Lisp 程式設計師用的寫法。它的效率很差，它沒有盡可能的壓縮，而且它只對由原子組成的列表有效。在幾個章節內，我們會學到解決這些問題的技巧。

::

   載入程式

   在這節的程式是我們第一個實質的程式。
   當我們想要寫超過數行的函數時，
   通常我們會把程式寫在一個檔案，
   然後使用 load 讓 Lisp 讀取函數的定義。
   如果我們把圖 3.6 跟 3.7 的程式，
   存在一個檔案叫做，"compress.lisp" 然後輸入

   (load "compress.lisp")

   到頂層，或多或少的，
   我們會像在直接輸入頂層一樣得到同樣的效果。

   注意：在某些實現中，Lisp 檔案的副檔名會是 ".lsp" 而不是 ".lisp"。

3.6 存取 (Access)
======================

Common Lisp 有額外的存取函數，它們是用 ``car`` 跟 ``cdr`` 所定義的。要找到列表特定位置的元素，我們可以呼叫 ``nth`` ，

::

   > (nth 0 '(a b c))
   A

而要找到第 n 個 cdr ，我們呼叫 ``nthcdr``：

::

   > (nthcdr 2 '(a b c))
   (C)

``nth`` 與 ``nthcdr`` 都是零索引的 (zero-indexed); 即元素從 0 開始編號，而不是從 1 開始。在 Common Lisp 裡，無論何時你使用一個數字來參照一個資料結構中的元素時，都是從 0 開始編號的。

兩個函數幾乎做一樣的事; ``nth`` 等同於取 ``nthcdr`` 的 ``car`` 。沒有檢查錯誤的情況下， ``nthcdr`` 可以這麼定義： 

::

   (defun our-nthcdr (n lst)
     (if (zerop n)
         lst
         (our-nthcdr (- n 1) (cdr lst))))

函數 ``zerop`` 僅在引數為零時，才回傳真。

函數 ``last`` 回傳列表的最後一個 Cons 物件：

::
   
   > (last '(a b c))
   (C)

這跟取得最後一個元素不一樣。要取得列表的最後一個元素，你要取得 ``last`` 的 ``car`` 。

Common Lisp 定義了函數 ``first`` 直到 ``tenth`` 可以取得列表對應的元素。這些函數不是 *零索引的* (zero-indexed)：

``(second x)`` 等同於 ``(nth 1 x)`` 。

此外， Common Lisp 定義了像是 ``caddr`` 這樣的函數，它是 cdr 的 cdr 的 car 的縮寫 (car of cdr of cdr)。所有這樣形式的函數 ``cxr`` ，其中 x 是一個字串，最多四個 a 或 d ，在 Common Lisp 裡都被定義好了。使用 ``cadr`` 可能會有例外 (exception)產生，這不是一個好主意，在所有人都可能會讀的程式碼裡來使用這樣的函數。

3.7 映成函數 (Mapping Functions)
============================================

Common Lisp 提供了數個函數來對一個列表的元素做函數呼叫。最常使用的是 ``mapcar`` ，接受一個函數與一個或多個列表，並回傳把函數應用至每個列表的元素的結果，直到有的列表沒有元素為止：

::

   > (mapcar #'(lambda (x) (+ x 10))
             '(1 2 3))
   (11 12 13)

   > (mapcar #'list
             '(a b c)
             '(1 2 3 4))
   ((A 1) (B 2) (C 3)) 

相關的 ``maplist`` 接受同樣的引數，將列表的漸進的下一個 cdr 傳入函數。

::

   > (maplist #'(lambda (x) x)
              '(a b c))
   ((A B C) (B C) (C))

其它的映成函數，包括 ``mapc`` 我們在 88 頁討論，以及 ``mapcan`` 在 202 頁討論。

3.8 樹 (Trees)
======================

Conses 物件可以想成是二元樹， ``car`` 代表右子樹，而 ``cdr`` 代表左子樹。舉例來說，列表

(a (b c) d) 也是一棵由圖 3.8 所代表的樹。（如果你逆時針旋轉45度，你會發現跟圖 3.3 一模一樣）

.. figure:: https://dl-web.dropbox.com/get/Juanito/acl-images/Figure-3.8.png?w=a854542a

圖 3.8 二元樹 (Binary Tree)

Common Lisp 有幾個內建的給樹使用的函數。舉例來說， ``copy-tree`` 接受一個樹，並回傳一份副本。它可以這麼定義：

::

   (defun our-copy-tree (tr)
     (if (atom tr)
          tr
          (cons (our-copy-tree (car tr))
                (our-copy-tree (cdr tr)))))

把這跟 36 頁的 ``copy-list`` 比較一下; ``copy-tree`` 複製每一個 Cons 物件的 car 與 cdr，而 ``copy-list`` 僅複製 cdr。

沒有內部節點的二元樹沒有太大的用處。 Common Lisp 包含了操作樹的函數，不只是因為我們需要樹這個結構，而是因為我們需要一種方法，來操作列表及所有內部的列表。舉例來說，假設我們有一個這樣的列表：

::

   (and (integerp x) (zerop (mod x 2)))

而我們想要把各處的 x 都換成 y 。呼叫 ``substitute`` 是不行的，它只能替換序列 (sequence)中的元素：

:: 

   > (substitute 'x 'y '(and (integerp x) (zerop (mod x 2))))
   (AND (INTEGERP X)(ZEROP (MOD X 2)))

這個呼叫是無效的，因為列表有三個元素，沒有一個元素是 x 。我們在這所需要的是 ``subst`` ，它替換樹中的元素。

::

   > (subst 'x 'y '(and (integerp x) (zerop (mod x 2))))
   (AND (INTEGERP Y)(ZEROP (MOD Y 2)))

如果我們定義一個 ``subst`` 的版本，它看起來跟 ``copy-tree`` 很相似：

::

   > (defun our-subst (new old tree)
       (if (eql tree old)
           new
           (if (atom tree)
               tree
               (cons (our-subst new old (car tree))
                     (our-subst new old (cdr tree))))))

操作樹的函數通常有這種形式，car 與 cdr 同時做遞迴。這種函數被稱之為是 *雙重遞迴* (doubly recursive)。

3.9 理解遞迴 (Understanding Recursion)
============================================

學生在學習遞迴時，有時候是被鼓勵在紙上追蹤 (trace)遞迴程式調用 (invocation)的過程。 （ 288頁（譯註：Appendix A Trace and Backtraces）可以看到一個遞迴函數的追蹤過程。）但這種練習可能會誤導你：一個程式設計師在定義一個遞迴函數時，通常不會明確地去想呼叫函數後，函數調用的順序是什麼。

如果一個人總是需要這樣子思考程式，遞迴會是艱難的、沒有幫助的。遞迴的優點是它精確地讓我們更抽象地來檢視演算法。你不需要考慮真正呼叫函數時所有的調用 (invocation)過程，就可以判斷一個遞迴函數是否是正確的。

要知道一個遞迴函數是否做它該做的事，你只需要問，它包含了所有的情況嗎？舉例來說，下面是一個尋找列表長度的遞迴函數：

::

   > (defun len (lst)
       (if (null lst)
           0
           (+ (len (cdr lst)) 1)))

我們可以藉由檢查兩件事情，來確信這個函數是正確的：

1. 對長度為 0 的列表是有效的。
2. 給定它對於長度為 n 的列表是有效的，它對長度是 n+1 的列表也是有效的。

如果這兩點是成立的，我們知道這個函數對於所有可能的列表都是正確的。

我們的定義顯然地滿足第一點：如果 列表 ( ``lst`` ) 是空的 ( ``nil`` )，函數直接回傳 0。現在假定我們的函數對長度為 n 的列表是有效的。我們給它一個 n+1 長度的列表。這個定義說明了，函數會回傳列表的 cdr 的長度再加上 1。 cdr 是一個長度為 n 的列表。我們經由假定可知它的長度是 n。所以整個列表的長度是 n+1。

我們需要知道的就是這些。理解遞迴的祕密就像是處理括號一樣。你怎麼知道哪個括號對上哪個？你不需要這麼做。你怎麼想像那些調用過程？你不需要這麼做。

更複雜的遞歸函數，可能會有更多的情況需要討論，但是流程是一樣的。舉例來說， 41 頁的 ``our-copy-tree``，我們需要討論三個情況： 原子，單一的 Cons 物件， n+1 的 Cons 樹。

第一個情況（長度零的列表）稱之為 *基本情況* ( *base case* )。當一個遞迴函數不像你想的那樣工作時，通常是因為基本情況是錯的。下面這個不正確 ``member``定義，是一個常見的錯誤，整個忽略了基本情況：

::

   (defun our-member (obj lst)
     (if (eql (car lst) obj)
         lst
         (our-member obj (cdr lst))))

我們需要初始一個 ``null`` 測試，確保在到達列表底部時，沒有找到目標要停止遞迴。如果我們要找的物件沒有在列表裡，這個版本的 ``member`` 會陷入無窮迴圈。附錄 A 更詳細地檢視了這種問題。

能夠判斷一個遞迴函數是否正確只不過是理解遞迴的上半場，下半場是能夠寫出一個做你想做的事情的遞迴函數。 6.9 節討論了這個問題。

3.10 集合 (Sets)
======================

列表是表示小集合的好方法。列表中的每個元素都代表了一個集合的成員：

::

   > (member 'b '(a b c))
   (B C)

當 ``member`` 要回傳"真"時，與其僅僅回傳 ``t`` ，它會

3.11 序列 (Sequences)
=================================

3.12 堆疊 (Stacks)
=================================

3.13 點列表 (Dotted Lists)
=================================

3.14 關連列表 (Assoc-lists)
===================================

3.15 範例：最短路徑 (Example: Shortest Path)
==================================================

3.16 垃圾 (Garbages)
=========================

Chapter 3 總結 (Summary)
================================ 

Chapter 3 練習 (Exercises)
==================================