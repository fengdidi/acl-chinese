.. highlight:: cl
   :linenothreshold: 0

Chapter 3 列表 (Lists)
**********************************

列表是 Lisp 中基本的数据结构之一。在最早的 Lisp 方言中，他们是唯一的数据结构： Lisp 这个名字本来代表的是 "LISt Processor" 。但 Lisp 已经超越这个缩写很久了。 Common Lisp 是一个有着各式各样数据结构的通用性程序语言(general-purpose programming language)。

Lisp 程序开发通常呼应着开发 Lisp 语言自身。在最初版本的 Lisp 程序，你可能使用很多列表。然而之后的版本，你可能换到快速、特定的数据结构。本章描述了你可以用列表所做的很多事情，以​​及使用它们来演示一些普遍的 Lisp 概念。

3.1 构建 (Conses)
====================

在2.4节我们介绍了 ``cons`` , ``car`` , 以及 ``cdr`` ，基本的 List 操作函数 (list-manipulation fuctions)。 ``cons`` 真正所做的事情是，把两个对象结合成一个有两部分的对象，称之为 *Cons* 对象。概念上来说，一个 cons 是一对指针; 第一个是car ，第二个是cdr。

Cons 对象提供了一个方便的表示法来表示任何类型的对象。一个 Cons 对象里的一对指针可以指向任何类型的对象，包括 Cons 对象本身。它利用到我们之后可以用 ``cons`` 来构建列表的可能性。

我们往往不会把列表想成是成对的，但它们可以这样被定义。任何非空的列表，都可以被视为一对由列表第一个元素及列表其余元素所组成的列表。 Lisp 列表体现了这个概念。我们使用 cons 的一半来指向列表的第一个元素，然后用另一半指向列表其余的元素(可能是别的 Cons 或nil)。 Lisp 的惯例是使用 ``car`` 代表列表的第一个元素，而用 ``cdr`` 代表列表的其余的元素。所以现在 ``car`` 是列表的第一个元素的同义词，而 ``cdr`` 是列表的其余的元素的同义词。列表不是不同的对象，而是像 Cons 这样的方式连结起来。

当我们想在 ``NIL`` 上面建立东西时，

::

   > (setf x (cons 'a nil))
   (A)

\

.. figure:: https://dl-web.dropbox.com/get/Juanito/acl-images/Figure-3.1.png?w=903fbe8f
    
图3.1 一个元素的列表

产生的列表由一个 Cons 所组成，见图3.1。这种表达 Cons 的方式叫做箱子标示法 (box notation)，因为每一个 Cons 是用一个箱子表示，内含一个 ``car`` 和 ``cdr`` 的指针。当我们调用 ``car`` 与 ``cdr`` 时，我们得到指针指向的地方：

::
   
   > (car x)
   A
   > (cdr x)
   NIL

当我们构建一个多元素的列表时，我们得到一串 Conses (a chain of conses) ：

::

   > (setf y (list 'a 'b 'c))
   (A B C)

产生的结构见图3.2。现在当我们想得到这个列表的 ``cdr`` 时，它是一个两个元素的列表。

\

.. figure:: https://dl-web.dropbox.com/get/Juanito/acl-images/Figure-3.2.png?w=b42e4db9
   
图3.2 三个元素的列表

::

   > (cdr y)
   (B C)

在一个有多个元素的列表中， ``car`` 指针让你取得元素，而 ``cdr`` 让你取得列表内其余的东西。

一个列表可以有任何类型的对象作为元素，包括另一个列表：

::

   > (setf z (list 'a (list 'b 'c) 'd'))
   (A (B C) D)

当这种情况发生时，它的结构如图3.3所示; 第二个 Cons 的 ``car`` 指针也指向一个列表：

::

  > (car (cdr z))
  (B C)

\

.. figure:: https://dl-web.dropbox.com/get/Juanito/acl-images/Figure-3.3.png?w=10d193e0
    
图3.3 嵌套列表


前两个我们构建的列表都有三个元素; 只不过 ``z`` 列表的第二个元素也刚好是一个列表。像这样的列表称为 *嵌套* 列表，而像 ``y`` 这样的列表称之为 *平坦* 列表( *flat* list)。

如果自变量是一个 Cons 对象，函数 ``consp`` 返回真。所以我们可以这样定义 ``listp`` ：

::

  (defun our-listp (x)
  (or (null x) (consp x)))

因为所有不是 Cons 对象的东西就是一个原子 (atom)，判断式 ``atom`` 可以这样定义：

::

   (defun our-atom (x) (not (consp x)))

注意， ``NIL`` 是一个原子，同时也是一个列表。

3.2 等式 (Equality)
=====================

每一次你调用 ``cons`` 时， Lisp 会配置一块新的内存给两个指针。所以如果我们用同样的自变量调用 ``cons`` 两次，我们得到两个数值看起来一样，但实际上是两个不同的对象：

::

   > (eql (cons 'a nil) (cons 'a nil))
   NIL

如果我们也可以询问两个列表是否有相同元素，那就很方便了。 Common Lisp 提供了这种目的另一个判断式： ``equal`` 。而另一方面 ``eql`` 只有在它的自变量是相同对象时才返回真，

::

   > (setf x (cons 'a nil))
   (A)
   > (eql x x)
   T
  
本质上 ``equal`` 若它的自变量打印出的值相同时，返回真：

::

   > (equal x (cons 'a nil))
   T

这个判断式对非列表结构​​的别种对象也有效，但一种仅对列表有效的版本可以这样定义：

::

   > (defun our-equal (x y)
       (or (eql x y)
           (and (consp x)
                (consp y)
                (our-equal (car x) (car y))
                (our-equal (cdr x) (cdr y)))))

这个定义意味着，如果某个 x 和 y 相等( ``eql`` )，那么他们也相等( ``equal`` )。

3.3 为什么Lisp没有指针 (Why Lisp Has No Pointers)
=======================================================

一个理解 Lisp 的秘密之一是意识到变量是有值的，就像列表有元素一样。如同 Conses 对象有指针指向他们的元素，变量有指针指向他们的值。

你可能在别的语言中使用过显示指针 (explicitly pointer)。在 Lisp ，你永远不用这么做，因为语言帮你处理好指针了。我们已经在列表看过这是怎么实现的。同样的事情发生在变量身上。举例来说，假设我们想要把两个变量设成同样的列表：

::

    > (setf x '(a b c))
   (A B C)
   > (setf y x)
   (A B C)

.. figure:: https://dl-web.dropbox.com/get/Juanito/acl-images/Figure-3.4.png?w=72840b32
    
图 3.4 两个变量设为相同的列表

当我们把 x 的值 赋给 y 时，究竟发生什么事呢？内存中与 x 有关的位置并没有包含这个列表，而是一个指针指向它。当我们给 y 赋一个相同的值时， Lisp 复制的是指针，而不是列表。（图 3.4 显示赋值 x 给y 后的结果）所以无论何时你把一个变量赋给另一个变量时，两个变量会有 `` eql`` 的值。

:: 

   > (eql x y)
   T

Lisp 没有指针的原因是因为每一个值，其实概念上来说都是一个指针。当你赋一个值给变量或将这个值存在数据结构中，其实被储 存的是指向这个值的指针。当你要取得变量的值，或是存在数据结构中的内容时， Lisp 返回指向这个值的指针。但这都在台面下发生。你可以不加思索地把值放在结构里，或放 "在" 变量里。

为了效率的原因， Lisp  有时会选择一个折衷的表示法，而不是指针。举例来说，因为一个小整数所需的内存空间，少于一个指针所需的空间，一个 Lisp 实现可能会直接处理这个小整数，而不是用指针来处理。但基本要点是，程序员，预设你可以把任何东西放在任何地方。除非你声明你不愿这么做，不然你能够在任何的资料结构，存放任何类型的对象，包括结构本身。

3.4 建立列表 (Building Lists)
=================================

.. figure:: https://dl-web.dropbox.com/get/Juanito/acl-images/Figure-3.5.png?w=d1e830b3

图 3.5 复制的结果

函数 ``copy-list`` 接受一个列表，然后返回此列表的副本。新的列表会有同样的元素，但是装在新的 Conses 对象里：

::

   > (setf x '(a b c))
           y (copy-list x))
   (A B C)

图 3.5 展示出结果的结构; 返回值像是有着相同乘客的新公车。我们可以把 ``copy-list`` 想成是这么定义的:

::

   (defun our-copy-list (lst)
     (if (atom list)
         lst
         (cons (car lst) (our-copy-list (cdr lst)))))

这个定义暗示着 x 与 (copy-list x) 会永远 ``equal`` ，并永远不 ``eql`` ，除非 x 是 ``NIL`` 。

最后，函数 ``append`` 返回任何数目的列表串接(concatenation)：

::

   > (append '(a b) '(c d) 'e)
   (A B C D E)

通过这么做，它复制所有的自变量，除了最后一个

3.5 示例：压缩 (Example: Compression)
============================================

作为一个例子，这节将演示如何实现简单形式的列表压缩。这个算法有一个令人映像深刻的名字， *游程编码* 。

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
               (cons (n​​-elts elt n)
                     (compr next 1 (cdr lst)))))))

   (defun n-elts (elt n)
     (if (> n 1)
         (list n elt)
         elt))

图 3.6 游程编码 (Run-length encoding)：压缩

在餐厅，这个算法的工作方式如下。一个女服务生走向有四个客人的桌子。"你们要什么？" 她问。"我要特餐，" 第一个客人说。
"我也是，" 第二个客人说。"听起来不错，" 第三个客人说。每个人看着第四个客人。 ¨我要一个"cilantro soufflé，" 他小声地说。 (译注：蛋奶酥上面洒香菜跟酱料）

瞬息之间，女服务生就转身踩着高跟鞋走回柜台去了。 "三个特餐，" 她大声对厨师说，"还有一个香菜蛋奶酥。"

图 3.6 展示了如何实现这个压缩列表演算法。函数 ``compress`` 接受一个由原子组成的列表，然后返回一个压缩的列表：

::

   > (compress '(1 1 1 0 1 0 0 0 0 1))
   ((3 1) 0 1 (4 0) 1)

当相同的元素连续出现好几次，这个连续出现的序列被一个列表取代，列表指明出现的次数及出现的元素。

主要的工作是由递归的 ``compr`` 所完成。这个函数接受三个自变量： ``elt`` ， 上一个我们看过的元素; ``n`` ，连续出现的次数， 以及 ``lst`` ，我们还没检视过的部分列表。如果没有东西需要检视了，我们调用 ``n-elts`` 来取得 n elts 的表示法。如果 ``lst`` 的第一个元素还是 ``elt`` ，我们增加出现的次数 ``n`` 并继续下去。否则我们得到，到目前为止的一个压缩的列表，然后 ``cons`` 这个列表在 ``compr`` 处理完剩下的列表所返回的东西之上。

要复原一个压缩的列表，我们调用 ``uncompress`` (图 3.7)

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

图 3.7 游程编码 (Run-length encoding)：解压缩


这个函数递归地遍历这个压缩列表，逐字复制原子并调用 ``list-of`` ，展开成列表。

::

   > (list-of 3 'ho)
   (HO HO HO)

我们其实不需要自己写 ``list-of`` 。内建的 ``make-list`` 可以办到一样的事情─但它使用了我们还没介绍到的关键字参数(keyword argument)。

图 3.6 跟 3.7 这种写法不是一个有经验的Lisp 程序员用的写法。它的效率很差，它没有尽可能的压缩，而且它只对由原子组成的列表有效。在几个章节内，我们会学到解决这些问题的技巧。

::

   载入程序

   在这节的程序是我们第一个实质的程序。
   当我们想要写超过数行的函数时，
   通常我们会把程序写在一个档案，
   然后使用 load 让 Lisp 读取函数的定义。
   如果我们把图 3.6 跟 3.7 的程序，
   存在一个档案叫做，"compress.lisp" 然后输入

   (load "compress.lisp")

   到顶层，或多或少的，
   我们会像在直接输入顶层一样得到同样的效果。

   注意：在某些实现中，Lisp 档案的扩展名会是".lsp" 而不是".lisp"。

3.6 存取 (Access)
======================

Common Lisp 有额外的存取函数，它们是用 ``car`` 跟 ``cdr`` 所定义的。要找到列表特定位置的元素，我们可以调用 ``nth`` ，

::

   > (nth 0 '(a b c))
   A

而要找到第 n 个 cdr ，我们调用 ``nthcdr`` ：

::

   > (nthcdr 2 '(a b c))
   (C)

``nth`` 与 ``nthcdr`` 都是零索引的(zero-indexed); 即元素从 0 开始编号，而不是从 1 开始。在Common Lisp 里，无论何时你使用一个数字来参照一个资料结构中的元素时，都是从 0 开始编号的。

两个函数几乎做一样的事; ``nth`` 等同于取 ``nthcdr`` 的 ``car`` 。没有检查错误的情况下， ``nthcdr`` 可以这么定义：

::

   (defun our-nthcdr (n lst)
     (if (zerop n)
         lst
         (our-nthcdr (- n 1) (cdr lst))))

函数 ``zerop`` 仅在自变量为零时，才返回真。

 函数 ``last`` 返回列表的最后一个 Cons 对象：

::
   
   > (last '(a b c))
   (C)

这跟取得最后一个元素不一样。要取得列表的最后一个元素，你要取得 ``last`` 的 ``car`` 。

Common Lisp 定义了函数 ``first`` 直到 ``tenth`` 可以取得列表对应的元素。这些函数不是 *零索引的* (zero-indexed)：

``(second x)`` 等同于 ``(nth 1 x)`` 。

此外， Common Lisp 定义了像是 ``caddr`` 这样的函数，它是 cdr 的 cdr 的 car 的缩写(car of cdr of cdr)。所有这样形式的函数 ``cxr`` ，其中 x 是一个字串，最多四个 a 或 d ，在 Common Lisp 里都被定义好了。使用``cadr`` 可能会有异常 (exception)产生，这不是一个好的主意，在所有人都可能会读的代码里使用这样的函数。

3.7 映射函数 (Mapping Functions)
============================================

Common Lisp 提供了数个函数来对一个列表的元素做函数调用。最常使用的是 ``mapcar`` ，接受一个函数与一个或多个列表，并返回把函数应用至每个列表的元素的结果，直到有的列表没有元素为止：

::

   > (mapcar #'(lambda (x) (+ x 10))
             '(1 2 3))
   (11 12 13)

   > (mapcar #'list
             '(a b c)
             '(1 2 3 4))
   ((A 1) (B 2) (C 3))

相关的 ``maplist`` 接受同样的自变量，将列表的渐进的下一个 cdr 传入函数。

::

   > (maplist #'(lambda (x) x)
              '(a b c))
   ((A B C) (B C) (C))

其它的映成函数，包括 ``mapc`` 我们在 88 页讨论，以及 ``mapcan`` 在202 页讨论。

3.8 树 (Trees)
======================

Conses 对象可以想成是二元树， ``car`` 代表右子树，而 ``cdr`` 代表左子树。举例来说，列表

(a (b c) d) 也是一棵由图 3.8 所代表的树。 （如果你逆时针旋转45度，你会发现跟图 3.3 一模一样）

.. figure:: https://dl-web.dropbox.com/get/Juanito/acl-images/Figure-3.8.png?w=a854542a

图 3.8 二元树 (Binary Tree)

Common Lisp 有几个内建的操作树的函数。举例来说， ``copy-tree`` 接受一个树，并返回一份副本。它可以这么定义：

::

   (defun our-copy-tree (tr)
     (if (atom tr)
          tr
          (cons (our-copy-tree (car tr))
                (our-copy-tree (cdr tr)))))

把这跟 36 页的 ``copy-list`` 比较一下; ``copy-tree`` 复制每一个 Cons 对象的 car 与 cdr，而 ``copy-list`` 仅复制 cdr 。

没有内部节点的二元树没有太大的用处。 Common Lisp 包含了操作树的函数，不只是因为我们需要树这个结构，而是因为我们需要一种方法，来操作列表及所有内部的列表。举例来说，假设我们有一个这样的列表：

::

   (and (integerp x) (zerop (mod x 2)))

而我们想要把各处的 x 都换成 y 。调用 ``substitute`` 是不行的，它只能替换序列(sequence)中的元素：

::

   > (substitute 'x 'y '(and (integerp x) (zerop (mod x 2))))
   (AND (INTEGERP X)(ZEROP (MOD X 2)))

这个调用是无效的，因为列表有三个元素，没有一个元素是 x 。我们在这所需要的是 ``subst`` ，它替换树中的元素。

::

   > (subst 'x 'y '(and (integerp x) (zerop (mod x 2))))
   (AND (INTEGERP Y)(ZEROP (MOD Y 2)))

如果我们定义一个 ``subst`` 的版本，它看起来跟 ``copy-tree`` 很相似：

::

   > (defun our-subst (new old tree)
       (if (eql tree old)
           new
           (if (atom tree)
               tree
               (cons (our-subst new old (car tree))
                     (our-subst new old (cdr tree))))))

操作树的函数通常有这种形式，car 与cdr 同时做递归。这种函数被称之为是 *双重递归* (doubly recursive)。

3.9 理解递归 (Understanding Recursion)
============================================

学生在学习递归时，有时候是被鼓励在纸上追踪 (trace)递归程序调用 (invocation)的过程。 （ 288页（译注：Appendix A Trace and Backtraces）可以看到一个递归函数的追踪过程。）但这种练习可能会误导你：一个程序员在定义一个递归函数时，通常不会明确地去想函数后，函数调用的顺序是什么。

如果一个人总是需要这样子思考程序，递归会是艰难的、没有帮助的。递归的优点是它精确地让我们更抽象地来检视算法。你不需要考虑真正函数时所有的调用 (invocation)过程，就可以判断一个递归函数是否​​是正确的。

要知道一个递归函数是否​​做它该做的事，你只需要问，它包含了所有的情况吗？举例来说，下面是一个寻找列表长度的递归函数：

::

   > (defun len (lst)
       (if (null lst)
           0
           (+ (len (cdr lst)) 1)))

我们可以藉由检查两件事情，来确信这个函数是正确的：

1. 对长度为 0 的列表是有效的。
2. 给定它对于长度为 n 的列表是有效的，它对长度是 n+1 的列表也是有效的。

如果这两点是成立的，我们知道这个函数对于所有可能的列表都是正确的。

我们的定义显然地满足第一点：如果列表( ``lst`` ) 是空的( ``nil`` )，函数直接返回 0。现在假定我们的函数对长度为 n 的列表是有效的。我们给它一个 n+1 长度的列表。这个定义说明了，函数会返回列表的 cdr 的长度再加上 1。 cdr 是一个长度为 n 的列表。我们经由假定可知它的长度是 n 。所以整个列表的长度是 n+1 。

我们需要知道的就是这些。理解递归的秘密就像是处理括号一样。你怎么知道哪个括号对上哪个？你不需要这么做。你怎么想像那些调用过程？你不需要这么做。

更复杂的递归函数，可能会有更多的情况需要讨论，但是流程是一样的。举例来说， 41 页的 ``our-copy-tree`` ，我们需要讨论三个情况： 原子，单一的 Cons 对象， n+1 的 Cons 树。

第一个情况（长度零的列表）称之为 *基本用例* ( *base case* )。当一个递归函数不像你想的那样工作时，通常是因为基本用例是错的。下面这个不正确 ``member``定义，是一个常见的​​错误，整个忽略了基本情况：

::

   (defun our-member (obj lst)
     (if (eql (car lst) obj)
         lst
         (our-member obj (cdr lst))))

我们需要初始一个 ``null`` 测试，确保在到达列表底部时，没有找到目标要停止递归。如果我们要找的对象没有在列表里，这个版本的 ``member`` 会陷入无穷回圈。附录 A 更详细地检视了这种问题。

能够判断一个递归函数是否​​正确只不过是理解递归的上半场，下半场是能够写出一个做你想做的事情的递归函数。 6.9 节讨论了这个问题。

3.10 集合 (Sets)
======================



3.11 序列 (Sequences)
=================================

3.12 栈 (Stacks)
=================================

3.13 点列表 (Dotted Lists)
=================================

3.14 关连列表 (Assoc-lists)
===================================

3.15 示例：最短路径 (Example: Shortest Path)
==================================================

3.16 垃圾 (Garbages)
=========================

Chapter 3 总结 (Summary)
================================

Chapter 3 习题 (Exercises)
==================================