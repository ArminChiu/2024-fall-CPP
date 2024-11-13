## 实验目的

1. 熟悉 ANTLR4 语法描述。



## 实验任务

1. 完善Safe C语言的ANTLR4词法文件(`SafeCLexer.g4`)，生成token流。
1. 完善Safe C语言的ANTLR4语法文件(`SafeCParser.g4`)，生成语法分析树。
3. 基于语法理解，自行编写正确样例和错误样例对上述实现进行测试。



## Safe C 文法介绍

Safe C是C语言的子集，一种简单的类C语言，在C语言的基础上，加入了obc(Out of Bound Check)数组的定义，后续实验中，将在编译过程中对obc数组插入obc检查代码，本实验中只需要实现相关语法解析。

Safe C部分基本文法可表示如下：

````
compUnit	→ (decl | funcDef)+ EOF
decl 		→ constDecl | varDecl
funcDef		→ ‘void’ Ident '(' ')' block
varDecl		→ bType varDef (',' varDef)* ';'
varDef		→ Ident | array | Ident '=' exp
contDecl	→ 'const' bType constDef (',' constDef)* ';'
constDef	→ Ident '=' exp
bType		→ 'int'
array		→ obcArray | unobcArray
obcArray 	→ 'obc' unobcArray
unobcArray 	→ Ident '[' (exp)? ']'
block		→ '{' blockItem* '}'
blockItem 	→ decl | stmt
stmt		→ ...
cond		→ ...
lval		→ Ident | Ident '[' exp ']'
number		→ IntConst
exp			→ ...
````

本文采用antlr生成文法解析器，由于antlr的特性，上述文法中，有多个当前产生式的情况下会**优先推导左侧产生式**，因此，在实现$exp$的产生式时会比较便利。

另外，**上述文法并不完整，请根据测例在词法文件和语法文件中完善文法**。



## 任务一 完善Safe C 词法文件

词法文件主要用于解析语言的token流，为后续语法分析做准备，比如变量，算数符号，内置关键字等等。

本实验中，需要完善的词法文件为`SafeCLexer.g4`。

在实验时，以下知识可能会有所帮助：

1. 普通 token 使用单引号包裹；
2. 对注释的要求与C语言一致，在最后使用 `-> skip`表示跳过分析
3. 对变量的要求与C一致，即字母、下划线开头，由字母、下划线、数字组成，暂时不考虑与关键字冲突的问题
4. 对常量数字的要求是，支持16进制和10进制表示
5. 和正则表达式写法类似`[a-z]`表示字母a-z的集合，?、+、*的使用也和正则表达式一样

完整的token撰写规则和例子可以查看官方文档 [lexer rules](https://github.com/ANTLR/antlr4/blob/master/doc/lexer-rules.md)。

当实现完词法文件后，通过如下命令进行编译：

```sh
// 编译 .g4文件得到相应的java代码
antlr SafeCLexer.g4
// 编译生成的java文件
javac SafeCLexer.java
```

编译通过后，使用如下命令可以获取测例`0.c`的token流：

````sh
grun SafeCLexer tokens -tokens ../tests/0.c
````

输出介绍，输出的token流中每一行都具有相同格式，格式如下：

````
[@序号,起始位置:结束位置='token',<词法单元>,行号:列号]
````

以一个例子说明，测例`0.c`中第四行的变量`a`对应的输出为：

````
[@2,57:57='a',<Identifier>,4:10]
````

其含义为：

+ 这是整个token流中第4个token
+ 把整个文件当做一个字符串，token所在位置是从57到57
+ 该token是`a`
+ 对应的词法单元是`Identifier`
+ 该token在文件的第4行第10列



## 任务二 完善SafeC 语法文件‌

语法文件建立在词法文件的基础之上，用于分析目标代码，生成抽象语法树。

本实验中，需要完善的语法文件为`SafeCParser.g4`。

ANTLR4 语法文件的描述十分接近语言的EBNF描述，因此写起来十分简洁易懂，具体语法可参考 [parser rules](https://github.com/ANTLR/antlr4/blob/master/doc/parser-rules.md) 。

完成语法文件后，可以通过如下命令进行编译：

````
// 编译 .g4文件得到相应的java代码
antlr SafeCParser.g4
// 编译生成的java文件
javac SafeCParser.java
````

在完成了任务一、二并且二者都编译成功之后，可以通过如下命令生成`0.c`的语法树：

```
// 以图片方式查看语法树
grun SafeC compUnit -gui ../tests/0.c
// 命令行方式输出语法树
grun SafeC compUnit -tree ../tests/0.c
```

P.S. ：

+ 所有测例的语法都是正确的，如果测试过程中报语法错误，请根据报错检查修改自己的词法文件和语法文件。
+ 评分时会用`-gui`人工判`Parse`写的是否正确。



## 实验提交和评分

Lab1-1代码完成后，拷贝到Lab1-2目录下，覆盖Lab1-2原有文件，具体提交要求和评分方法见按照Lab1-2文档。



## 其他

本实验文件目录如下：

```
Lab1-1
├── grammar
│   ├── SafeCLexer.g4
│   └── SafeCParser.g4
├── tests
│   ├── 0.c
│   └── ...
└── answers
│	├── graph/
│	└── text/
└── go.sh
```

+ `tests`：全部的测试样例。
+ `answers`：以文本和矢量图格式存储的参考答案，输出答案不要求和给定答案完全一致。
+ `go.sh`：用于批量测试测例并将结果以文本形式输出到`output`文件夹，linux下可使用`diff`命令和`answers/text`下的参考答案进行对比:`diff -r output answers/text`。



## 参考资料

- [antlr4 grammars](https://github.com/antlr/grammars-v4) ：包含了大量常用语言(C/C++/Java等)的ANTLR语法文件，在实验时可以进行参考。
- [LL(*): The Foundation of the ANTLR Parser Generator](https://www.antlr.org/papers/LL-star-PLDI11.pdf) ：ANTLR4所使用的LL(*)算法。
- [The Definitive ANTLR 4 Reference](https://pragprog.com/titles/tpantlr2/the-definitive-antlr-4-reference/)：ANTLR4官方参考手册，国内也有一本《ANTLR权威指南》，可参照着阅读。