> 实验文档、代码处于迭代更新过程中，如有错误或建议，欢迎同学们联系实验助教提PR。



## 实验目的

1. 熟悉 `ANTLR4` 访问者模式和访问者模式下的调试方法。
2. 熟悉 `C++`类型转换与继承相关知识。



## 实验任务

1. 根据已有框架，修改`AstBuilder.cpp`，实现对语法分析树的遍历，生成`Json`表示的抽象语法树。



## 实验概述

本实验旨在为语法分析树生成抽象语法树，识别语法分析树中各语法节点使用的产生式，从而推断其对应的抽象语法树节点，并完善其属性。

以如下代码片段为例：

````c
int a = 1 - 2 * 3;
````

在`Lab1-1`中，生成的语法分析子树为：

<img src="Lab1-img\tree.png" alt="image-20230228200324226" style="zoom:67%;" />

语法分析树呈现了该代码片段的推导过程，但不包含语句、表达式类型等信息。本实验中，我们需要根据这一语法分析树生成如下信息：

````json
{
    "type": "var_def_stmt_node",
    "line": 2,
    "pos": 4,
    "var_def_nodes": [
        {
            "type": "var_def_node",
            "line": 2,
            "pos": 8,
            "is_const": false,
            "is_obc": false,
            "name": "a",
            "BType": "INT",
            "vardef_initializer": {
                "type": "binop_expr_node",
                "line": 2,
                "pos": 12,
                "op": "MINUS",
                "lhs": {
                    "type": "number_node",
                    "line": 2,
                    "pos": 12,
                    "BType": "INT",
                    "number": 1
                },
                "rhs": {
                    "type": "binop_expr_node",
                    "line": 2,
                    "pos": 16,
                    "op": "MULTIPLY",
                    "lhs": {
                        "type": "number_node",
                        "line": 2,
                        "pos": 16,
                        "BType": "INT",
                        "number": 2
                    },
                    "rhs": {
                        "type": "number_node",
                        "line": 2,
                        "pos": 20,
                        "BType": "INT",
                        "number": 3
                    }
              	}
        	}
        }
    ]
}
````

在上述json表达中，"type"属性表示的是抽象语法树节点类型，这些节点定义在`include/AstNode.h`中，根据该语法分析子树生成上述抽象语法树的过程可以简单表示如下：

+ 访问`varDecl`节点：为当前节点创建对应的抽象语法树节点`var_def_stmt_node`，填充行号、列号。

+ 根据文法`varDecl → bType varDef (',' varDef)* ';'`，获取`bType`，同时发现当前非常量、非obc数组，填充`is_const`和`is_obc`。

+ 由于当前节点可能有多个`varDef`子节点，故遍历所有`varDef`子节点，将遍历返回的AST节点用于填充`var_def_nodes`属性：

  + 访问第一个`varDef`子节点：创建对应的抽象语法树节点`var_def_node`，填充行号、列号。
  + 根据子节点类型（有标识符和赋值符号）判断出该节点使用了生成式`varDef → Ident '=' exp`，则分别访问`Ident`和`exp`：
    + 访问`Ident`：获取标识符字符串。
    + 访问`exp`：根据子节点包含两个`exp`这一特性，判断这是一个二元表达式，创建AST节点`binop_expr_node`，填充行号、列号、符号类型，分别访问左右表达式，将结果用于填充`lhs`和`rhs`，过程略。
  + 完成第一个`var_def_node`创建。

  + 没有更多`varDef`子节点，结束。



## 实验内容

在本实验中，我们将会通过antlr为我们提供的c++接口以及访问者模式，遍历先前得到的语法分析树，并生成抽象语法树。

需要补充的代码为`AstBuilder.cpp`，但不排除由于在文法中添加其他产生式导致需要改动其他部分（可能的部分为`src/`和`include/`）。

### 遍历语法分析树

生成抽象语法树是在遍历语法分析树的基础上进行的，本实验中，antlr已经将语法分析树被组织成为了一个树形的数据结构，我们将在本小节中了解这个数据结构。

#### 语法分析树结构

在本实验中，[初次编译](#初次编译)后，antlr会根据我们在`Lab1-1`中实现的语法文件和文法文件，在`build/antlr4cpp_generated_src`目录下生成访问词法分析（`SafeCLexer`目录）和语法分析（`SafeCParser目录`）的C++代码实现，本实验中，我们只需要了解语法分析部分的代码，即`SafeCParser/SafeCParser.*`。

##### `SafeCParser`类

`SafeCParser`类是语法分析的主要类，负责将输入的SafeC代码解析为语法分析树并组织为树形结构。

##### 非终结符类`XXXContext`

`SafeCParser`中有多个名字形式如`XXXContext`的内部类，它们中的每一个都对应一个非终结符，如`CompUnitContext`对应`compUnit`，`DelcContext`对应`decl`，这些类表示非终结符对应的语法节点。

每一个非终结符类都是`ParserRuleContext`的子类，他们从`ParserRuleContext`中继承了多个用于获取该语法节点基本信息的成员函数，如行号、列号（使用方式见`AstBuilder.cpp`中的）。

可以通过两个方法获取该语法节点的子节点：

+ `children`成员变量：每一个非终结符类都有一个`std::vector<ParseTree *> children`成员变量，其中存储了其产生式右侧所有的语法节点。

+ 获取特定类型子节点的成员函数：在`SafeCParser.h`中，可以看到每一个语法节点类都定义了不同的成员函数，而这些成员函数对应了其产生式右侧的符号，可以通过这些函数，获取对应符号类型的子节点。

  如，非终结符`compUnit`的产生式如下：

  ````
  compUnit: (decl | funcDef) + EOF;
  ````

  其对应的类`CompUnitContext`如下：

  ````cpp
   class  CompUnitContext : public antlr4::ParserRuleContext {
    public:
      CompUnitContext(antlr4::ParserRuleContext *parent, size_t invokingState);
      virtual size_t getRuleIndex() const override;
      antlr4::tree::TerminalNode *EOF();
      std::vector<DeclContext *> decl();
      DeclContext* decl(size_t i);
      std::vector<FuncDefContext *> funcDef();
      FuncDefContext* funcDef(size_t i);
  
  
      virtual antlrcpp::Any accept(antlr4::tree::ParseTreeVisitor *visitor) override;
     
    };
  ````

  其中，名为`decl`的函数用于获取右侧产生的`decl`，名为`funcDef`的函数用于获取右侧产生的`funcDef`，名为`EOF`的函数用于获取最后的`EOF`。由于产生式中产生的`decl`和`funcDef`的数量不确定，所以为它们分别生成了两个函数，用于获取所有该类子语法节点或获取第`i`个。若当前生成的语法分析树中没有对应的子语法节点，则会返回空的向量或空指针。

##### 终结符类`TerminalNode`

所有的终结符都用`TerminalNode`这个类表示，通过调用其成员函数`getSymbol()`可以获得该终结符的`Token`对象，调用`Token::getText()`可以获得该token对应的文本。

如，函数定义的产生式如下：

````
funcDef: Void Identifier LeftParen RightParen block;
````

在得到`funcDef`对应的对象`FuncDefContext *ctx`后，我们可以通过如下代码获取该函数的函数名（上述产生式中的`Identifier`）：

````c
string func_name = ctx->Identifier()->getSymbol()->getText();
````

#### 语法分析树的遍历

了解语法分析树的数据结构组织方式后，遍历语法分析树实际上就非常简单。简单来说，我们只需要从语法分析树的起始符号`compUnit`对应的语法节点开始，逐个分析子语法节点即可，一般来说，遍历顺序是DFS。

遍历语法分析树的代码主要在`AstBuilder.*`中，采用[访问者模式](https://zh.wikipedia.org/zh-hans/%E8%AE%BF%E9%97%AE%E8%80%85%E6%A8%A1%E5%BC%8F)实现。访问根节点`CompUnit`的函数`visitCompUnit`和`visitExp`已经为大家作为示例完全/部分实现，请补充代码完成语法分析树的遍历。

### 生成抽象语法树

在`Lab1-1`中，我们已经生成了一个语法分析树，抽象语法树比语法分析树具有更丰富的内涵，详细标记了各语法节点属于或表达了一个什么类型的代码块、语句或表达式，这些标记将在后续实验的进一步分析中帮助我们生成和优化中间代码。

本实验中，我们需要将通过前面描述的方法遍历语法分析树，在遍历的同时完善抽象语法树内容。

#### 抽象语法树结构

本实验代码中，抽象语法树实现在`include/AstNode.h`中，抽象语法树的基类为`ast_node`：

````cpp
struct ast_struct ast_node
{
    int line;		//当前节点行号
    int pos;		//在该行的位置
    virtual void accept(AstNode_Visitor &visitor) = 0;	//纯虚函数，用于访问者模式
};
````

抽象语法树中的节点都是该类的派生类，其他类名都具有`XXX_node`的形式，如起始符`comp_unit_node`：

````cpp
struct comp_unit_node : virtual ast_node
{
    ptr_vector<comp_unit_child_node> comp_units;
    virtual void accept(AstNode_Visitor &visitor) override;
};
````

每一个抽象语法树节点都可能包含一些其他的成员变量，这些成员变量用于标记该语法节点的属性或保存其子节点，比如上述`comp_unit_node`的`comp_units`成员，用于保存其所有子节点`decl`和`funcDef`，亦即全局变量声明和函数声明。该成员类型是`ptr_vector<comp_unit_child_node>`，是`comp_unit_child_node`的指针数组，`comp_unit_child_node`及其派生类如下：

````cpp
struct comp_unit_child_node : virtual ast_node
{
    virtual void accept(AstNode_Visitor &visitor) override;
};

struct global_def_node : comp_unit_child_node
{
    virtual void accept(AstNode_Visitor &visitor) override;
};

struct func_def_node : comp_unit_child_node
{
    std::string name;
    ptr<block_node> body;
    virtual void accept(AstNode_Visitor &visitor) override;
};
````

`ptr_vector<comp_unit_child_node> comp_units`这一向量实际存储了转换为`comp_unit_child_node`指针类型后的`global_def_node`和`func_def_node`指针，利用C++类型转换可以实现`comp_unit_child_node`和这二者之间的类型转换。

本实验中主要用到动态类型转换`dynamic_cast`关键字来进行类型转换，当转换失败时，返回结果是一个空指针，具体示例见`AstBuilder.cpp`中的给出的代码。

### 步骤说明

具体而言，在生成抽象语法树的过程中，我们需要完成如下步骤：

+ 完善`AstBuilder.cpp`，补充完成各`visitXXX`函数，完成对语法分析树的遍历。
+ 在遍历的过程中，分析各语法分析树节点，判断当前语句对应的抽象语法树节点类型，创建对应的抽象语法树节点并填充其成员变量，包括从`ast_node`继承而来的`line`、`pos`和其他自有成员变量。

最后，`include/AstSerializer.h`会遍历我们生成的抽象语法树，并生成最终的json格式文件。

> 如果在实验过程中自己在`AstNode.h`中添加了新的抽象语法树节点，还需修改`AstSerializer.h`中对应部分，完成最终抽象语法树的输出。



## 代码使用说明

### 文件结构

Lab1-2实验目录如下：

```
Lab1-2
├── build.sh
├── cmake
│   ├── antlr4-generator.cmake.in
│   ├── Antlr4Package.md
│   ├── antlr4-runtime.cmake.in
│   ├── ExternalAntlr4Cpp.cmake
│   ├── FindANTLR.cmake
│   └── README.md
├── CMakeLists.txt
├── go.sh
├── include
│   ├── AstBuilder.h
│   ├── AstNode.h
│   ├── AstNode_Visitor.h
│   └── AstSerializer.h
├── main.cpp
├── SafeCLexer.g4
├── SafeCParser.g4
├── src
│   └── AstBuilder.cpp
├── tests
│   ├── decl
│   ├── expr
│   ├── ifelse
│   ├── stmt
│   └── while
└── answers
```

- `cmake`目录：提供ANTLR4项目构建所需要的cmake文件。
- `CMakeLists.txt`文件：编译项目所需要的CMakeList文件。
- `go.sh`：对于编译后的`astbuilder`进行批量测试的脚本。
- `main.cpp`：项目的入口文件。
- `SafeCLexer.g4、SafeCParser.g4`：`Lab1-1`已实现完成的SafeC语法文件。
- `AstNode.h`：实验提供的抽象语法树数据结构，需要阅读了解。
- `AstBuilder.h/AstBuilder.cpp`：**实验任务主要文件，需要同学们添加代码**。
- `AstSerializer.h`：将生成的抽象语法树序列化为json格式的文件，需要阅读了解。
- `test_cases` ：测试文件所在目录。
- `answers`：参考输出结果。

**注意：**

1. 如果在实验Lab1-1中未加入其它功能以及非终结符，以上文件理论上除`AstBuilder.cpp`外，都不需要做任何功能上的修改（但这不意味着不能修改其他文件）。
1. 如果添加了额外的文法，或需要对抽象语法树结构进行定制化，则可能需要修改`include`目录下的文件。
1. 参考输出结果仅供参考，不需要完全一致，但应当保证输出**合理且没有属性错误**的抽象语法树。

### 编译和运行

#### 文件准备

将`Lab1-1`完成的词法和语法文件拷贝到`Lab1-2`目录下：

````sh
# 当前目录为Lab1-2
cp ../Lab1-1/grammar/*.g4 .
````

#### 初次编译

如果没有按照`README.md`中说明安装`antlr-4.9.3-complete.jar`到`/usr/local/lib`目录下，则需要自行修改`CMakeLists.txt`中第24行的`ANTLR_EXECUTABLE`路径，为自己环境中antlr的`jar`包所在路径：

````
set(ANTLR_EXECUTABLE /path/to/antlr-4.9.3-complete.jar)
````

接下来对`Lab1-2`代码进行编译：

````sh
# 当前目录为Lab1-2
mkdir build
cd build
cmake ..
make -j8
````

>  请根据实验使用电脑CPU内核情况自行调整make命令的job数量。
>
>  注意，初次编译时需要从`github`下载antlr运行时库，请保持网络畅通。
>
>  若git总是失败，可以修改host文件，使用命令`sudo gedit /etc/hosts`，在末尾加上以下内容
>
>  ```
>  140.82.112.3 github.com
>  151.101.1.6 github.global.ssl.fastly.net
>  151.101.65.6 github.global.ssl.fastly.net
>  151.101.129.6 github.global.ssl.fastly.net
>  151.101.193.6 github.global.ssl.fastly.net
>  ```
>
>  

如果网络畅通，且没有在`Lab1-1`中添加产生式的情况下，第一次编译通常可以正常完成，**如果在`Lab1-1`中添加了产生式，那么有一定可能编译不通过**，请检查修改自己编写的产生式或根据报错修改实验代码。

#### 常规编译

初次编译后，如后续代码改动没有增加代码文件，则可以不再调用`cmake`更新配置文件，直接进入`build`文件夹`make`即可，避免重新下载antlr运行时花费时间：

````sh
# 进入build目录
cd build
make clean
make -j8
````

#### 使用

编译成功后会在`build`目录下生成名为`astbuilder`的可执行文件，其使用方法为`./astbuilder filepath`，运行的输出为`filepath`的token流、语法分析树和抽象语法树。

一个简单的示例如下（抽象语法树部分需要完成本试验后才能输出正确结果）：

````sh
# 当前在build目录下
$ cat simple.c
void f()
{
   int a = 1;
}
$ ./astbuilder simple.c
[@0,0:3='void',<25>,1:0]
[@1,5:5='f',<28>,1:5]
[@2,6:6='(',<8>,1:6]
[@3,7:7=')',<9>,1:7]
[@4,9:9='{',<6>,2:0]
[@5,14:16='int',<24>,3:3]
[@6,18:18='a',<28>,3:7]
[@7,20:20='=',<3>,3:9]
[@8,22:22='1',<29>,3:11]
[@9,23:23=';',<2>,3:12]
[@10,25:25='}',<7>,4:0]
[@11,27:26='<EOF>',<-1>,5:0]
(compUnit (funcDef void f ( ) (block { (blockItem (decl (varDecl (bType int) (varDef a = (exp (number 1))) ;))) })) <EOF>)
Ast:
{
    "type": "comp_root",
    "line": 1,
    "pos": 0,
    "global_defs": [
        {
            "type": "func_def_node",
            "line": 1,
            "pos": 0,
            "name": "f",
            "body": {
                "type": "block_node",
                "line": 2,
                "pos": 0,
                "body": [
                    {
                        "type": "var_def_stmt_node",
                        "line": 3,
                        "pos": 7,
                        "is_const": false,
                        "is_obc": false,
                        "name": "a",
                        "BType": "INT",
                        "vardef_initializer": {
                            "type": "number_node",
                            "line": 3,
                            "pos": 11,
                            "BType": "INT",
                            "number": 1
                        }
                    }
                ]
            }
        }
    ]
}
````

### 调试

代码中`AstBuilder`类提供了`log`函数输出debug信息，运行时添加`-d`选项可以输出debug信息，即`./astbuilder filepath -d`。`log`函数定义如下：

````c
void log(string info);
````

另外，`Segment Fault`错误是本实验中常常出现的错误，大多是由于类型转换出错导致出现空指针引用，可以使用上述方法排查问题，但更为建议使用`gdb`进行调试，快速定位出错行。

### 约定

##### 数组长度

如果数组初始化如下：

```
int array[]={0, 1, 2, 3, 4, 5, 6}
```

我们约定数组的长度为初始化数值的个数。

### 补充：C++类型转换

本实验中会大量使用到C++类型转换，不当的类型转换会造成`Segment Fault`，实验中可能涉及到的方式如下：

+ `antlrcpp::Any`：

  + 所有`visitXXX`函数的返回类型都是`antlrcpp::Any`，该结构类似加强版的`void*`，返回其他类型指针时，会默认转换为该类型。

  + `antlrcpp::Any`提供了`template<class U> StorageType<U>& as()`方法来将指针转换为原本的类型，如

    ````cpp
    auto func_def_n = visit(func_def).as<func_def_node *>()
    ````

+ `dynamic_cast`：C++提供的动态类型转换方法，转换时注意如下两点：

  + 派生类转换为基类时，可以直接转换；
  + 基类转换为派生类时，目标派生类类型必须是该对象创建时使用的类型。



## 实验提交要求和评分标准

1. 对于已给出的示例，实验提交时应当无`Segmentation Fault`问题。

2. 最终提交时，实验目录如下，**不需要提交build目录**，如果存在对其他文件修改请自行附上。

   ````
   Lab1-2
   ├── readme.txt
   ├── include
   │   ├── AstBuilder.h
   │   ├── AstNode.h
   │   ├── AstNode_Visitor.h
   │   └── AstSerializer.h
   ├── main.cpp
   ├── SafeCLexer.g4
   ├── SafeCParser.g4
   └── src
       └── AstBuilder.cpp
   ````

   请在`readme.txt`说明除了`AstBuilder.cpp`以外的其他修改，如果编译步骤和实验中给定不一样，也请在`readme.txt`中说明。

   另请附上**实验报告**一份，篇幅不宜过长，请尽量以`pdf`格式提交，避免其他格式带来不必要的兼容性问题，内容应当包括但不限于：

   + 小组成员；
   + 实验中实现的完整文法；
   + 如果修改的代码内容除了补充`AstBuilder.cpp`中的内容，请简要说明你修改的其他部分以及目的；
   + 实现过程以及实现过程中遇到的困难。

   同时可以在实验报告中提出对本次实验设计的建议、实验代码和文档勘误等。

3. 评分标准：

   1. `Lab1-1`共6分，20个测例平均给分，助教人工运行每个测例并核对`gui`方式生成的语法分析树是否正确，评分标准如下：
      1. 若语法分析树结构正确，则当前测例得满分；
      2. 若整体结构正确但有部分错误，则根据情况扣除0.05-0.15分；
      3. 若不能正常生成语法分析树、整体结构错误，则当前测例得0分。
   2. `Lab1-2`共6分，25个测例平均给分，助教人工运行`Lab1-2`中测例并判别结果是否正确，每个测例分值相同，评分标准如下：
      1. 若抽象语法树生成正确，则当前测例得满分；
      1. 若整体结构正确，但部分属性填充错误/不完整，则扣除0.1分；
      3. 若整体结构有明显错误（节点类型不正确、语法树结构不正确）且语法分析树正确，则扣除0.2分；
      3. 若整体结构有明显错误（节点类型不正确、语法树结构不正确）且语法分析树错误，则当前测例得0分。
   3. 实验报告共3分，按要求编写即可。
   3. 提交截止时间以课程网站为准，迟交请微信/邮箱联系实验助教补交，会在得分基础上扣除至少**20%**分数。
   3. 实验代码会进行查重，小组之间交流时请勿共享代码。
   4. 实验成绩公布后，如果对得分有异议，请在一周内联系实验助教复核。
   




## 参考资料

- [访问者模式-菜鸟教程](https://www.runoob.com/design-pattern/visitor-pattern.html)

- [ANTLR学习笔记4：语法导入和访问者(Visitor)模式](https://blog.csdn.net/SHU15121856/article/details/106331151)

- [c++ 强制类型转换](https://zhuanlan.zhihu.com/p/101493574)

- [CMPE 152: Compiler Design](https://www.cs.sjsu.edu/~mak/archive/CMPE152/index.html)

