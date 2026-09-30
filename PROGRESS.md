# sahaxe 进度总览 (PROGRESS)

Haxe `--sa` 目标：将 Haxe 程序编译为 SA 文本汇编 (`.sa`)，
由 `sci` 工具链校验 (`sa check`) 并交付 (`sa build-exe` / `sa build-wasm`)。
设计文档见 `SA_TARGET.md`； touchpoint 检查 `tools/check_sa_target.py`；
形状自检 `tools/check_sa_shape.py`；验证集 `tests/sa-demo/`。

## 门禁 (每提交必过)

- `make haxe` 零警告零错误 (OCaml 5.2, dune 3.24)
- `tools/check_sa_target.py` 26/26
- `tools/check_sa_shape.py` 全过 (块终结/标签列/无不可达)
- 全部 fixture：零 `SA-TODO` + `sa check` 零 trap (Referee 真验)
- 每特性独立提交并推送 `sahaxe:main`

## 已交付 (sahaxe:main)

| 版本 | 内容 | fixture |
|---|---|---|
| v0.1 | 平台注册 (9 处) + 骨架发射 + `std/sa` | hello-world e2e |
| v0.2 | 直线 lowering (常量/驻留/算术/trace) | Arith |
| v0.3 | if/else + while/do-while (Phi 安全) | Flow |
| v0.4a | 数组字面量/索引/length + mem_ty | Arr |
| v0.4b | 匿名结构 (排序布局) + 字符串值 | Obj, ObjArr |
| v0.5a | switch eq 链 + 无参 enum | Switch |
| v0.5b | 静态方法 + 调用 (含递归) | Funcs |
| v0.5c | sci 补充 `STRING_EQ`/`STRING_NEQ` (sci:cross-platform) | — |
| v0.6a | 字符串 `==`/`!=` + switch-on-string | StrEq |
| v0.6b | `Std.string(int)` + 拼接打印 + typedef | StrFmt |
| v0.7 | hxml/`--next` 批处理 (零代码) | build.hxml |
| v0.8 | 宏求值互通 (零代码) | Mac |

当前：10 fixture，零 TODO，Referee 全绿。

## 所有权纪律 (Referee 实战结论)

- 栈槽 (`stack_alloc`) 永不显式释放 (StackEscape)；堆分配必释。
- 分支合并边存活集必须一致：臂内临时对称释放；循环 cond 残留
  在 break 边保留；switch 测试寄存器单例复用。
- 存活表按路径存取 (save/restore/keep_oldest)；`keep_oldest`
  留最旧，`take` 留最新，用反即双释/漏释。
- 函数参数是存活寄存器，出口必释。
- `&CONST` 入参的宏不得释放入参 (SLICE_NEW 惯例)。

## 待办 (按优先级)

1. class 实例构造 + 方法调用 (this/self，借用前缀)
2. for-in 协议 + 迭代器 (IntIterator/数组遍历)
3. fs/net 对接 (`Sys`/`File`/`Socket` 经 `sa_std`)
4. String 入参函数 (借用 `(ptr,len)` 双参改写)
5. 可存 computed-string (句柄种标记的出口纪律)
6. float 全覆盖 (fmt 精度语义、`fcmp_*` 比较)
7. float Math.* 需 sci Zig 运行时实现 (不可 Haxe 侧原创)
8. 泛型单态化、异常 `T!`/`?`、payload enum/match 提取

## sa_std 缺口状态 (sci/sa_std)

- 已补：`STRING_EQ`/`STRING_NEQ` (纯既有契约组合，零新 ABI)
- 待补：float Math (floor/sqrt/sin/random，需 Zig 实现)
- 复用中：print/fmt/concat/vec/hash/fs/env/net/mem/time
