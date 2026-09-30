# Haxe SA 目标 (Safe ASM)

`haxe --sa <file>` 把 Haxe 程序编译为 SA 文本汇编 (`.sa`)，
再由 `sci` 工具链 (`sa build-exe` / `sa build-wasm`) 验证并交付。

## 1. 背景: sala / sa_plugin_ts / sla 对比

| 维度 | sala | sa_plugin_ts | sla (sa_plugin_sla) |
|---|---|---|---|
| 是什么 | SA/SLA 的 CHM 风格帮助文档 (`sa_all/sala`) | TypeScript → SA 编译器插件 (Zig) | Sla → SA/SAB 编译器插件 (Zig)，Rust 风格高级语言 |
| 输入 | 无 (文档) | 受限 TS 子集 (demos 001–198) | `.sla` (Rust 风格，LLM 友好) |
| 输出 | HTML 帮助 | `.sa` 文本，经 `sa build` 校验 | `.sa` 文本 + SAB 主线 (direct AST→SAB 快路径) |
| 所有权处理 | 文档定义五符号契约 (`= & ^ ! *`) | lowerer 自动插入 `!` 释放，惰性 label，`block_terminated` 跟踪 | 编译器托管清理，用户面无 `drop`；分支汇合自动补释放 |
| 标准库 | 记录 `sci/sa_std` (`.sai`/`.sal`/`.sa`) | TS `fs`/`net` 映射到 `@sa_fs_*` / `@sa_net_*` 原语 | 直接 `@import "sa_std/..."`，方法经 `std_surface.sla_meta` lower |
| 状态 (2026 快照) | 帮助章节 01–12 | 72 Zig 测试全过；256 demo 扫 255 过 | check 306/313，build-exe 约 76% (239/313) |

**Haxe SA 目标的定位**: 与上面两者一样，同为"上游语言 → SA IR"的前端，
复用同一套 Referee 校验与 LLVM/WASM 发射。Haxe 侧做类型检查与 texpr
lowering，SA 侧做所有权校验——即 sala 所说的"前端责任制"。

## 2. SA 发射规则 (gensa.ml 必须遵守)

来源: sala `content/03_sa_asm/02_sa_syntax.html` + `sci/docs/ebnf.md`
+ `sci/docs/llm_cheat_sheet.md`。

- 顶层: `@import "sa_std/io/print.sai"`、`@const X = utf8:"..."`、
  `#def T_field = +N`、`@name(params) -> type:`、`@extern`、`@export`。
- 函数体是扁平指令流，`L_LABEL:` 起始 (label 必须第 0 列)，
  每个基本块必须以 `jmp` / `br` / `return` 终结，终结后不得再有指令。
- 禁止 `if/while/{}/a.b.c` 等形态 (`ForbiddenSyntax`)；
  Haxe 的 `if/switch/while/for` 全部降为 `eq+br` 链 + `jmp`。
- 内存: `x = alloc 8` / `x = stack_alloc 8` /
  `load r+off as T` / `store r+off, v as T` (offset 必填) /
  `slot = ptr_add base, i`。
- 算术比较统一写作 `r = op a, b`:
  `add sub mul sdiv udiv srem urem neg and or xor shl lshr ashr not`
  `eq ne slt sle sgt sge ult ule ugt uge fadd fsub fmul fdiv fcmp_*`
  + `trunc zext sext bitcast` 等转换。
- 调用: `r = call @f(args)` + `return r` (禁止 `return call @f(...)`)；
  参数前缀 `&`(借用) `/` `^`(move) 必须与被调函数签名一致
  (`CapabilityMismatch`)。
- 所有权: 每个出口手动 `!reg` 释放全部活跃寄存器 (`MemoryLeak`)；
  move 后不得再读 (`UseAfterMove`)；`stack_alloc` 必须提升到分支之前
  (`PhiStateConflict`)；标量重赋值用新寄存器名 (`RegisterRedefinition`)。
- 错误传播: 仅 `-> T!` 函数可用 `v = ? res`；`panic(lit)` 直接中止。
- 类型注解仅限 `i8..i64/u8..u64/f32/f64/ptr/v128` (`UnsupportedType`)。

## 3. sa_std 复用表 (禁止原创)

Haxe Std → `sci/sa_std` 映射只允许引用已存在的契约；
缺失的能力必须先在 `sci/sa_std` 补充对应 `.sai`/`.sal`/`.sa`，
不得在 Haxe 侧发明新的运行时 ABI。

| Haxe 侧 | sa_std 现有契约 | 状态 |
|---|---|---|
| `trace` / `Sys.print` | `sa_std/io/print.sai` (`@sa_print_bytes`) | v0.1 已用 |
| 整数格式化 (`Std.string(i)`) | `sa_std/fmt.sai` (`sa_fmt_*_into`, buffer 三件套) | 待接入 |
| 字符串相等 (`==`, switch-on-string) | `sa_std/string.sa` (`STRING_EQ`/`STRING_NEQ`, 已补，无新 ABI) | v0.5c 已补 |
| 字符串 | `sa_std/string.sai` + `string.sa` | 待接入 |
| 数组/Vec | `sa_std/vec.sa` + `alloc/vec.sal` | 待接入 |
| Map | `sa_std/hashmap.sa` / `btree_map.sa` | 待接入 |
| Option/Result | `sa_std/core/option.sa`, `core/result.sa` | 待接入 |
| 文件 IO (`sys.io.File`) | `sa_std/fs.sai` (`@sa_fs_*`) | 待接入 |
| 环境/参数 (`Sys.args`) | `sa_std/env.sai` | 待接入 |
| 时间 (`Sys.time`) | `sa_std/time.sai` | 待接入 |
| Socket (`sys.net.Socket`) | `sa_std/net.sai` | 待接入 |
| Haxe 特有但 sa_std 缺失的 | 先在 `sci/sa_std` 按现有三件套约定补充 | 禁止 Haxe 侧原创 ABI |

## 4. Haxe 特性 → SA 降级策略 (已知限制来源 sala 06_limitations)

- `switch/match`: 逐个 `eq` + `br` 链 (与 SA 无结构化 switch 对应)。
- 闭包捕获: Haxe `pf_capture_policy = CPLoopVars` (同 Lua/Python)，
  循环变量装箱；SA 侧无 GC，捕获环境经 `alloc` + 显式释放。
- 异常 (`throw/try-catch`): v0.1 降为 `panic` (同 sa_plugin_ts)；
  完整 `T!` + `?` 传播待后续特性。
- 泛型: 编译期单态化 (同 SLA)，每种实例化独立函数。
- `return` 统一出口: Haxe 多出口函数在 SA 侧每个出口重复释放集。

## 5. 编译器改动清单

- `src/core/globals.ml`: `platform` 新增 `Sa`，`platforms`、
  `platform_name` (`"sa"`)、`parse_platform`。
- `src/context/common.ml`: `short_platform_name` (`"sa"`)，
  `get_config` 新增 `| Sa` (动态目标配置，对标 Lua)。
- `src/compiler/compiler.ml`: `Setup.initialize_target` 新增
  `| Sa -> add_std "sa"; "sa"`。
- `src/compiler/args.ml`: `--sa/-sa <file>` (Target 组)，
  `parse_args`、`to_raw_args`。
- `src/compiler/generate.ml`: `| Sa -> Gensa.generate, "sa"`。
- `src/macro/macroApi.ml`: `encode_platform` 新增 `Sa -> 12`
  (既有 tag 保持不动，CustomTarget 仍为 11)。
- `src/optimization/analyzerTexpr.ml`: `target_handles_unops`、
  switch subject 与 `Lua | Python` 同组，加入 `Sa`。
- `src/generators/gensa.ml`: 新增 (v0.1 骨架，见文件头注释)。
- `std/sa/Boot.hx`: 新增 (v0.1 stub)。
- dune: `(modules (:standard ...))` 自动发现 `gensa.ml`，无需注册。

## 6. 验证

- `python3 tools/check_sa_target.py` (touchpoint 静态检查，
  无 OCaml 环境也可跑)。
- `make haxe` (需 OCaml ≥ 5.0 + opam 依赖，见 `extra/BUILDING.md`)。
- Hello-world: `haxe --sa /tmp/hx_hello.sa -main Hello -cp tests/sa-demo`
  (待 lowering 落地后启用)，再用 `sa build-exe /tmp/hx_hello.sa` 校验。

## 7. 路线图 (demo 语料优先级)

语料普查 (`sa_all/survey_demos.py`, 608 个入口)：
`sa_plugin_ts/demos` 286 个 (function 100%, arith 62%, for 25%,
if 24%, array 21%, struct 17%, switch 10%, enum 6%)，
`sa_plugin_sla/demos` 322 个 (arith/if/function/return ~99%,
struct 20%, array 10%)。按用户指令：基础特性优先，其次 hxml，最后宏。

- v0.1 : 平台注册 + 骨架发射 + 文档 (可编译，可出已知-good 形状)。
- v0.2 : 常量/局部变量/`stack_alloc` 驻留 + 整数算术/比较 +
  bool 逻辑 + 字面量 `trace` → `@sa_print_bytes` (覆盖 function/return/arith)；
  `main_expr` 经 `entry_stmts` 解出静态 main 方法体；
  验证集 `tests/sa-demo/Arith.hx(.sa)`，`check_sa_target.py` 26/26。
- v0.3 : `if/else` + `while`/`do-while` → `br`+`jmp` (覆盖 if 24-99%)；
  全部分支 `stack_alloc` 提升 (PhiStateConflict)，臂内临时对称释放，
  break/continue/终结追踪 (无不可达指令)，do-while+直接跳转诚实回退；
  优化器改写覆盖 (`+=`/`++`)； fixtures Flow/Arith 零 TODO，
  `tools/check_sa_shape.py` 形状自检通过。
- v0.4a : 定长数组字面量/索引读写/`.length` + `mem_ty`
  (f64/i32/ptr 三档，存取标注一致)；读形状照抄 `ARRAY_GET_U64`
  (`mul idx,8` + `ptr_add` + `load`)；布局 `[len:u64][elems×8]`；
  fixture Arr 零 TODO；`push`/增长留待 vec 宏 (v0.5)。
- v0.4b : 匿名结构字面量 + 字段读写 (按名排序布局，
  声明/使用一致；类实例需构造调用，留 v0.5)；字符串值操作数
  (`&CONST` 直存)；数组元素内字段写保护性回退；
  fixtures Obj/ObjArr 零 TODO (堆对象经数组存活得到覆盖)。
- v0.5a: `switch` → `eq`+`br` 链 (多模式或链，default，
  对称释放，终结传播)；无参 enum 构造 = tag 常量，`TEnumIndex`
  透传；字符串/guard/payload 模式诚实回退；fixture Switch 零 TODO。
- v0.5b : 主类静态方法 → 独立 `@hx_Class_m` 函数
  (全标量签名，参数栈驻留，递归/前向调用预注册)；
  调用 `r = call` / void 裸 `call`；函数尾声按终结标志取舍
  (无不可达)；fixture Funcs (add/fact 递归) 零 TODO。
- v0.6a: 字符串 `==`/`!=` + switch-on-string，
  消费补充的 `STRING_EQ`/`STRING_NEQ` (长度前端跟踪，
  条件 `@import "sa_std/string.sa")；单测试寄存器复用；
  存活表路径纪律 (save/restore/keep_oldest)，Referee 全绿
  (8 fixtures 零 TODO + `sa check` 零 trap)。
- v0.6b: `Std.string(int)` + 字符串 `+` 直接打印
  (fmt/concat 句柄经共享注册表 data/len/free，全既有契约；
  借用 `&pslot` 满足 print 合约；左折叠多段拼接)；
  typedef 转写透明；fixture StrFmt；9 fixture Referee 全绿。
- v0.9 : class 实例构造 + 方法调用
  (可达性发射 ctor/method/static；`this` 栈驻留；
  `op=` 扩展到字段/元素；构造器经 `cl_constructor` 定位，
  对照 ts 插件 trait-downgrade 惯例)；fixture ClassHx。
  消费补充的 `STRING_EQ`/`STRING_NEQ` (长度前端跟踪，
  条件 `@import "sa_std/string.sa")；单测试寄存器复用；
  存活表路径纪律 (save/restore/keep_oldest)，Referee 全绿
  (8 fixtures 零 TODO + `sa check` 零 trap)。
- v0.7 : hxml 工程互通 — `--sa` 走通用管线，
  hxml/`--next` 批处理零代码可用 (fixture `tests/sa-demo/build.hxml`)。
- v0.8: 宏互通 — 宏在 Eval 上下文求值，与 codegen 无关，
  零代码可用 (fixture `tests/sa-demo/Mac.hx`, expr 宏展开后正常降)。
- v0.10 : for-in 协议 — 范围/array 由 typer 化为索引
  while (零新增代码)，自定义迭代器走 v0.9 方法机制；
  fixture ForLoop (三形状) 零 TODO + Referee 全绿。
- v0.11: sys 对接 (`File.saveContent/getContent`, `FileSystem.exists`,
  `Sys.getEnv`) 经既有 fs/env 契约 (宏+extern)；失败 loud-panic；
  fixture FsDemo；socket 诚实延后。
- v0.12: float 全覆盖 (`fcmp_*` 比较经 ISA 实锤，混合运算显式
  `sitofp`，`fneg`，`Std.int` 经 `fptosi`，`Std.string(float)`
  经 `fmt_f64` 精度 6 [ts 惯例，repr 差已注明])；
  fixture FloatHx；14 fixture Referee 全绿。
- v0.13: String 入参函数 (`name_ptr: ptr, name_len: u64` 双参改写，
  调用点 pair 拼接，长度表复用；trace 非字面量一律 `&pslot`)；
  fixture StrParam；15 fixture Referee 全绿。
- v0.14: 可存 computed-string (concat/fmt 物化自有堆 + 出口释放，
  句柄即时 free；嵌套 concat 递归物化)；fixture StrStore；
  16 fixture Referee 全绿。
- v0.15: `@:generic` 单态化直通 (typer 特化后常规发射；
  裸泛型诚实延后) + `br` 统一寄存器绑定 (Imm 条件不再非法)；
  String 返回值比较诚实延后；fixture GenHx；17 fixture 全绿。
- v0.16: `throw`→`panic` (中止等价，载荷注明丢弃) +
  无中止源 try 体直降 (handler 注明省略，有中止源诚实回退)；
  fixture TryHx；18 fixture 全绿。
- v0.17: `Sys` 纵深 (`putEnv`/`sleep`/`getCwd` 可存 `getEnv`，
  `time()` 经 `unix_ms`→`sitofp`→`fdiv`，`time.sai` 条件导入)；
  fixture SysHx；19 fixture 全绿。
- 每个版本独立提交并推送，`sa_std` 缺失先补 `sci/sa_std`。
