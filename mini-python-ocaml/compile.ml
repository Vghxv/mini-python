
open Format
open X86_64
open Ast
open Utils
open Environment
open Functions

let debug = ref false

let compile_const (env: env_t) (c: Ast.constant) : X86_64.text * X86_64.data * ty =
  match c with
  | Cnone ->
    movq (imm (2 * byte))  !%rdi ++
    call "malloc_wrapper" ++
    movq (imm 0) (ind rax) ++
    movq (imm 0) (ind ~ofs:(byte) rax)
    , nop, `none
  | Cbool b ->
    movq (imm (2 * byte)) !%rdi ++
    call "malloc_wrapper" ++
    movq (imm 1) (ind rax) ++
    movq (imm (if b then 1 else 0)) (ind ~ofs:(byte) rax)
    , nop, `bool
  | Cint i ->
    movq (imm (2 * byte)) !%rdi ++
    call "malloc_wrapper" ++
    movq (imm 2) (ind rax) ++
    movq (imm64 i) (ind ~ofs:(byte) rax)
    , nop, `int
  | Cstring s ->
    let len = String.length s in
    let (text_code, _) = String.fold_left (fun (acc, counter) i ->
      let counter = counter + byte in
      acc ++ movq (imm (Char.code i)) (ind ~ofs:(counter) rax), counter
    ) (nop, byte) s in
    movq (imm ((len + 3) * byte)) !%rdi ++
    call "malloc_wrapper" ++
    movq (imm 3) (ind rax) ++
    movq (imm len) (ind ~ofs:(byte) rax) ++
    text_code ++
    movq (imm 0) (ind ~ofs:((len + 2) * byte) rax)
    ,nop , `string

let compile_var (env: env_t) (v: Ast.var) : X86_64.text * X86_64.data * ty =
  let var, ofs, var_type = StringMap.find v.v_name env.vars in
    movq (ind ~ofs:(ofs) rbp) !%rax, nop, var_type

(* return value *)
let rec compile_expr (env: env_t) (expr: Ast.texpr) : X86_64.text * X86_64.data * ty =
  match expr with
  | TEcst c ->
    compile_const env c
  | TEvar v ->
    compile_var env v
  | TEbinop (op, e1, e2) ->
    begin match op with
    | Band ->
      let text_code1, data_code1, _ = compile_expr env e1 in
      let text_code2, data_code2, _ = compile_expr env e2 in
      let l = unique_label env "cond_false" in
      text_code1 ++
      movq (ind ~ofs:(byte) rax) !%rdi ++
      cmpq (imm 0) !%rdi ++
      je l ++
      text_code2 ++
      label l, 
      data_code1 ++ data_code2, `bool  
    | Bor ->
      let text_code1, data_code1, expr_type1 = compile_expr env e1 in
      let text_code2, data_code2, expr_type2 = compile_expr env e2 in
      let l = unique_label env "cond_true" in
      text_code1 ++
      movq (ind ~ofs:(byte) rax) !%rdi ++
      cmpq (imm 0) !%rdi ++
      jne l ++
      text_code2 ++
      label l,
      data_code1 ++ data_code2, `bool
    | Badd | Bsub | Bmul | Bdiv | Bmod | Beq | Bneq | Blt | Ble | Bgt | Bge ->
      let text_code1, data_code1, expr_type1 = compile_expr env e1 in
      let text_code2, data_code2, expr_type2 = compile_expr env e2 in
      begin match op, expr_type1, expr_type2 with
      | Badd, `int, `int ->
        arith_asm text_code1 text_code2 (addq !%rsi !%rdi),
        data_code1 ++ data_code2, `int
      | Badd, `int, `string ->
        print_endline "Badd int string";
        text_code1 ++ text_code2 ++ call "runtime_error"
        , nop, `int
      | Bsub, `int, `int ->
        arith_asm text_code1 text_code2 (subq !%rsi !%rdi),
        data_code1 ++ data_code2, `int
      | Bmul, `int, `int ->
        arith_asm text_code1 text_code2 (imulq !%rsi !%rdi),
        data_code1 ++ data_code2, `int
      | Bdiv, `int, `int ->
        arith_asm text_code1 text_code2
        (
          movq !%rdi !%rax ++
          cqto ++
          movq !%rsi !%rbx ++
          idivq !%rbx ++
          movq !%rax !%rdi
        ),
        data_code1 ++ data_code2, `int
      | Bmod, `int, `int ->
        arith_asm text_code1 text_code2
        (
          movq !%rdi !%rax ++
          cqto ++
          movq !%rsi !%rbx ++
          idivq !%rbx ++
          movq !%rdx !%rdi
        ),
        data_code1 ++ data_code2, `int
      | Beq, `int, `int | Beq, `bool, `bool ->
        arith_asm text_code1 text_code2
        (
          cmpq !%rsi !%rdi ++
          sete !%dil ++
          movzbq !%dil rdi
        ),
        data_code1 ++ data_code2, `bool
      | Bneq, `int, `int | Bneq, `bool, `bool ->
        arith_asm text_code1 text_code2
        (
          cmpq !%rsi !%rdi ++
          setne !%dil ++
          movzbq !%dil rdi
        ),
        data_code1 ++ data_code2, `bool
      | Blt, `int, `int | Blt, `bool, `bool ->
        arith_asm text_code1 text_code2
        (
          cmpq !%rsi !%rdi ++
          setl !%dil ++
          movzbq !%dil rdi
        ),
        data_code1 ++ data_code2, `bool
      | Ble, `int, `int | Ble, `bool, `bool ->
        arith_asm text_code1 text_code2
        (
          cmpq !%rsi !%rdi ++
          setle !%dil ++
          movzbq !%dil rdi
        ),
        data_code1 ++ data_code2, `bool
      | Bgt, `int, `int | Bgt, `bool, `bool ->
        arith_asm text_code1 text_code2
        (
          cmpq !%rsi !%rdi ++
          setg !%dil ++
          movzbq !%dil rdi
        ),
        data_code1 ++ data_code2, `bool
      | Bge, `int, `int | Bge, `bool, `bool ->
        arith_asm text_code1 text_code2
        (
          cmpq !%rsi !%rdi ++
          setge !%dil ++
          movzbq !%dil rdi
        ),
        data_code1 ++ data_code2, `bool
      | _ ->
        failwith "Unsupported binop"
      end
    end
  | TEunop (op, e) ->
    begin match op with
    | Uneg ->
      let text_code, data_code, expr_type = compile_expr env e in
      begin match expr_type with
      | `int ->
        text_code ++
        movq (ind ~ofs:(byte) rax) !%rdi ++
        negq !%rdi ++
        pushq !%rdi ++
        movq (imm byte) !%rdi ++
        call "malloc_wrapper" ++
        popq rdi ++
        movq (imm 2) (ind rax) ++
        movq !%rdi (ind ~ofs:(byte) rax),
        data_code, `int
      | _ ->
        failwith "Unsupported Uneg"
      end
    | Unot ->
      let text_code, data_code, expr_type = compile_expr env e in
      begin match expr_type with
      | `bool ->
        text_code ++
        movq (ind ~ofs:(byte) rax) !%rdi ++
        xorq (imm 1) !%rdi ++
        pushq !%rdi ++
        movq (imm byte) !%rdi ++
        call "malloc_wrapper" ++
        popq rdi ++
        movq (imm 1) (ind rax) ++
        movq !%rdi (ind ~ofs:(byte) rax),
        data_code, `bool
      | _ ->
        failwith "Unsupported Unot"
      end
    end
  | TEcall (fn, args) ->
    failwith "Unsupported TEcall"
  | TElist l ->
    let len = List.length l in
    List.fold_left (fun (acc, counter) i ->
      let text_code, _, _ = compile_expr env i in
      acc ++
      pushq !%rax ++ (* save heap address*)
      text_code ++
      movq !%rax !%rsi ++
      popq rax ++
      movq !%rsi (ind ~ofs:(counter) rax),
      counter + byte
    ) (nop, 2 * byte) l |> fun (text_code, _) ->
    movq (imm ((len + 2) * byte)) !%rdi ++
    call "malloc_wrapper" ++
    movq (imm 4) (ind rax) ++ (* type *)
    movq (imm len) (ind ~ofs:(byte) rax) ++ (* length *)
    text_code, nop, `list
  | TErange e ->
    failwith "Unsupported TErange"
  | TEget (e1, e2) ->
    failwith "Unsupported TEget"

let rec compile_stmt (env: env_t) (stmt: Ast.tstmt) : X86_64.text * X86_64.data =
  match stmt with
  | TSif (cond, s1, s2) ->
    let text_code_cond, data_code_cond, _ = compile_expr env cond in
    let text_code_s1, data_code_s1 = compile_stmt env s1 in
    let text_code_s2, data_code_s2 = compile_stmt env s2 in
    let else_label = unique_label env "else" in
    let end_label = unique_label env "end" in
    text_code_cond ++
    cmpq (imm 0) (ind rax) ++
    je else_label ++
    text_code_s1 ++
    jmp end_label ++
    label else_label ++
    text_code_s2 ++
    label end_label
    , data_code_cond ++ data_code_s1 ++ data_code_s2
  | TSreturn expr ->
    failwith "Unsupported Sreturn"
  | TSassign (var, expr) ->
    env.stack_offset <- env.stack_offset - byte;
    let text_code, data_code, expr_type = compile_expr env expr in
    env.vars <- StringMap.add var.v_name (var, env.stack_offset, expr_type) env.vars;
    text_code ++
    movq !%rax (ind ~ofs:(env.stack_offset) rbp)
    , data_code
  | TSprint expr ->
    let text_code, data_code, expr_type = compile_expr env expr in
    begin match expr_type with
    | `none ->
      text_code ++
      call "print_none" ++
      put_character newline,
      data_code
    | `bool ->
      text_code ++
      movq !%rax !%rdi ++
      call "print_bool" ++
      put_character newline,
      data_code
    | `int ->
      text_code ++
      movq !%rax !%rdi ++
      call "print_int" ++
      put_character newline,
      data_code
    | `string ->
      text_code ++
      movq !%rax !%rdi ++
      call "print_string" ++
      put_character newline,
      data_code
    | `list ->
      text_code ++
      movq !%rax !%rdi ++
      call "print_list" ++
      put_character newline, 
      data_code
    end
  | TSblock stmts ->
    List.fold_left (fun (acc_text, acc_data) stmt ->
      let text_code, data_code = compile_stmt env stmt in
      acc_text ++ text_code, acc_data ++ data_code
    ) (nop, nop) stmts
    |> fun (text_code, data_code) -> 
      addq (imm (env.stack_offset)) !%rsp ++
      text_code ++
      subq (imm (env.stack_offset)) !%rsp
      , data_code
  | TSfor (var, expr, body) ->
    failwith "Unsupported Sfor"
  | TSeval expr ->
    failwith "Unsupported TSeval"
  | TSset (e1, e2, e3) ->
    failwith "Unsupported TSset"

let compile_def env ((fn, body): Ast.tdef) : X86_64.text * X86_64.data =
  let env_local = { env with vars = StringMap.empty; } in
  let prologue = 
    pushq !%rbp ++
    movq !%rsp !%rbp in
  let epilogue = 
    xorq !%rax !%rax ++
    movq !%rbp !%rsp ++
    popq rbp ++
    ret in
  let body_code, data_code = compile_stmt env_local body in
  label fn.fn_name ++ prologue ++ body_code ++ epilogue, data_code


let file ?debug:(b=false) (p: Ast.tfile) : X86_64.program =
  debug := b;
  let env = empty_env in
  let runtime_error_text, runtime_error_data = emit_runtime_error env in
  (* Compile each function *)
  let text_code, data_code = List.fold_left (fun (text_acc, data_acc) (fn, body) ->
    let fn_code, data_code = compile_def env (fn, body) in
    (text_acc ++ fn_code, data_acc ++ data_code)
  ) (nop, nop) p in
  { 
    text = 
      c_standard_function_wrapper "malloc_wrapper" "malloc" ++
      c_standard_function_wrapper "putchar_wrapper" "putchar" ++
      c_standard_function_wrapper "printf_wrapper" "printf" ++
      c_standard_function_wrapper "strcmp_wrapper" "strcmp" ++
      c_standard_function_wrapper "strcpy_wrapper" "strcpy" ++
      c_standard_function_wrapper "strcat_wrapper" "strcat" ++
      func_print_none ++
      func_print_bool env ++
      func_print_int ++
      func_print_string env ++
      func_print_list env ++
      runtime_error_text ++
      globl "main" ++ text_code ++
      label "aaa";
    data = 
      data_code ++
      runtime_error_data ++
      label "print_inta" ++
      string "%d" ++
      label "true_string" ++
      string "True" ++
      label "false_string" ++
	    string "False" ++
      label "none_string" ++
      string "None";
  }