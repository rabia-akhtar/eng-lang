(* Code generation: translate takes a semantically checked AST and
produces LLVM IR

LLVM tutorial: Make sure to read the OCaml version of the tutorial

http://llvm.org/docs/tutorial/index.html

Detailed documentation on the OCaml LLVM library:

http://llvm.moe/
http://llvm.moe/ocaml/

*)

module L = Llvm
module A = Ast

module StringMap = Map.Make(String)
module String = String

let translate (globals, functions, structs) =
  let context = L.global_context () in
  let the_module = L.create_module context "English"
  and i32_t  = L.i32_type  context
  and i8_t   = L.i8_type   context
  and i64_t   = L.i64_type   context
  and p_t  = L.pointer_type (L.i8_type (context))
  and i1_t   = L.i1_type   context
  and f_t    = L.double_type context
  and void_t = L.void_type context
  and struct_t id = L.named_struct_type context id in

  let ltype_of_typ = function
      A.Int -> i32_t
    | A.Float -> f_t
    | A.Bool -> i1_t
    | A.Void -> void_t
    | A.String -> p_t
    | A.Struct id -> struct_t id 
    in

  (* Declare printf(), which the print built-in function will call *)
  let printf_t = L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in
  let printf_func = L.declare_function "printf" printf_t the_module in

  (* Declare the built-in printbig() function *)
  let printbig_t = L.function_type i32_t [| i32_t |] in
  let printbig_func = L.declare_function "printbig" printbig_t the_module in

  (* Declare the built-in open() function *)
  let open_t = L.function_type p_t [| L.pointer_type i8_t; L.pointer_type i8_t |] in
  let open_func = L.declare_function "fopen" open_t the_module in

  (* Declare the built-in close() function *)
  let close_t = L.function_type i32_t [| p_t |] in
  let close_func = L.declare_function "fclose" close_t the_module in
   
  (* Declare the built-in fputs() function as write() *)
  let write_t = L.function_type i32_t [| L.pointer_type i8_t; p_t |] in 
  let write_func = L.declare_function "fputs" write_t the_module in

  (* Declare the built-in fread() function as read() *)
  let read_t = L.function_type i32_t [| p_t; i32_t; i32_t; p_t |] in 
  let read_func = L.declare_function "fread" read_t the_module in

  (* Declare the built-in strlen() function as string_length() *)
  let string_length_t = L.function_type i32_t [| p_t |] in 
  let string_length_func = L.declare_function "strlen" string_length_t the_module in

  (* Declare the built-in strcmp() function as string_length() *)
  let string_compare_t = L.function_type i32_t [| p_t; p_t|] in 
  let string_compare_func = L.declare_function "strcmp" string_compare_t the_module in

  (* format string *)
  let int_format_str builder = L.build_global_stringptr "%d\n" "fmt" builder in
  let float_format_str builder = L.build_global_stringptr "%f\n" "fmt" builder in
  let string_format_str builder = L.build_global_stringptr "%s\n" "fmt" builder in


  (* Return the value for a variable or formal argument *)
  let lookup g_map l_map n = try StringMap.find n l_map
        with Not_found -> StringMap.find n g_map in

  (* Define each function (arguments and return type) so we can call it *)
  let function_decls =
    let function_decl m fdecl =
      let name = fdecl.A.fname
      and formal_types =
  Array.of_list (List.map (fun (t,_) -> ltype_of_typ t) fdecl.A.formals)
      in let ftype = L.function_type (ltype_of_typ fdecl.A.typ) formal_types in
      StringMap.add name (L.define_function name ftype the_module, fdecl) m in
    List.fold_left function_decl StringMap.empty functions in

 let format_str x_type builder =
    let b = builder in
      match x_type with
        A.Int      -> int_format_str b
      | A.Float    -> float_format_str b
      | A.String   -> string_format_str b
      | A.Bool     -> int_format_str b
      | _ -> raise (Failure ("Invalid printf type"))
  in

  (* get type *)
  let rec gen_type g_map l_map = function
      A.NumLit _ -> A.Int
    | A.FloatLit _ -> A.Float
    | A.StringLit _ -> A.String
    | A.BoolLit _ -> A.Bool
    | A.Unop(_,e) -> (gen_type g_map l_map) e
    | A.Binop(e1,_,_) -> (gen_type g_map l_map) e1
    | A.Noexpr -> A.Void

  in

 let get_init_val  = function
        A.NumLit i -> L.const_int i32_t i
      | A.FloatLit f -> L.const_float f_t f
      | A.BoolLit b -> L.const_int i1_t (if b then 1 else 0)
      | A.StringLit s -> let l = L.define_global "" (L.const_stringz context s) the_module in
      L.const_bitcast (L.const_gep l [|L.const_int i32_t 0|]) p_t 
      | A.Noexpr -> L.const_int i32_t 0
 in

 let get_init_noexpr = function
        A.Int -> L.const_int i32_t 0
      | A.Float -> L.const_float f_t 0.0
      | A.Bool -> L.const_int i1_t 0
      | A.String -> get_init_val(A.StringLit "")
  in

  (* Construct code for an expression; return its value *)
  let rec expr builder g_map l_map = function
        A.NumLit i -> L.const_int i32_t i
      | A.FloatLit f -> L.const_float f_t f
      | A.StringLit s -> L.build_global_stringptr s "tmp" builder
      | A.BoolLit b -> L.const_int i1_t (if b then 1 else 0)
      | A.Noexpr -> L.const_int i32_t 0
      | A.Id s -> L.build_load (lookup g_map l_map s) s builder
      | A.Binop (e1, op, e2) ->
       let e1' = expr builder g_map l_map e1
       and e2' = expr builder g_map l_map e2 in
        if (L.type_of e1' = f_t || L.type_of e2' = f_t) then
           (match op with
            A.Add     -> L.build_fadd
          | A.Sub     -> L.build_fsub
          | A.Mult    -> L.build_fmul
          | A.Div     -> L.build_fdiv
          | A.Equal   -> L.build_fcmp L.Fcmp.Oeq
          | A.Neq     -> L.build_fcmp L.Fcmp.One
          | A.Less    -> L.build_fcmp L.Fcmp.Olt
          | A.Leq     -> L.build_fcmp L.Fcmp.Ole
          | A.Greater -> L.build_fcmp L.Fcmp.Ogt
          | A.Geq     -> L.build_fcmp L.Fcmp.Oge
          | _ -> raise (Failure ("operator not supported for operand"))
          ) e1' e2' "tmp" builder
        else
          (match op with
            A.Add     -> L.build_add
          | A.Sub     -> L.build_sub
          | A.Mult    -> L.build_mul
          | A.Div     -> L.build_sdiv
          | A.And     -> L.build_and
          | A.Or      -> L.build_or
          | A.Equal   -> L.build_icmp L.Icmp.Eq
          | A.Neq     -> L.build_icmp L.Icmp.Ne
          | A.Less    -> L.build_icmp L.Icmp.Slt
          | A.Leq     -> L.build_icmp L.Icmp.Sle
          | A.Greater -> L.build_icmp L.Icmp.Sgt
          | A.Geq     -> L.build_icmp L.Icmp.Sge
            ) e1' e2' "tmp" builder
      | A.Unop(op, e) ->
       let e' = expr builder g_map l_map e in
         (match op with
            A.Neg     -> 
              (if (L.type_of e' = f_t) then
                L.build_fneg
              else
                L.build_neg)
            | A.Not     -> L.build_not) e' "tmp" builder
         
      | A.Assign (s, e) -> let e' = expr builder g_map l_map e in
                     ignore (L.build_store e' (lookup g_map l_map s) builder); e'
      | A.Call ("print", [e]) 

      | A.Call ("printb", [e]) -> L.build_call printf_func [| int_format_str builder; (expr builder g_map l_map e) |]
                                 "printf" builder
      | A.Call ("printbig", [e]) -> L.build_call printbig_func [| (expr builder g_map l_map e) |] "printbig" builder
      | A.Call("open", e) -> let x = List.rev (List.map (expr builder g_map l_map) (List.rev e)) in
            L.build_call open_func (Array.of_list x) "fopen" builder
      | A.Call("close", e) -> let x = List.rev (List.map (expr builder g_map l_map) (List.rev e)) in
            L.build_call close_func (Array.of_list x) "fclose" builder
      | A.Call ("read", e) -> let x = List.rev (List.map (expr builder g_map l_map) (List.rev e)) in
            L.build_call read_func (Array.of_list x) "fread" builder
      | A.Call("write", e) -> let x = List.rev (List.map (expr builder g_map l_map) (List.rev e)) in
            L.build_call write_func (Array.of_list x) "fputs" builder
      | A.Call("string_length", e) -> let x = List.rev (List.map (expr builder g_map l_map) (List.rev e)) in
            L.build_call string_length_func (Array.of_list x) "strlen" builder
      | A.Call("string_compare", e) -> let x = List.rev (List.map (expr builder g_map l_map) (List.rev e)) in
            L.build_call string_compare_func (Array.of_list x) "strcmp" builder
      | A.Call ("print_float", [e]) ->
            L.build_call printf_func [| float_format_str builder ; (expr builder g_map l_map e) |] "printf" builder
      | A.Call ("print_string", [e]) ->
             L.build_call printf_func [| string_format_str builder ; (expr builder g_map l_map e) |] "printf" builder
      | A.Call ("print_all", [e]) ->
          let e' = expr builder g_map l_map e in
          let e_type = (gen_type) g_map l_map e in
          L.build_call printf_func [| (format_str e_type builder) ; e' |] "printf" builder
      | A.Call (f, act) ->
         let (fdef, fdecl) = StringMap.find f function_decls in
         let actuals = List.rev (List.map (expr builder g_map l_map) (List.rev act)) in
         let result = (match fdecl.A.typ with A.Void -> ""
                                                        | _ -> f ^ "_result") in
         L.build_call fdef (Array.of_list actuals) result builder
    in

 (* Declare each global variable; remember its value in a map *)
  let global_vars =
    let global_var m (A.VarDecl(_, n, e)) =
      let init = get_init_val e in
      StringMap.add n (L.define_global n init the_module) m in
    List.fold_left global_var StringMap.empty globals in
  
  (* Fill in the body of the given function *)
  let build_function_body fdecl =
    let (the_function, _) = StringMap.find fdecl.A.fname function_decls in
    let builder = L.builder_at_end context (L.entry_block the_function) in
    
    (* Construct the function's "locals": formal arguments and locally
       declared variables.  Allocate each on the stack, initialize their
       value, if appropriate, and remember their values in the "locals" map *)
    let local_vars =
      let add_formal m (t, n) p = L.set_value_name n p;
	    let local = L.build_alloca (ltype_of_typ t) n builder in
	    ignore (L.build_store p local builder);
	    StringMap.add n local m in

    let add_local m (A.VarDecl(t, n, e)) =
      let e' = match e with 
            A.Noexpr -> get_init_noexpr t
          | _ -> expr builder global_vars m e 
      in
      L.set_value_name n e';
      let l_var = L.build_alloca (ltype_of_typ t) n builder in
      ignore (L.build_store e' l_var builder);
      StringMap.add n l_var m in

    let formals = List.fold_left2 add_formal StringMap.empty fdecl.A.formals
          (Array.to_list (L.params the_function)) in
      List.fold_left add_local formals fdecl.A.locals in

    (* Define structs *)
  let struct_decls =
    let struct_decl m sdecl =
      let sname = sdecl.A.sname
      and sformal_types =
  Array.of_list (List.map (fun (A.VarDecl(t,_,_)) -> ltype_of_typ t) sdecl.A.sformals)
      in let stype = L.struct_type context sformal_types in
      StringMap.add sname (stype, sdecl.A.sformals) m in
    List.fold_left struct_decl StringMap.empty structs in
    let struct_lookup n = try StringMap.find n struct_decls
  with Not_found -> raise (Failure ("struct " ^ n ^ " unrecognized")) 
in


    (* Invoke "f builder" if the current block doesn't already
       have a terminal (e.g., a branch). *)
    let add_terminal builder f =
      match L.block_terminator (L.insertion_block builder) with
	Some _ -> ()
      | None -> ignore (f builder) in
	
    (* Build the code for the given statement; return the builder for
       the statement's successor *)
    let rec stmt builder = function
	     A.Block sl -> List.fold_left stmt builder sl
      | A.Expr e -> ignore (expr builder global_vars local_vars e); builder
      | A.Return e -> ignore (match fdecl.A.typ with
	  A.Void -> L.build_ret_void builder
	| _ -> L.build_ret (expr builder global_vars local_vars e) builder); builder
      | A.If (predicate, then_stmt, else_stmt) ->
         let bool_val = expr builder global_vars local_vars predicate in
	 let merge_bb = L.append_block context "merge" the_function in

	 let then_bb = L.append_block context "then" the_function in
	 add_terminal (stmt (L.builder_at_end context then_bb) then_stmt)
	   (L.build_br merge_bb);

	 let else_bb = L.append_block context "else" the_function in
	 add_terminal (stmt (L.builder_at_end context else_bb) else_stmt)
	   (L.build_br merge_bb);

	 ignore (L.build_cond_br bool_val then_bb else_bb builder);
	 L.builder_at_end context merge_bb

      | A.While (predicate, body) ->
	  let pred_bb = L.append_block context "while" the_function in
	  ignore (L.build_br pred_bb builder);

	  let body_bb = L.append_block context "while_body" the_function in
	  add_terminal (stmt (L.builder_at_end context body_bb) body)
	    (L.build_br pred_bb);

	  let pred_builder = L.builder_at_end context pred_bb in
	  let bool_val = expr pred_builder global_vars local_vars predicate in

	  let merge_bb = L.append_block context "merge" the_function in
	  ignore (L.build_cond_br bool_val body_bb merge_bb pred_builder);
	  L.builder_at_end context merge_bb

      | A.For (e1, e2, e3, body) -> stmt builder
	    ( A.Block [A.Expr e1 ; A.While (e2, A.Block [body ; A.Expr e3]) ] )
    in

    (* Build the code for each statement in the function *)
    let builder = stmt builder (A.Block fdecl.A.body) in

    (* Add a return if the last block falls off the end *)
    add_terminal builder (match fdecl.A.typ with
        A.Void -> L.build_ret_void
      | t -> L.build_ret (L.const_int (ltype_of_typ t) 0))
  in

  List.iter build_function_body functions;
  the_module
