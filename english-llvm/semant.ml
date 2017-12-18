(* Semantic checking for the ELL compiler *)

open Ast
module A = Ast

module StringMap = Map.Make(String)
module StringSet = Set.Make(String)

(* Semantic checking of a program. Returns void if successful,
   throws an exception if something is wrong.

   Check each global variable, then check each function *)

let check (globals, functions, structs) =

  (* Raise an exception if the given list has a duplicate *)
  let report_duplicate exceptf list =
    let rec helper = function
	n1 :: n2 :: _ when n1 = n2 -> raise (Failure (exceptf n1))
      | _ :: t -> helper t
      | [] -> ()
    in helper (List.sort compare list)
  in

  (* Check struct name and recursive definition *)
  let find_sdecl_from_sname struct_t_name =
    try List.find (fun t-> t.sname= struct_t_name) structs 
      with Not_found -> raise (Failure("Struct " ^ struct_t_name ^ "not found")) 
  in
  let rec check_rec_struct_h sdecl structs_known_set =
    let check_for_repetition struct_t_name =
      if StringSet.mem struct_t_name structs_known_set 
      then raise (Failure ("recursive struct definition"))
      else check_rec_struct_h (find_sdecl_from_sname struct_t_name)  
      (StringSet.add struct_t_name structs_known_set)
    in
    let struct_field_check = function
      (Struct s, _) -> check_for_repetition s
      | _ -> () 
    in
    let sformals_list = List.map (fun (VarDecl(t, n, _)) -> (t, n)) sdecl.sformals in
    List.iter (struct_field_check) sformals_list
  in
  let check_recursive_struct sdecl =
     check_rec_struct_h sdecl StringSet.empty    
  in
  let _ = List.map check_recursive_struct structs
  in

  (* Raise an exception if a given binding is to a void type *)
  let check_not_void_f exceptf = function
      (Void, n) -> raise (Failure (exceptf n))
    | _ -> ()
  in

   let check_not_void_v exceptf = function
     (VarDecl(Void, n,_)) -> raise (Failure (exceptf n)) 
    | _ -> ()
  in
  
  (* Raise an exception of the given rvalue type cannot be assigned to
     the given lvalue type *)
  (* let check_assign lvaluet rvaluet err =
     if lvaluet == rvaluet then lvaluet else raise err
  in  *)

  let resolve_struct_access sname field = 
    let  s = try List.find (fun t -> t.sname = sname) structs 
      with Not_found -> raise (Failure("Struct " ^ sname ^ " not found")) in
    let sformals = List.map (fun (VarDecl(t, n, _)) -> (t, n)) s.sformals in
    try fst( List.find (fun s -> snd(s) = field) sformals) with
  Not_found -> raise (Failure("Field " ^ field ^ " not found in Struct" ^ sname))
  in

  let check_access lhs rhs =
     match lhs with
       Struct s -> resolve_struct_access s rhs
       | _ -> raise (Failure(string_of_typ lhs^ " is not a struct"))
  
  in

  (* Check function declrations *)
  let check_func_decl func_name =
    if List.mem func_name (List.map (fun fd -> fd.fname) functions)
  then raise (Failure ("function may not be defined as " ^ func_name))
  in

  (* check all reserved function names *)
  check_func_decl "printb";
  check_func_decl "printbig";
  check_func_decl "print_float";
  check_func_decl "print_all";
  check_func_decl "open";
  check_func_decl "close";
  check_func_decl "read";
  check_func_decl "write";
  check_func_decl "strlen";
  check_func_decl "strcmp";
  check_func_decl "strcat";
  check_func_decl "strcpy";
  check_func_decl "strget";
  check_func_decl "to_lower";
  check_func_decl "calloc";
  check_func_decl "free";
  check_func_decl "print_char";
  check_func_decl "is_stop_word";
  check_func_decl "word_count";
  check_func_decl "print_string";

   
  (**** Checking Global Variables ****)

  List.iter (check_not_void_v (fun n -> "illegal void global " ^ n)) globals;
   
  report_duplicate (fun n -> "duplicate global " ^ n) 
    (List.map (fun (VarDecl(_,n,_)) -> n)  globals);

  (* allowed initiation types *)
  let globalInitTyps = function
      NumLit _ -> A.Simple(A.Int)
      | FloatLit _ -> A.Simple(A.Float)
      | BoolLit _ -> Bool
      | StringLit _ -> A.Simple(A.String)
      | CharLit _ -> A.Simple(A.Char)
      | _ -> raise (Failure ("Illegal global initialization"))
  in

  let check_type lvaluet rvaluet err =
     if (String.compare (string_of_typ lvaluet) (string_of_typ rvaluet)) == 0 then lvaluet else raise err
  in

  let checkGlobalInit = function
    VarDecl(t,n,e) -> if e != Noexpr then
      let typ = globalInitTyps e in
        ignore (check_type t typ(Failure ("Global initialization type does not match " ^ n ^ " " ^ string_of_expr e))) 
  in

  (* check assignment types *)
  List.iter checkGlobalInit globals; 

  (**** Checking Functions ****)

  if List.mem "print" (List.map (fun fd -> fd.fname) functions)
  then raise (Failure ("function print may not be defined")) else ();

  report_duplicate (fun n -> "duplicate function " ^ n)
    (List.map (fun fd -> fd.fname) functions);

  (* Function declaration for a named function *)
  let built_in_decls =

      StringMap.add "print"
     { typ = Void; fname = "print"; formals = [(A.Simple(A.Int), "x")];
       locals = []; body = [] }

       (StringMap.add "printb"
     { typ = Void; fname = "printb"; formals = [(Bool, "x")];
       locals = []; body = [] }

        (StringMap.add "printbig"
     { typ = Void; fname = "printbig"; formals = [(A.Simple(A.Int), "x")];
       locals = []; body = [] }

        (StringMap.add "print_float"
     { typ = Void; fname = "print_float"; formals = [(A.Simple(A.Float), "x")];
       locals = []; body = [] }

       (StringMap.add "print_all"
     { typ = Void; fname = "print_all"; formals = [(A.Simple(A.String), "x")];
       locals = []; body = [] }

       (StringMap.add "open"
     { typ = A.Simple(A.String); fname = "open"; formals = 
     [(A.Simple(String), "x"); (A.Simple(A.String), "y")]; locals = []; body = []}

       (StringMap.add "close"
     { typ = Void; fname = "close"; formals = 
     [(A.Simple(A.String), "x")]; locals = []; body = []}

       (StringMap.add "read"
     { typ = A.Simple(A.Int); fname = "read"; formals =
     [(A.Simple(A.String), "a"); (A.Simple(A.Int), "b"); (A.Simple(A.Int), "c"); (A.Simple(A.String), "d")];
       locals = []; body = [] }

       (StringMap.add "write"
     { typ = A.Simple(Int); fname = "write"; formals = 
     [(A.Simple(String), "x"); (A.Simple(String), "y")]; 
       locals = []; body = [] }


       (StringMap.add "strlen"
     { typ = A.Simple(A.Int); fname = "strlen"; formals = 
     [(A.Simple(A.String), "x")]; 

       locals = []; body = [] }

       (StringMap.add "strcmp"
     { typ = A.Simple(A.Int); fname = "strcmp"; formals = 
     [(A.Simple(A.String), "x"); (A.Simple(A.String), "x")]; 
       locals = []; body = [] }

        (StringMap.add "strcat"
     { typ = A.Simple(A.String); fname = "strcat"; formals = 
     [(A.Simple(A.String), "x"); (A.Simple(A.String), "x")]; 
       locals = []; body = [] }

        (StringMap.add "strcpy"
     { typ = A.Simple(A.String); fname = "strcpy"; formals = 
     [(A.Simple(A.String), "x"); (A.Simple(A.String), "x")]; 
       locals = []; body = [] }

        (StringMap.add "strget"
     { typ = A.Simple(A.Char); fname = "strcat"; formals = 
     [(A.Simple(A.String), "x"); (A.Simple(A.Int), "y")]; 
       locals = []; body = [] }

       (StringMap.add "to_lower"
     { typ = A.Simple(A.Char); fname = "to_lower"; formals = 
     [(A.Simple(A.Char), "x")]; 
       locals = []; body = [] }

       (StringMap.add "calloc"
     { typ = A.Simple(A.String); fname = "calloc"; formals = 
     [(A.Simple(A.Int), "x"); (A.Simple(A.Int), "x")]; 
       locals = []; body = [] }

        (StringMap.add "free"
     { typ = A.Simple(A.String); fname = "free"; formals = 
     [(A.Simple(A.String), "x") ]; 
       locals = []; body = [] }

       (StringMap.add"print_char"
     { typ = Void; fname = "print_char"; formals = [(A.Simple(A.Char), "x")];
       locals = []; body = [] }

         (StringMap.add"is_stop_word"
     { typ = A.Simple(A.Int); fname = "is_stop_word"; formals = [(A.Simple(A.String), "x")];
       locals = []; body = [] }

           (StringMap.add"string_at"
     { typ = A.Simple(A.String); fname = "string_at"; formals = [(A.Simple(A.String), "x"); (A.Simple(A.Int), "x"); (A.Simple(String), "x")];
       locals = []; body = [] }

         (StringMap.add"word_count"
     { typ = A.Simple(A.Int); fname = "word_count"; formals = [(A.Simple(A.String), "x")];
       locals = []; body = [] }

       (StringMap.singleton "print_string"

     { typ = Void; fname = "print_string"; formals = [(A.Simple(A.String), "x")];
       locals = []; body = [] })))))))))))))))))))))


   in

  (* Accepted types for print_all *)
  let print_types = [A.Simple(String); A.Simple(Int); Bool; A.Simple(Float); A.Simple(A.Char)] in

  let function_decls = List.fold_left (fun m fd -> StringMap.add fd.fname fd m)
                         built_in_decls functions
  in

  let function_decl s = try StringMap.find s function_decls
       with Not_found -> raise (Failure ("unrecognized function " ^ s))
  in

  (* let struct_decls = List.fold_left (fun m st -> StringMap.add st.sname st m) 
                          StringMap.empty structs *)
  
  let check_type lvaluet rvaluet err =
     if (String.compare (string_of_typ lvaluet) (string_of_typ rvaluet)) == 0 then lvaluet else raise err
  in
  
 (* let struct_decl s = try StringMap.find s struct_decls
      with Not_found -> raise (Failure ("unrecognized struct" ^ s)) *)
 

  let _ = function_decl "main" in (* Ensure "main" is defined *)

  let check_function func =

    List.iter (check_not_void_f (fun n -> "illegal void formal " ^ n ^
      " in " ^ func.fname)) func.formals;

    report_duplicate (fun n -> "duplicate formal " ^ n ^ " in " ^ func.fname)
      (List.map snd func.formals);

    List.iter (check_not_void_v (fun n -> "illegal void local " ^ n ^
      " in " ^ func.fname)) func.locals;

    report_duplicate (fun n -> "duplicate local " ^ n ^ " in " ^ func.fname)
      (List.map (fun (VarDecl(_,n,_)) -> n) func.locals);

    (* Type of each variable (global, formal, or local *)
    let var_symbols = List.fold_left (fun m (t, n) -> StringMap.add n t m) 
       StringMap.empty func.formals in

    let symbols = List.fold_left (fun m (VarDecl(t,n,_)) -> StringMap.add n t m)
       var_symbols (globals @ func.locals) in

    let type_of_identifier s =
      try StringMap.find s symbols
      with Not_found -> raise (Failure ("undeclared identifier " ^ s))
    in

    let array_access_type = function
      Array(t,_) -> Simple(t)
      | _ -> raise(Failure("Can only access a[x] from an array a")) 
    in

    (* Return the type of an expression or throw an exception *)
    let rec expr = function
	  NumLit _ -> A.Simple(A.Int)
      | FloatLit _ -> A.Simple(A.Float)
      | BoolLit _ -> Bool
      | CharLit _ -> A.Simple(A.Char)
      | StringLit _ -> A.Simple(A.String)
      | ArrayLit(l) -> let first_type = expr (List.hd l) in
                         let _ = (match first_type with
                                    Simple _ -> ()
                                  | _ -> raise (Failure ("'" ^ string_of_expr (List.hd l) ^ "' is not simple and is in array"))
                                 ) in
                         let _ = List.iter (fun x -> if string_of_typ(expr x) == string_of_typ first_type then ()
                                                     else raise (Failure ("'" ^ string_of_expr x ^ "' doesn't match array's type"))) l in
                         Array((match first_type with Simple(x) -> x
                                 | _ -> raise(Failure("not array type"))), 1)  
      | ArrayAccess(s, e1) -> let _ = (match (expr e1) with
                                        Simple(Int) -> Simple(Int) (* || A.Simple(A.String) -> A.Simple(A.String) || A.Simple(A.Float) -> A.Simple(A.Float) *)
                                       | _ -> raise (Failure ("attempting to access with a non integer type"))) in
                              array_access_type (type_of_identifier s)
      | Index (a, i) -> if string_of_typ(expr (List.hd i)) != string_of_typ(Simple(Int))
                       then raise ( Failure("Array index ('" ^ string_of_expr (List.hd i) ^ "') is not an integer") )
                       else
                         let type_of_entity = expr a in
                         (match type_of_entity with
                            Array(d, _) -> Simple(d)
                          | Simple(String) -> Simple(String)
                          | _ -> raise (Failure ("Entity being indexed ('" ^ string_of_expr a ^"') cannot be array")))
      | Id s -> type_of_identifier s
      | ArrayAssign(v, i, e) as ex -> let type_of_left_side = 
                                      if string_of_typ(expr (List.hd i)) != string_of_typ(Simple(Int))
                                      then raise ( Failure("Array index ('" ^ string_of_expr (List.hd i) ^ "') is not an integer") )
                                      else 
                                        let type_of_entity = type_of_identifier v in
                                        (match type_of_entity with
                                           Array(d, _) -> Simple(d)
                                         | _ -> raise (Failure ("Entity being indexed ('" ^ v ^"') cannot be array"))) in
                                      let type_of_right_side = expr e in
                                      check_type type_of_left_side type_of_right_side 
                                      (Failure ("illegal assignment " ^ string_of_typ type_of_left_side ^
                                                " = " ^ string_of_typ type_of_right_side ^ " in " ^ 
                                                string_of_expr ex))
      | Binop(e1, op, e2) as e -> let t1 = expr e1 and t2 = expr e2 in
	       (match op with
           Add | Sub | Mult | Div when t1 = A.Simple(A.Int) && t2 = A.Simple(A.Int) -> A.Simple(A.Int)
         | Add | Sub | Mult | Div when t1 = A.Simple(A.Float) && t2 = A.Simple(A.Float) -> A.Simple(A.Float)
         | Add | Sub | Mult | Div when t1 = A.Simple(A.Char) && t2 = A.Simple(A.Char) -> A.Simple(A.Char)
	       | Equal | Neq when t1 = t2 -> Bool
	       | Less | Leq | Greater | Geq when t1 = A.Simple(A.Int) && t2 = A.Simple(Int) -> Bool
         | Less | Leq | Greater | Geq when t1 = A.Simple(A.Float) && t2 = A.Simple(A.Float) -> Bool
	       | And | Or when t1 = Bool && t2 = Bool -> Bool
         | _ -> raise (Failure ("illegal binary operator " ^
              string_of_typ t1 ^ " " ^ string_of_op op ^ " " ^
              string_of_typ t2 ^ " in " ^ string_of_expr e))
        )
      | Dot(e, field) -> check_access (expr e) field
      | Unop(op, e) as ex -> let t = expr e in
	     (match op with
	         Neg when t = A.Simple(A.Int) -> A.Simple(A.Int)
         | Neg when t = A.Simple(A.Float) -> A.Simple(A.Float)
	       | Not when t = Bool -> Bool
         | _ -> raise (Failure ("illegal unary operator " ^ string_of_uop op ^
	  		   string_of_typ t ^ " in " ^ string_of_expr ex)))
     | Pop(e, op) as ex -> let t = expr e in
        (match op with
          | Inc | Dec -> (match t with 
                           A.Simple(A.Int) -> A.Simple(A.Int)
                         | A.Simple(A.Float) -> A.Simple(A.Float)
                         | _ -> raise (Failure ("illegal postfix operator " ^ string_of_pop op ^
                                              string_of_typ t ^ " in " ^ string_of_expr ex)))
        )
      | Noexpr -> Void
      | Assign(var, e) as ex -> let lt = expr var
                                and rt = expr e in
        check_type lt rt (Failure ("illegal assignment " ^ string_of_typ lt ^
				     " = " ^ string_of_typ rt ^ " in " ^ 
				     string_of_expr ex))
      | Call(fname, actuals) as call -> let fd = function_decl fname in
         if List.length actuals != List.length fd.formals then
           raise (Failure ("expecting " ^ string_of_int
             (List.length fd.formals) ^ " arguments in " ^ string_of_expr call))
         else
           let _ =
                (match fname with
                  "print_all" ->
                    ignore (List.iter (fun e ->
                      let etyp = expr e in
                      if (List.mem etyp print_types) == false then
                        raise (Failure ("illegal actual argument found " ^ string_of_typ etyp ^ " in " ^ string_of_expr e))) actuals);
                  | _ ->
                  List.iter2 (fun (ftyp, _) e ->
                    let etyp = expr e in
                    ignore (check_type ftyp etyp (Failure ("illegal actual argument found " ^ string_of_typ etyp ^ " expected " ^ string_of_typ ftyp ^ " in " ^ string_of_expr e)))
                  ) fd.formals actuals
              ) in
            fd.typ
    in

    let check_bool_expr e = if expr e != Bool
     then raise (Failure ("expected Boolean expression in " ^ string_of_expr e))
     else () in

    (* Verify a statement or throw an exception *)
    let rec stmt = function
	Block sl -> let rec check_block = function
           [Return _ as s] -> stmt s
         | Return _ :: _ -> raise (Failure "nothing may follow a return")
         | Block sl :: ss -> check_block (sl @ ss)
         | s :: ss -> stmt s ; check_block ss
         | [] -> ()
        in check_block sl
      | Expr e -> ignore (expr e)
      | Return e -> let t = expr e in if t = func.typ then () else
         raise (Failure ("return gives " ^ string_of_typ t ^ " expected " ^
                         string_of_typ func.typ ^ " in " ^ string_of_expr e))
           
      | If(p, b1, b2) -> check_bool_expr p; stmt b1; stmt b2
      | For(e1, e2, e3, st) -> ignore (expr e1); check_bool_expr e2;
                               ignore (expr e3); stmt st
      | While(p, s) -> check_bool_expr p; stmt s
    in

    let check_var_init = function 
      VarDecl(t,_,e) as ex -> if e != Noexpr then
        let v = expr e in
          ignore (check_type t v(Failure ("illegal initialization of" ^ string_of_typ t ^
             " = " ^ string_of_typ v ^ " in " ^ string_of_vdecl ex))) in

    stmt (Block func.body);
    List.iter check_var_init func.locals
  in
  List.iter check_function functions
