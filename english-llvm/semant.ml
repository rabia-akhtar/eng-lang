(* Semantic checking for the ELL compiler *)

open Ast

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
  let check_assign lvaluet rvaluet err =
     if lvaluet == rvaluet then lvaluet else raise err
  in

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
  check_func_decl "to_lower";
  check_func_decl "print_char";
  check_func_decl "print_string";

   
  (**** Checking Global Variables ****)

  List.iter (check_not_void_v (fun n -> "illegal void global " ^ n)) globals;
   
  report_duplicate (fun n -> "duplicate global " ^ n) 
    (List.map (fun (VarDecl(_,n,_)) -> n)  globals);

  (* allowed initiation types *)
  let globalInitTyps = function
      NumLit _ -> Int
      | FloatLit _ -> Float
      | BoolLit _ -> Bool
      | StringLit _ -> String
      | CharLit _ -> Char
      | _ -> raise (Failure ("Illegal global initialization"))
  in

  let checkGlobalInit = function
    VarDecl(t,n,e) -> if e != Noexpr then
      let typ = globalInitTyps e in
        if t != typ 
          then raise (Failure ("Global initialization type does not match " ^ n ^ " " ^ string_of_expr e)) 
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
     { typ = Void; fname = "print"; formals = [(Int, "x")];
       locals = []; body = [] }

       (StringMap.add "printb"
     { typ = Void; fname = "printb"; formals = [(Bool, "x")];
       locals = []; body = [] }

        (StringMap.add "printbig"
     { typ = Void; fname = "printbig"; formals = [(Int, "x")];
       locals = []; body = [] }

        (StringMap.add "print_float"
     { typ = Void; fname = "print_float"; formals = [(Float, "x")];
       locals = []; body = [] }

       (StringMap.add "print_all"
     { typ = Void; fname = "print_all"; formals = [(String, "x")];
       locals = []; body = [] }

       (StringMap.add "open"
     { typ = String; fname = "open"; formals = 
     [(String, "x"); (String, "y")]; locals = []; body = []}

       (StringMap.add "close"
     { typ = Void; fname = "close"; formals = 
     [(String, "x")]; locals = []; body = []}

       (StringMap.add "read"
     { typ = Int; fname = "read"; formals = 
     [(String, "a"); (Int, "b"); (Int, "c"); (String, "d")];
       locals = []; body = [] }

       (StringMap.add "write"
     { typ = Int; fname = "write"; formals = 
     [(String, "x"); (String, "y")]; 
       locals = []; body = [] }

       (StringMap.add "strlen"
     { typ = Int; fname = "strlen"; formals = 
     [(String, "x")]; 
       locals = []; body = [] }

       (StringMap.add "strcmp"
     { typ = Int; fname = "strcmp"; formals = 
     [(String, "x"); (String, "x")]; 
       locals = []; body = [] }

        (StringMap.add "strcat"
     { typ = String; fname = "strcat"; formals = 
     [(String, "x"); (String, "x")]; 
       locals = []; body = [] }

        (StringMap.add "strcpy"
     { typ = String; fname = "strcpy"; formals = 
     [(String, "x"); (String, "x")]; 
       locals = []; body = [] }

        (StringMap.add "strget"
     { typ = Char; fname = "strcat"; formals = 
     [(String, "x"); (Int, "y")]; 
       locals = []; body = [] }

       (StringMap.add "to_lower"
     { typ = Char; fname = "to_lower"; formals = 
     [(Char, "x")]; 
       locals = []; body = [] }

       (StringMap.add "calloc"
     { typ = String; fname = "calloc"; formals = 
     [(Int, "x"); (Int, "x")]; 
       locals = []; body = [] }

        (StringMap.add "free"
     { typ = String; fname = "free"; formals = 
     [(String, "x") ]; 
       locals = []; body = [] }

       (StringMap.add"print_char"
     { typ = Void; fname = "print_char"; formals = [(Char, "x")];
       locals = []; body = [] }

         (StringMap.add"is_stop_word"
     { typ = Int; fname = "is_stop_word"; formals = [(String, "x")];
       locals = []; body = [] }

       (StringMap.singleton "print_string"
     { typ = Void; fname = "print_string"; formals = [(String, "x")];
       locals = []; body = [] })))))))))))))))))))

   in

  (* Accepted types for print_all *)
  let print_types = [String; Int; Bool; Float; Char] in

  let function_decls = List.fold_left (fun m fd -> StringMap.add fd.fname fd m)
                         built_in_decls functions
  in

  let function_decl s = try StringMap.find s function_decls
       with Not_found -> raise (Failure ("unrecognized function " ^ s))
  in

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

    (* Return the type of an expression or throw an exception *)
    let rec expr = function
	  NumLit _ -> Int
      | FloatLit _ -> Float
      | BoolLit _ -> Bool
      | CharLit _ -> Char
      | StringLit _ -> String
      | Id s -> type_of_identifier s
      | Binop(e1, op, e2) as e -> let t1 = expr e1 and t2 = expr e2 in
	       (match op with
           Add | Sub | Mult | Div when t1 = Int && t2 = Int -> Int
         | Add | Sub | Mult | Div when t1 = Float && t2 = Float -> Float
         | Add | Sub | Mult | Div when t1 = Char && t2 = Char -> Char
	       | Equal | Neq when t1 = t2 -> Bool
	       | Less | Leq | Greater | Geq when t1 = Int && t2 = Int -> Bool
         | Less | Leq | Greater | Geq when t1 = Float && t2 = Float -> Bool
	       | And | Or when t1 = Bool && t2 = Bool -> Bool
         | _ -> raise (Failure ("illegal binary operator " ^
              string_of_typ t1 ^ " " ^ string_of_op op ^ " " ^
              string_of_typ t2 ^ " in " ^ string_of_expr e))
        )
      | Dot(e, field) -> check_access (expr e) field
      | Unop(op, e) as ex -> let t = expr e in
	     (match op with
	         Neg when t = Int -> Int
         | Neg when t = Float -> Float
	       | Not when t = Bool -> Bool
         | _ -> raise (Failure ("illegal unary operator " ^ string_of_uop op ^
	  		   string_of_typ t ^ " in " ^ string_of_expr ex)))
      | Pop(e, op) as ex -> let t = expr e in
        (match op with
          | Inc | Dec -> (match t with 
                           Int -> Int
                         | Float -> Float
                         | _ -> raise (Failure ("illegal postfix operator " ^ string_of_pop op ^
                                              string_of_typ t ^ " in " ^ string_of_expr ex)))
        )
      | Noexpr -> Void
      | Assign(var, e) as ex -> let lt = expr var
                                and rt = expr e in
        check_assign lt rt (Failure ("illegal assignment " ^ string_of_typ lt ^
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
                    ignore (check_assign ftyp etyp (Failure ("illegal actual argument found " ^ string_of_typ etyp ^ " expected " ^ string_of_typ ftyp ^ " in " ^ string_of_expr e)))
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
          if (t != v) then
            raise (Failure ("illegal initialization of" ^ string_of_typ t ^
             " = " ^ string_of_typ v ^ " in " ^ string_of_vdecl ex)) in

    stmt (Block func.body);
    List.iter check_var_init func.locals
  in
  List.iter check_function functions
