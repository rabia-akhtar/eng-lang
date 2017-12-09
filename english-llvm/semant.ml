(* Semantic checking for the ELL compiler *)

open Ast

module StringMap = Map.Make(String)

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
   
  (**** Checking Global Variables ****)

  List.iter (check_not_void_v (fun n -> "illegal void global " ^ n)) globals;
   
  report_duplicate (fun n -> "duplicate global " ^ n) 
    (List.map (fun (VarDecl(_,n,_)) -> n)  globals);

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

       (StringMap.add "string_length"
     { typ = Int; fname = "string_length"; formals = 
     [(String, "x")]; 
       locals = []; body = [] }

       (StringMap.add "string_compare"
     { typ = Int; fname = "string_compare"; formals = 
     [(String, "x"); (String, "x")]; 
       locals = []; body = [] }

       (StringMap.add "to_lower"
     { typ = Char; fname = "to_lower"; formals = 
     [(Char, "x")]; 
       locals = []; body = [] }

       (StringMap.add"print_char"
     { typ = Void; fname = "print_char"; formals = [(Char, "x")];
       locals = []; body = [] }

       (StringMap.singleton "print_string"
     { typ = Void; fname = "print_string"; formals = [(String, "x")];
       locals = []; body = [] }

     ))))))))))))

   in
     
  let function_decls = List.fold_left (fun m fd -> StringMap.add fd.fname fd m)
                         built_in_decls functions
  in

  let function_decl s = try StringMap.find s function_decls
       with Not_found -> raise (Failure ("unrecognized function " ^ s))
  in

  let struct_decls = List.fold_left (fun m st -> StringMap.add st.sname st m) 
                          StringMap.empty structs
  in
  
  let struct_decl s = try StringMap.find s struct_decls
      with Not_found -> raise (Failure ("unrecognized struct" ^ s))
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
      | Char_Lit _ -> Char
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
      | Unop(op, e) as ex -> let t = expr e in
	     (match op with
	         Neg when t = Int -> Int
         | Neg when t = Float -> Float
	       | Not when t = Bool -> Bool
         | _ -> raise (Failure ("illegal unary operator " ^ string_of_uop op ^
	  		   string_of_typ t ^ " in " ^ string_of_expr ex)))
      | Noexpr -> Void
      | Assign(var, e) as ex -> let lt = type_of_identifier var
                                and rt = expr e in
        check_assign lt rt (Failure ("illegal assignment " ^ string_of_typ lt ^
				     " = " ^ string_of_typ rt ^ " in " ^ 
				     string_of_expr ex))
      | Call(fname, actuals) as call -> let fd = function_decl fname in
         if List.length actuals != List.length fd.formals then
           raise (Failure ("expecting " ^ string_of_int
             (List.length fd.formals) ^ " arguments in " ^ string_of_expr call))
         else
           List.iter2 (fun (ft, _) e -> let et = expr e in
              ignore (check_assign ft et
                (Failure ("illegal actual argument found " ^ string_of_typ et ^
                " expected " ^ string_of_typ ft ^ " in " ^ string_of_expr e))))
             fd.formals actuals;
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

    stmt (Block func.body)
   
  in
  List.iter check_function functions
