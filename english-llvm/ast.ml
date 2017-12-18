(* Abstract Syntax Tree and functions for printing it *)

type op = Add | Sub | Mult | Div | Equal | Neq | Less | Leq | Greater | Geq |
          And | Or

type pop = 
  | Dec 
  | Inc

type dtyp = Int | String | Float |Char

type uop = Neg | Not

type typ = Simple of dtyp
    
    | Bool
    | Void
    | Array of dtyp * int
    | Struct of string

type bind = typ * string

type expr =
    (* Literal of int *)
    NumLit of int
  | FloatLit of float
  | BoolLit of bool
  | StringLit of string
  | ArrayLit of expr list
  | Index of expr * expr list
  | CharLit of char
  | Id of string
  | Binop of expr * op * expr
  | Unop of uop * expr
  | Pop of expr * pop 
  | Assign of expr * expr
  | ArrayAccess of string * expr 
  | ArrayAssign of string * expr list * expr
  | Call of string * expr list
  | Dot of expr * string
  | Noexpr

type var_decl = VarDecl of typ * string * expr

type struct_decl = {
    sname: string;
    sformals: var_decl list;
 }

type stmt =
    Block of stmt list
  | Expr of expr
  | Return of expr
  | If of expr * stmt * stmt
  | For of expr * expr * expr * stmt
  | While of expr * stmt

type func_decl = {
    typ : typ;
    fname : string;
    formals : bind list;
    locals : var_decl list;
    body : stmt list;
  }

type program = var_decl list * func_decl list * struct_decl list 

(* Pretty-printing functions *)

let string_of_op = function
    Add -> "+"
  | Sub -> "-"
  | Mult -> "*"
  | Div -> "/"
  | Equal -> "=="
  | Neq -> "!="
  | Less -> "<"
  | Leq -> "<="
  | Greater -> ">"
  | Geq -> ">="
  | And -> "&&"
  | Or -> "||"

let string_of_uop = function
    Neg -> "-"
  | Not -> "!"

let string_of_pop = function
    Inc -> "++"
  | Dec -> "--"

let convert_array l conversion joiner =
    let glob_item original data = original ^ (conversion data) ^ joiner in
    let full = (List.fold_left glob_item "" l) in
    "[" ^ String.sub full 0 ((String.length full) - 2) ^ "]"

let rec string_of_expr = function
    NumLit(l) -> string_of_int l
  | FloatLit(f) -> string_of_float f
  | BoolLit(true) -> "true"
  | BoolLit(false) -> "false"
  | StringLit(s) -> s
  | ArrayLit(l) -> convert_array l string_of_expr ", "
  | Index(e, l) -> string_of_expr e ^
                   "{|" ^ string_of_expr (List.hd l) ^ "|}"
  | CharLit(s) -> Char.escaped s
  | Id(s) -> s
  | Binop(e1, o, e2) ->
      string_of_expr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_expr e2
  | Unop(o, e) -> string_of_uop o ^ string_of_expr e
  | Pop(v, p) -> string_of_expr v ^ string_of_pop p
  | Assign(v, e) ->  string_of_expr v ^ " = " ^ string_of_expr e
  | Dot(e, s) ->  string_of_expr e ^ "." ^ s
  | Call(f, el) ->
      f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"
  | Noexpr -> ""
  | ArrayAccess(s,e2) -> (s) ^ "[" ^ (string_of_expr e2) ^ "]"
  | ArrayAssign(v, l, e) -> v ^ "[" ^ string_of_expr (List.hd l) ^ "]" ^ " = " ^ string_of_expr e

let rec string_of_stmt = function
    Block(stmts) ->
      "{\n" ^ String.concat "" (List.map string_of_stmt stmts) ^ "}\n"
  | Expr(expr) -> string_of_expr expr ^ ";\n";
  | Return(expr) -> "return " ^ string_of_expr expr ^ ";\n";
  | If(e, s, Block([])) -> "if (" ^ string_of_expr e ^ ")\n" ^ string_of_stmt s
  | If(e, s1, s2) ->  "if (" ^ string_of_expr e ^ ")\n" ^
      string_of_stmt s1 ^ "else\n" ^ string_of_stmt s2
  | For(e1, e2, e3, s) ->
      "for (" ^ string_of_expr e1  ^ " ; " ^ string_of_expr e2 ^ " ; " ^
      string_of_expr e3  ^ ") " ^ string_of_stmt s
  | While(e, s) -> "while (" ^ string_of_expr e ^ ") " ^ string_of_stmt s

let string_of_d_typ = function
  Int -> "int"
| String -> "string"
| Float -> "float"
| Char -> "char"

let rec repeat c = function
    0 -> ""
  | n -> c ^ (repeat c (n - 1))

let string_of_typ = function
    Bool -> "bool"
  | Void -> "void"
  | Simple(d) -> string_of_d_typ d
  | Array(d,n) -> string_of_d_typ d ^ repeat "[]" n
  | Struct(id) -> "struct" ^ id

let string_of_vdecl = function
  VarDecl(t, id, e)  -> string_of_typ t ^ " " ^ id ^  "=" ^ string_of_expr e ^ ";\n"

let string_of_fdecl fdecl =
  string_of_typ fdecl.typ ^ " " ^
  fdecl.fname ^ "(" ^ String.concat ", " (List.map snd fdecl.formals) ^
  ")\n{\n" ^
  String.concat "" (List.map string_of_vdecl fdecl.locals) ^
  String.concat "" (List.map string_of_stmt fdecl.body) ^
  "}\n"

let string_of_sdecl sdecl =
 "struct " ^ sdecl.sname ^ String.concat 
 "{\n" (List.map string_of_vdecl sdecl.sformals) ^ "\n}\n"

let string_of_program (vars, funcs, structs) =
  String.concat "" (List.map string_of_vdecl vars) ^ "\n" ^
  String.concat "\n" (List.map string_of_fdecl funcs) ^ "\n" ^
  String.concat "\n" (List.map string_of_sdecl structs)
