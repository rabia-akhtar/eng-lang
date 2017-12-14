(* Ocamllex scanner for MicroC *)

{ open Parser }



rule token = parse
  [' ' '\t' '\r' '\n'] { token lexbuf } (* Whitespace *)
| "/*"     { comment lexbuf }           (* Comments *)
| '('      { LPAREN }
| ')'      { RPAREN }
| '{'      { LBRACE }
| '}'      { RBRACE }
| "{|"     { LINDEX }
| "|}"     { RINDEX }
| "of"     { OF }
| "["      { LSQUARE }
| "]"      { RSQUARE }
| ';'      { SEMI }
| ','      { COMMA }
| '+'      { PLUS }
| '-'      { MINUS }
| '*'      { TIMES }
| '/'      { DIVIDE }
| "++"     { INCREMENT }
| "--"     { DECREMENT }
| '='      { ASSIGN }
| "=="     { EQ }
| "!="     { NEQ }
| '<'      { LT }
| "<="     { LEQ }
| ">"      { GT }
| ">="     { GEQ }
| "&&"     { AND }
| "||"     { OR }
| "!"      { NOT }
| "if"     { IF }
| "else"   { ELSE }
| "for"    { FOR }
| "while"  { WHILE }
| "return" { RETURN }
| "int"    { INT }
| "float"  { FLOAT }
| "bool"   { BOOL }
| "void"   { VOID }
| "true"   { TRUE }
| "false"  { FALSE }
| "string" { STRING }
| "char"   { CHAR }
| "file_ptr" { STRING }
| "struct" { STRUCT }
| ['0'-'9']+ as lxm { NUM_LIT(int_of_string lxm) }
| ['0'-'9']+'.'['0'-'9']* | ['0'-'9']*'.'['0'-'9']+ 
	as lxm { FLOAT_LIT(float_of_string lxm)}
| '"' (([^ '"'] | "\\\"")* as strlit) '"' { STRING_LIT(strlit) } 
| '''([' '-'!' '#'-'[' ']'-'~' ]|['0'-'9'])''' as lxm {CHAR_LITERAL( String.get lxm 1)}
| ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']* as lxm { ID(lxm) }
| eof { EOF }
| _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

and comment = parse
  "*/" { token lexbuf }
| _    { comment lexbuf }
