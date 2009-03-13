{
  (** Configuration lexer *)

  type var_line = {
    vl_left: string;
    vl_name: string;
    vl_equal: string;
    vl_value: string;
    vl_right: string;
  }

  type line =
    | Empty of string
    | Var of var_line

  let buf = Buffer.create 142
}

let blank = [' ' '\t' '\r']
let id = ['a'-'z' 'A'-'Z' '0'-'9' '_' '-']*
let value_char = [^' ' '\t' '\r' '\n' '#']
let comment = '#' [^'\n']*

rule line = parse
  | (blank* as left) (id as name) (blank* '=' blank* as equal)
      { let value, right = value lexbuf in
        Var {
          vl_left = left;
          vl_name = name;
          vl_equal = equal;
          vl_value = value;
          vl_right = right
        } }
  | [^'=' '\n']* as x
      { nl_or_eof lexbuf;
        Empty x }
  | eof
      { raise End_of_file }

and value = parse
  | '"'
      { Buffer.clear buf;
        quote lexbuf }
  | ((blank* value_char)* as value) (blank* comment? as right)
      { nl_or_eof lexbuf;
        value, right }

and quote = parse
  | [^'"' '\\']+ as x
      { Buffer.add_string buf x; quote lexbuf }
  | "\\n"
  | '\\' "\n" blank*
      { Buffer.add_char buf '\n'; quote lexbuf }
  | "\\r"
      { Buffer.add_char buf '\r'; quote lexbuf }
  | "\\t"
      { Buffer.add_char buf '\t'; quote lexbuf }
  | "\\\\"
  | '\\'
      { Buffer.add_char buf '\\'; quote lexbuf }
  | '"' (blank* comment? as right)
      { nl_or_eof lexbuf;
        let value = Buffer.contents buf in
        value, right }

and nl_or_eof = parse
  | '\n'
      { () }
  | eof
      { () }
  | _
      { failwith "Configlex parser implementation error" }
