(**************************************************************************)
(* Copyright (c) 2009, Romain BARDOU                                      *)
(* All rights reserved.                                                   *)
(*                                                                        *)
(* Redistribution and  use in  source and binary  forms, with  or without *)
(* modification, are permitted provided that the following conditions are *)
(* met:                                                                   *)
(*                                                                        *)
(* * Redistributions  of  source code  must  retain  the above  copyright *)
(*   notice, this list of conditions and the following disclaimer.        *)
(* * Redistributions in  binary form  must reproduce the  above copyright *)
(*   notice, this list of conditions  and the following disclaimer in the *)
(*   documentation and/or other materials provided with the distribution. *)
(* * Neither the name of Fury Puyo nor  the names of its contributors may *)
(*   be used  to endorse or  promote products derived  from this software *)
(*   without specific prior written permission.                           *)
(*                                                                        *)
(* THIS SOFTWARE  IS PROVIDED BY  THE COPYRIGHT HOLDERS  AND CONTRIBUTORS *)
(* "AS  IS" AND  ANY EXPRESS  OR IMPLIED  WARRANTIES, INCLUDING,  BUT NOT *)
(* LIMITED TO, THE IMPLIED  WARRANTIES OF MERCHANTABILITY AND FITNESS FOR *)
(* A PARTICULAR PURPOSE  ARE DISCLAIMED. IN NO EVENT  SHALL THE COPYRIGHT *)
(* OWNER OR CONTRIBUTORS BE  LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, *)
(* SPECIAL,  EXEMPLARY,  OR  CONSEQUENTIAL  DAMAGES (INCLUDING,  BUT  NOT *)
(* LIMITED TO, PROCUREMENT OF SUBSTITUTE  GOODS OR SERVICES; LOSS OF USE, *)
(* DATA, OR PROFITS; OR BUSINESS  INTERRUPTION) HOWEVER CAUSED AND ON ANY *)
(* THEORY OF  LIABILITY, WHETHER IN  CONTRACT, STRICT LIABILITY,  OR TORT *)
(* (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING  IN ANY WAY OUT OF THE USE *)
(* OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.   *)
(**************************************************************************)

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
let value_char = [^' ' '\t' '\r' '\n' '#' '"']
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
