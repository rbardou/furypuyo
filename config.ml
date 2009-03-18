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

open Configlex
open Printf

let cfgdir, set_cfgdir =
  let dir = ref None in
  (fun () ->
     match !dir with
       | None -> failwith "Module Config was not initialized."
       | Some dir -> dir),
  (fun d -> dir := Some d)

let some_nf = function
  | None -> raise Not_found
  | Some x -> x

let convert dir =
  let home = try Sys.getenv "HOME" with Not_found -> "." in
  if String.length dir >= 2 && dir.[0] = '~' && dir.[1] = '/' then
    Filename.concat home (String.sub dir 2 (String.length dir - 2))
  else if dir = "~" then
    home
  else
    dir

let init ?var dir =
  let dir =
    try
      Sys.getenv (some_nf var)
    with Not_found ->
      dir
  in
  set_cfgdir (convert dir)

let filename file =
  if Filename.is_implicit file then
    Filename.concat (cfgdir ()) file
  else
    file

type var_contents = {
  mutable vc_already_exists: bool;
    (* only used by save *)
  vc_desc: string;
  vc_print: unit -> string;
}

type text_file = {
  tf_name: string;
  tf_desc: string;
  tf_contents: line list;
    (* lines are stored in reverse order *)
  tf_vars: (string, var_contents) Hashtbl.t;
    (* all keys are stored uppercase *)
}

type 'a var = {
  mutable v_value: 'a;
  v_contents: var_contents;
}

let get x = x.v_value

let set x y = x.v_value <- y

let rec find_var var = function
  | [] ->
      raise Not_found
  | Empty _ :: rem ->
      find_var var rem
  | Var vl :: rem ->
      if String.uppercase vl.vl_name = var then vl.vl_value else
        find_var var rem

let find_var v = find_var (String.uppercase v)

let load file desc =
  let file = filename file in
  let contents = ref [] in
  if Sys.file_exists file then begin
    let i = open_in file in
    let lexbuf = Lexing.from_channel i in
    begin try
      while true do
        contents := Configlex.line lexbuf :: !contents
      done
    with End_of_file ->
      ()
    end
  end;
  {
    tf_name = file;
    tf_desc = desc;
    tf_contents = !contents;
    tf_vars = Hashtbl.create 42;
  }

let save file =
  Hashtbl.iter (fun _ vc -> vc.vc_already_exists <- false) file.tf_vars;
  let contents = List.map
    (function
       | Empty _ as x -> x
       | Var vl ->
           let value =
             try
               let vc =
                 Hashtbl.find file.tf_vars (String.uppercase vl.vl_name) in
               if vc.vc_already_exists then raise Not_found;
               vc.vc_already_exists <- true;
               vc.vc_print ()
             with Not_found ->
               vl.vl_value
           in
           Var { vl with vl_value = value })
    file.tf_contents
  in
  let out = open_out file.tf_name in
  if file.tf_contents = [] then
    fprintf out "# %s\n" file.tf_desc;
  List.iter
    (function
       | Empty s ->
           fprintf out "%s\n" s
       | Var vl ->
           fprintf out "%s%s%s%s%s\n"
             vl.vl_left vl.vl_name vl.vl_equal vl.vl_value vl.vl_right)
    (List.rev contents);
  Hashtbl.iter
    (fun vn vc ->
       if not vc.vc_already_exists then
         fprintf out "\n# %s\n%s = %s\n" vc.vc_desc vn (vc.vc_print ()))
    file.tf_vars;
  fprintf out "%!";
  close_out out

let custom of_string to_string file var_name desc def =
  let value =
    try
      match of_string (find_var var_name file.tf_contents) with
        | None -> def
        | Some v -> v
    with Not_found ->
      def
  in
  let rec var = {
    v_value = value;
    v_contents = {
      vc_already_exists = false; (* value is not important here *)
      vc_desc = desc;
      vc_print = (fun () -> to_string var.v_value);
    };
  } in
  Hashtbl.add file.tf_vars (String.uppercase var_name) var.v_contents;
  var

let quoted x = "\"" ^ String.escaped x ^ "\""

let string = custom (fun x -> Some x) (fun x -> quoted x)

let int =
  custom
    (fun x -> try Some (int_of_string x) with _ -> None)
    string_of_int

let float =
  custom
    (fun x -> try Some (float_of_string x) with _ -> None)
    string_of_float

let bool =
  let o x = match String.uppercase x with
    | "YES" | "TRUE" -> Some true
    | "NO" | "FALSE" -> Some false
    | _ ->
        try
          Some (int_of_string x <> 0)
        with _ ->
          try
            Some (float_of_string x <> 0.)
          with _ ->
            None
  in
  let t = function
    | true -> "yes"
    | false -> "no"
  in
  custom o t
