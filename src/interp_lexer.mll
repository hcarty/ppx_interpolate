{
  open Interp_types

  let pos ?(offset = 0) lexbuf =
    let first = Lexing.lexeme_start_p lexbuf in
    let last = Lexing.lexeme_end_p lexbuf in
    { first with Lexing.pos_cnum = first.Lexing.pos_cnum + offset },
    { last with Lexing.pos_cnum = last.Lexing.pos_cnum + offset }
}

(* For matching basic OCaml identifiers *)
let whitespace = [ ' ' '\t' ]*
let ucletter = [ 'A' - 'Z' ]
let lcletter = [ 'a' - 'z' '_' ]
let acletter = ucletter | lcletter
let value_id = ( acletter+ '.' )* lcletter acletter *

(* For matching any OCaml expression in a ${ } *)
let any_char = _
let curly_braces = [ '{' '}' ]
let any_expr = (any_char#curly_braces)+

(* For matching printf-like format descriptors *)
let format = '%'
             [ '0' '-' ' ' ]*    (* no more modifiers are supported by Ocaml *)
             ['0'-'9']*
             ( '.' ['0'-'9']* )?
             ( ( ['L' 'l' 'n'] [ 'd' 'i' 'u' 'x' 'X' 'o' ])
               | [ 'd' 'i' 'u' 'x' 'X' 's' 'c' 'f' 'e' 'E' 'g' 'G' 'b' 't' ]
             )

rule token = parse
  (* Simple values *)
  | '$' (value_id as content)
      (* Use a phony position for the second piece, because it doesn't exist
         in the original code. *)
      {
        prerr_endline @@ Lexing.lexeme lexbuf;
        let loc' = { Location.loc_start = Lexing.lexeme_start_p lexbuf;
                     Location.loc_end = Lexing.lexeme_end_p lexbuf;
                     Location.loc_ghost = false } in
        prerr_endline @@ Format.asprintf "%a" Location.print loc';
        let range = pos ~offset:5 lexbuf in
        prerr_endline @@ Lexing.lexeme lexbuf;
        let loc' = { Location.loc_start = Lexing.lexeme_start_p lexbuf;
                     Location.loc_end = Lexing.lexeme_end_p lexbuf;
                     Location.loc_ghost = false } in
        prerr_endline @@ Format.asprintf "%a" Location.print loc';
        Variable ({ content; range }, { content = "%s"; range })
      }
  | '$' '(' (value_id as content) ')'
      {
        let range = pos ~offset:3 lexbuf in
        Variable ({ content; range}, { content = "%s"; range})
      }
  (* Values with printf-style formats *)
  | '$' '{' (any_expr as vid) ',' (whitespace as w)? (format as fmt) '}'
      {
        let base_offset = 6 in
        let p_vid = pos ~offset:base_offset lexbuf in
        let w_length =
          match w with
          | None -> 0
          | Some s -> String.length s
        in
        let fmt_offset = base_offset + String.length vid + 1 + w_length in
        let p_fmt = pos ~offset:fmt_offset lexbuf in
        Variable (
          { content = ex [] (Lexing.from_string vid); range = p_vid},
          { content = fmt; range = p_fmt}
        )
      }
  (* Custom (value expression, printer expression) pairs *)
  | '$' '{' (any_expr as vid) ',' (any_expr as func) '}'
      {
        let p_vid = pos ~offset:3 lexbuf in
        let func_offset = 3 + String.length vid + 1 in
        let p_func = pos ~offset:func_offset lexbuf in
        Custom_variable (
          { content = ex [] (Lexing.from_string vid); range = p_vid },
          { content = func; range = p_func}
        )
      }
  (* Everything else! *)
  | '\\' '\n'
      { Literal { content = ""; range = pos lexbuf } }
  | '$' '$'
      { Literal { content = "$"; range = pos lexbuf } }
  | '\\' [ '0'-'9' ] [ '0'-'9' ] [ '0'-'9' ]
      {
        let s = Lexing.lexeme lexbuf in
        let n = int_of_string(String.sub s 1 3) in
        let content = Printf.sprintf "%c" (Char.chr n) in
        Literal { content; range = pos lexbuf }
      }
  | '\\' 'x' [ '0'-'9' 'a'-'f' 'A'-'F' ] [ '0'-'9' 'a'-'f' 'A'-'F' ]
      {
        let s = Lexing.lexeme lexbuf in
        let n = int_of_string("0" ^ String.sub s 1 3) in
        let content = Printf.sprintf "%c" (Char.chr n) in
        Literal { content; range = pos lexbuf }
      }

  | '\\' _
      {
        let content = Lexing.lexeme lexbuf in
        Literal { content; range = pos lexbuf }
      }
  | [^ '$' '\\']+
      {
        let content = Lexing.lexeme lexbuf in
        Literal { content; range = pos lexbuf }
      }
  | eof
      { Textend }
  | _
      {
        let content = Lexing.lexeme lexbuf in
        Literal { content; range = pos lexbuf }
      }
and ex acc = parse
  (* For the future... this is where any special quoting would go *)
  | any_char as c
      { ex (c :: acc) lexbuf }
  | eof
      {
        let chars = Array.of_list (List.rev acc) in
        let s = Bytes.make (Array.length chars) ' ' in
        Array.iteri (fun i c -> Bytes.set s i c) chars;
        Bytes.to_string s
      }
