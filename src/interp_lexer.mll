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
let format = '%' (any_char#curly_braces)+

rule token = parse
  (* Simple values *)
  | '$' (value_id as content)
      (* Use a phony position for the second piece, because it doesn't exist
         in the original code. *)
      {
        let range = pos ~offset:1 lexbuf in
        Variable ({ content; range }, { content = "%s"; range })
      }
  (* Values with printf-style formats *)
  | '$' '{' (any_expr as vid) ',' (whitespace as w)? (format as fmt) '}'
      {
        let base_offset = 2 in
        let p_vid = pos ~offset:base_offset lexbuf in
        let w_length =
          match w with
          | None -> 0
          | Some s -> String.length s
        in
        let fmt_offset = base_offset + String.length vid + 1 + w_length in
        let p_fmt = pos ~offset:fmt_offset lexbuf in
        Variable (
          { content = vid; range = p_vid},
          { content = fmt; range = p_fmt}
        )
      }
  (* Custom (value expression, printer expression) pairs *)
  | '$' '{' (any_expr as vid) ',' (whitespace as w)? (any_expr as func) '}'
      {
        let base_offset = 2 in
        let p_vid = pos ~offset:base_offset lexbuf in
        let w_length =
          match w with
          | None -> 0
          | Some s -> String.length s
        in
        let func_offset = base_offset + String.length vid + 1 + w_length in
        let p_func = pos ~offset:func_offset lexbuf in
        Custom_variable (
          { content = vid; range = p_vid },
          { content = func; range = p_func}
        )
      }
  (* Special character handling *)
  | '$' '$'
      { Literal { content = "$"; range = pos lexbuf } }
  | "\n" | "\r" | "\r\n"
      { Lexing.new_line lexbuf; Newline (Lexing.lexeme lexbuf) }
  | eof
      { Textend }
  (* Other content *)
  | [^ '$' '\\' '\r' '\n']+
      {
        let content = Lexing.lexeme lexbuf in
        Literal { content; range = pos lexbuf }
      }
  (* Anything else that may fall through the cracks *)
  | _
      {
        let content = Lexing.lexeme lexbuf in
        Literal { content; range = pos lexbuf }
      }
