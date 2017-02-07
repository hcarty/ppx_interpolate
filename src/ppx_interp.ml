open Ast_mapper
open Parsetree

let add_locs ~base ~offset extra =
  let open Lexing in
  { base with
(*
    pos_lnum = base.pos_lnum + offset.pos_lnum;
    pos_bol = base.pos_bol + offset.pos_bol;
*)
    pos_cnum = base.pos_cnum + offset.pos_cnum + extra; }

let make_location { Location.loc_start; _ } (s, e) delim =
  let extra =
    match delim with
    | None -> 1
    | Some s -> String.length s + 2
  in
  { Location.loc_start = add_locs ~base:loc_start ~offset:s extra;
    loc_end = add_locs ~base:loc_start ~offset:e extra;
    loc_ghost = true; }

let set_loc expr pexp_loc =
  { expr with pexp_loc }

let rec tokenize parent_location lexbuf delim (fmt_string, args) =
  let result =
    let open Interp_types in
    match Interp_lexer.token lexbuf with
    | Literal lit (** Literal string *) ->
      Some (lit.content :: fmt_string, args)
    | Variable (v, directive) (** ${expr, "%printf-like-format"} *) ->
      let v_expr = Parse.expression @@ Lexing.from_string v.content in
      let location = make_location parent_location v.range delim in
      Some (directive.content :: fmt_string, set_loc v_expr location :: args)
    | Custom_variable (v, fmt) (** [${expr, expr}] *) ->
      let v_expr = Parse.expression @@ Lexing.from_string v.content in
      let v_location = make_location parent_location v.range delim in
      let fmt_expr = Parse.expression @@ Lexing.from_string fmt.content in
      let fmt_location = make_location parent_location fmt.range delim in
      Some ("%a" :: fmt_string, (set_loc v_expr v_location) :: (set_loc fmt_expr fmt_location) :: args)
    | Textend (** The end of the quoted text *) ->
      None
  in
  match result with
  | Some accu -> tokenize parent_location lexbuf delim accu
  | None ->
    let fmt =
      String.concat "" (List.rev fmt_string)
      |> Ast_helper.Const.string
      |> Ast_helper.Exp.constant
      |> (fun e -> set_loc e parent_location)
    in
    let args = List.rev_map (fun e -> Asttypes.Nolabel, e) args in
    (Asttypes.Nolabel, fmt) :: args

let rec last l accu =
  match l with
  | [] -> None
  | [Asttypes.Nolabel, { pexp_desc = Pexp_constant (Pconst_string (fmt, delim)); pexp_loc }] -> Some (fmt, delim, pexp_loc, List.rev accu)
  | hd :: tl -> last tl (hd :: accu)

let fmt_mapper _args =
  {
    default_mapper with
    expr = (
      fun mapper expr ->
        match expr with
        | [%expr [%fmt [%e? exp]]] ->
          begin match exp.pexp_desc with
            | Pexp_apply (f, args) ->
              begin match last args [] with
                | None -> default_mapper.expr mapper expr
                | Some (fmt, delim, loc, rest) ->
                  let args = tokenize loc (Lexing.from_string fmt) delim ([], []) in
                  let generated = { exp with pexp_desc = Pexp_apply (f, rest @ args) } in
                  generated
(*
                  let pexp_loc =
                    (* [loc_ghost] tells the compiler and other tools than this is
                       generated code *)
                    { generated.pexp_loc with Location.loc_ghost = true }
                  in
                  { generated with pexp_loc }
*)
              end
            | _ -> default_mapper.expr mapper expr
          end
        | _ ->
          default_mapper.expr mapper expr
    )
  }

let () = register "fmt" fmt_mapper
