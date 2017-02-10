(** (start, end) range *)
type range = Lexing.position * Lexing.position

(** {1 Potential Clause Values} *)
type clause =
  | Literal of seg (** Literal string *)
  | Variable of seg * seg (** ${expr, "%printf-like-format"} *)
  | Custom_variable of seg * seg (** [${expr, expr}] *)
  | Newline of string (** Newline, with the literal representation *)
  | Textend (** The end of the quoted text *)
and seg = {
  content : string; (** Parsed content *)
  range : Lexing.position * Lexing.position; (** Position range of content *)
}
