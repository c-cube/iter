(*
Zipperposition: a functional superposition prover for prototyping
Copyright (C) 2012 Simon Cruanes

This is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.

This is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301 USA.
*)

(* {1 Basic S-expressions, with printing and parsing} *)

(** S-expression *)
type t =
  | Atom of string  (** An atom *)
  | List of t list  (** A list of S-expressions *)

type token = [ `Open | `Close | `Atom of string ]
(** Token that compose a Sexpr once serialized *)

(** {2 Traverse an iterator of tokens} *)

val iter : (token -> unit) -> t -> unit
(** Iterate on the S-expression, calling the callback with tokens *)

val traverse : t -> token Iter.t
(** Traverse. This yields an iterator of tokens *)

val validate : token Iter.t -> token Iter.t
(** Returns the same iterator of tokens, but during iteration, if
      the structure of the Sexpr corresponding to the iterator
      is wrong (bad parenthesing), Invalid_argument is raised
      and iteration is stoped *)

(** {2 Text <-> tokens} *)

val lex : char Iter.t -> token Iter.t
(** Lex: create an iterator of tokens from the given iterator of chars. *)

val of_seq : token Iter.t -> t
(** Build a Sexpr from an iterator of tokens, or raise Failure *)

(** {2 Printing} *)

val pp_token : Format.formatter -> token -> unit
(** Print a token on the given formatter *)

val pp_tokens : Format.formatter -> token Iter.t -> unit
(** Print an iterator of Sexpr tokens on the given formatter *)

val pp_sexpr : ?indent:bool -> Format.formatter -> t -> unit
(** Pretty-print the S-expr. If [indent] is true, the S-expression
      is printed with indentation. *)

(** {2 Serializing} *)

val output_seq : string -> token Iter.t -> (token -> unit) -> unit
(** print a pair "(name @,iterator)" *)

val output_str : string -> string -> (token -> unit) -> unit
(** print a pair "(name str)" *)

(** {2 Parsing} *)

(** Monadic combinators for parsing data from an iterator of tokens,
    without converting to concrete S-expressions. *)

type 'a parser

exception ParseFailure of string

val ( >>= ) : 'a parser -> ('a -> 'b parser) -> 'b parser
(** Monadic bind: computes a parser from the result of
      the first parser *)

val ( >> ) : 'a parser -> 'b parser -> 'b parser
(** Like (>>=), but ignores the result of the first parser *)

val return : 'a -> 'a parser
(** Parser that consumes no input and return the given value *)

val fail : string -> 'a parser
(** Fails parsing with the given message *)

val one : (token -> 'a) -> 'a parser
(** consumes one token with the function *)

val skip : unit parser
(** Skip the token *)

val lookahead : (token -> 'a parser) -> 'a parser
(** choose parser given current token *)

val left : unit parser
(** Parses a `Open *)

val right : unit parser
(** Parses a `Close *)

val pair : 'a parser -> 'b parser -> ('a * 'b) parser
val triple : 'a parser -> 'b parser -> 'c parser -> ('a * 'b * 'c) parser

val ( ^|| ) : string * (unit -> 'a parser) -> 'a parser -> 'a parser
(** [(name,p) ^|| p'] behaves as [p ()] if the next token is [`Atom name], and
      like [p'] otherwise *)

val map : 'a parser -> ('a -> 'b) -> 'b parser
(** Maps the value returned by the parser *)

val p_str : string parser
val p_int : int parser
val p_bool : bool parser
val many : 'a parser -> 'a list parser
val many1 : 'a parser -> 'a list parser

val parse : 'a parser -> token Iter.t -> 'a
(** Parses exactly one value from the iterator of tokens. Raises
      ParseFailure if anything goes wrong. *)

val parse_seq : 'a parser -> token Iter.t -> 'a Iter.t
(** Parses an iterator of values *)
