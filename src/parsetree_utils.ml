open Longident
open Location
open Asttypes
open Parsetree
open Ast_helper
open Ast_convenience

let deriver = "cmdliner"
let raise_errorf = Ppx_deriving.raise_errorf

let argn = Printf.sprintf "arg%d"

let expr_opt ~kind =
  function
  | None -> [%expr None]
  | Some x -> [%expr Some [%e kind x]]

let attr_int_encoding attrs =
  match Ppx_deriving.attr ~deriver "encoding" attrs |>
        Ppx_deriving.Arg.(get_attr ~deriver (enum ["string"; "number"])) with
  | Some "string" -> `String
  | Some "number" | None -> `Int
  | _ -> assert false

let attr_char_opt name attrs =
  Ppx_deriving.attr ~deriver name attrs |>
  Ppx_deriving.Arg.(get_attr ~deriver char)

let attr_string name default attrs =
  match Ppx_deriving.attr ~deriver name attrs |>
        Ppx_deriving.Arg.(get_attr ~deriver string)with
  | Some x -> x
  | None   -> default

let attr_string_opt name attrs =
  Ppx_deriving.attr ~deriver name attrs |>
  Ppx_deriving.Arg.(get_attr ~deriver string)

let attr_int_opt name attrs =
  Ppx_deriving.attr ~deriver name attrs |>
  Ppx_deriving.Arg.(get_attr ~deriver int)

let attr_key  = attr_string "key"
let attr_name = attr_string "name"

let attr_default attrs =
  Ppx_deriving.attr ~deriver "default" attrs |>
  Ppx_deriving.Arg.(get_attr ~deriver expr)

let attr_expr attrs name =
  Ppx_deriving.attr ~deriver name attrs |>
  Ppx_deriving.Arg.(get_attr ~deriver expr)

let clize_flag =  String.map (fun c -> if c = '_' then '-' else c)

let parse_options options =
  options |> List.iter (fun (name, expr) ->
    match name with
    | _ -> raise_errorf ~loc:expr.pexp_loc "%s does not support option %s" deriver name)

let attr_default attrs =
  Ppx_deriving.(attrs |> attr ~deriver "default" |> Arg.(get_attr ~deriver expr))

let attr_split attrs =
  Ppx_deriving.(attrs |> attr ~deriver "split" |> Arg.get_flag ~deriver)
