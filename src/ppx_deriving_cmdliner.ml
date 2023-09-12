open Ppxlib
open Ast_helper

module Ast_builder_default_loc = struct
  include Ppx_deriving.Ast_convenience

  let gen_def_loc f x =
    let loc = !Ast_helper.default_loc in
    f ~loc x

  let lid = gen_def_loc Ast_builder.Default.Located.lident
  let list = gen_def_loc Ast_builder.Default.elist
  let pstr = gen_def_loc Ast_builder.Default.pstring
  let plist = gen_def_loc Ast_builder.Default.plist
  let lam = gen_def_loc Ast_builder.Default.pexp_fun Nolabel None
end

open Ast_builder_default_loc

let deriver = "cmdliner"
let raise_errorf = Ppx_deriving.raise_errorf

let argn = Printf.sprintf "arg%d"

let char c =
  Ast_helper.Exp.constant (Ast_helper.Const.char c)

let expr_opt ~loc ~kind =
  function
  | None -> [%expr None]
  | Some x -> [%expr (Some [%e kind x])]

let key_attr_exists attrs name =
  Ppx_deriving.attr ~deriver name attrs |>
  Ppx_deriving.Arg.get_flag ~deriver

let value_attr_exists attrs name =
  match Ppx_deriving.attr ~deriver name attrs |>
        Ppx_deriving.Arg.(get_attr ~deriver expr) with
  | Some _ -> true
  | None -> false

let attr_char_opt name attrs =
  Ppx_deriving.attr ~deriver name attrs |>
  Ppx_deriving.Arg.(get_attr ~deriver char)

let attr_string name default attrs =
  match Ppx_deriving.attr ~deriver name attrs |>
        Ppx_deriving.Arg.(get_attr ~deriver string) with
  | Some x -> String.trim x
  | None   -> default

let attr_string_opt name attrs =
  match Ppx_deriving.attr ~deriver name attrs |>
        Ppx_deriving.Arg.(get_attr ~deriver string) with
  | Some x -> Some (String.trim x)
  | None   -> None

let attr_int_opt name attrs =
  Ppx_deriving.attr ~deriver name attrs |>
  Ppx_deriving.Arg.(get_attr ~deriver int)

let attr_expr attrs name =
  Ppx_deriving.attr ~deriver name attrs |>
  Ppx_deriving.Arg.(get_attr ~deriver expr)

let attr_expr_exn attrs name =
  match attr_expr attrs name with
  | None -> failwith "attr_expr_exn: expected an expression"
  | Some e -> e

let clize_flag =  String.map (fun c -> if c = '_' then '-' else c)

let parse_options options =
  options |> List.iter (fun (name, expr) ->
    match name with
    | _ -> raise_errorf ~loc:expr.pexp_loc "%s does not support option %s"
             deriver name)


let rec converter_for ?list_sep ?enum typ =
  let loc = typ.ptyp_loc in
  let list_sep' = expr_opt ~loc ~kind:char list_sep in
  match enum, typ with
  | _, [%type: [%t? typ] list] ->
    [%expr (Cmdliner.Arg.list ?sep:[%e list_sep'] [%e converter_for ?enum typ])]
  | _, [%type: [%t? typ] array] ->
    [%expr (Cmdliner.Arg.array ?sep:[%e list_sep'] [%e converter_for ?enum typ])]
  | _, [%type: [%t? typ1] * [%t? typ2]] ->
    [%expr (Cmdliner.Arg.t2 ?sep:[%e list_sep']
              [%e converter_for ?enum typ1]
              [%e converter_for ?enum typ2])]
  | _, [%type: [%t? typ1] * [%t? typ2] * [%t? typ3]] ->
    [%expr (Cmdliner.Arg.t3 ?sep:[%e list_sep']
              [%e converter_for ?enum typ1]
              [%e converter_for ?enum typ2]
              [%e converter_for ?enum typ3])]
  | _, [%type: [%t? typ1] * [%t? typ2] * [%t? typ3] * [%t? typ4]] ->
    [%expr (Cmdliner.Arg.t4 ?sep:[%e list_sep']
              [%e converter_for ?enum typ1]
              [%e converter_for ?enum typ2]
              [%e converter_for ?enum typ3]
              [%e converter_for ?enum typ4])]
  | Some xs, _ -> [%expr Cmdliner.Arg.enum [%e xs]]
  | _, [%type: int] -> [%expr Cmdliner.Arg.int]
  | _, [%type: int32] | _, [%type: Int32.t] -> [%expr Cmdliner.Arg.int32]
  | _, [%type: int64] | _, [%type: Int64.t] -> [%expr Cmdliner.Arg.int64]
  | _, [%type: nativeint] | _, [%type: Nativeint.t] ->
    [%expr Cmdliner.Arg.nativeint]
  | _, [%type: float] -> [%expr Cmdliner.Arg.float]
  | _, [%type: bool] -> [%expr Cmdliner.Arg.bool]
  | _, [%type: string] -> [%expr Cmdliner.Arg.string]
  | _, [%type: char] -> [%expr Cmdliner.Arg.char]
  | _, [%type: [%t? typ] option] -> converter_for ?list_sep ?enum typ
  | _, [%type: bytes] ->
    [%expr Cmdliner.Arg.conv
        ((fun s -> Result.Ok (Bytes.of_string s)),
         (fun fmt b -> Format.fprintf fmt "[@%s@]@." (Bytes.to_string b)))]
         (* ^ Print inside a pretty box to ignore raw new lines in comments. *)
  | _, { ptyp_desc = Ptyp_constr ({ txt = lid }, args) } ->
    let fn_name = Ppx_deriving.mangle_lid (`Suffix "cmdliner_converter") lid in
    [%expr [%e Exp.ident (mknoloc fn_name)]]
  | _, _ ->
    failwith
      (Printf.sprintf
         "Ppx_deriving_cmdliner: converter_for doesn't support: `%s`"
         (Ppx_deriving.string_of_core_type typ))


let rec docv_for ?list_sep typ =
  let s = (match list_sep with None -> ',' | Some s -> s) in
  match typ with
  | [%type: int] -> "INT"
  | [%type: int32] | [%type: Int32.t] -> "INT32"
  | [%type: int64] | [%type: Int64.t] -> "INT64"
  | [%type: nativeint] | [%type: Nativeint.t] -> "NATIVEINT"
  | [%type: float] -> "FLOAT"
  | [%type: bool] -> "BOOL"
  | [%type: string] -> "STRING"
  | [%type: char] -> "CHAR"
  | [%type: bytes] -> "BYTES"
  | [%type: [%t? typ] option] -> docv_for typ
  | [%type: [%t? typ] list] | [%type: [%t? typ] array] ->
    Printf.sprintf "%s%c..." (docv_for typ) s
  | [%type: [%t? typ1] * [%t? typ2]] ->
    Printf.sprintf "%s%c%s"
      (docv_for typ1) s (docv_for typ2)
  | [%type: [%t? typ1] * [%t? typ2] * [%t? typ3]] ->
    Printf.sprintf "%s%c%s%c%s"
      (docv_for typ1) s (docv_for typ2) s (docv_for typ3)
  | [%type: [%t? typ1] * [%t? typ2] * [%t? typ3] * [%t? typ4]] ->
    Printf.sprintf "%s%c%s%c%s%c%s"
      (docv_for typ1) s
      (docv_for typ2) s
      (docv_for typ3) s
      (docv_for typ4)
  | _ -> "VAR"



let info_for ~is_pos ~attrs ~name ?list_sep ~typ ~env =
  let loc = typ.ptyp_loc in
  let name' = match attr_string_opt "name" attrs with
  | None -> str (clize_flag name)
  | Some s -> str (clize_flag s)
  in
  let aka = match attr_expr attrs "aka" with
  | None -> [%expr []]
  | Some e -> e
  in
  let docv' = match attr_string_opt "docv" attrs with
  | None -> str (docv_for ?list_sep typ)
  | Some d -> str d in
  let doc' = attr_string_opt "ocaml.doc" attrs |> expr_opt ~loc ~kind:str  in
  let docs' = attr_string_opt "docs" attrs |> expr_opt ~loc ~kind:str in
  let names = if is_pos then
    [%expr []]
  else
    [%expr [%e name'] :: [%e aka]]
  in
  [%expr info ?env:[%e env]
      ?docs:[%e docs'] ?doc:[%e doc'] ~docv:[%e docv'] [%e names]]


let rec ser_expr_of_typ typ attrs name =
  let loc = typ.ptyp_loc in
  let default' = attr_expr attrs "default" in
  let env' =
    let docs' = attr_string_opt "env.docs" attrs |> expr_opt ~loc ~kind:str in
    let doc' = attr_string_opt "env.doc" attrs |> expr_opt ~loc ~kind:str in
    match attr_string_opt "env" attrs with
    | None -> [%expr None]
    | Some e -> [%expr Some
        (Cmdliner.Arg.env_var ?docs:[%e docs'] ?doc:[%e doc'] [%e str e])]
  in
  let list_sep = attr_char_opt "sep" attrs in
  let enum = attr_expr attrs "enum" in
  let conv' = match attr_expr attrs "conv" with
  | None -> converter_for ?list_sep ?enum typ
  | Some conv -> conv
  in
  let pos = attr_int_opt "pos" attrs in
  let is_pos = pos<>None || key_attr_exists attrs "pos_all" in
  let info' = info_for ~is_pos ~attrs ~name ?list_sep ~typ ~env:env' in
  match typ with
  | _ when value_attr_exists attrs "term" ->
    attr_expr_exn attrs "term"
  | [%type: bool] ->
    begin match enum with
    | None ->  begin match default' with
      | None | Some [%expr false] ->
        [%expr Cmdliner.Arg.(value & flag & [%e info'])]
      | Some [%expr true] -> [%expr
        Cmdliner.Term.app
          (Cmdliner.Term.const (fun b -> (not b)))
          Cmdliner.Arg.(value & flag & [%e info'])]
      | Some _ -> failwith "Default of a `bool` must be a bool."
      end
    | Some _ ->
      let t = match pos with
      | None -> [%expr Cmdliner.Arg.opt]
      | Some i -> [%expr Cmdliner.Arg.pos [%e int i]]
      in
      begin match default' with
      | None ->
        [%expr Cmdliner.Arg.(required & [%e t] (some [%e conv']) None & [%e info'])]
      | Some [%expr false] ->
        [%expr Cmdliner.Arg.(value & [%e t] [%e conv'] false & [%e info'])]
      | Some [%expr true] ->
        [%expr Cmdliner.Arg.(value & [%e t] [%e conv'] true & [%e info'])]
      | Some _ -> failwith "Default of a `bool` must be a bool."
      end
    end
  | [%type: [%t? typ] option] ->
    begin match default' with
    | None -> [%expr
      Cmdliner.Arg.(value & opt (some [%e conv']) None & [%e info'])]
    | Some _ -> failwith "`option` types shouldn't have defaults."
    end
  | [%type: [%t? typ] list]
    when key_attr_exists attrs "opt_all" ->
    let conv' = match attr_expr attrs "conv" with
    | None -> converter_for ?list_sep ?enum:(attr_expr attrs "enum") typ
    | Some conv -> conv
    in
    begin match default' with
    | None -> [%expr
      Cmdliner.Arg.(value & opt_all [%e conv'] [] & [%e info'])]
    | Some d -> [%expr
      Cmdliner.Arg.(value & opt_all [%e conv'] [%e d] & [%e info'])]
    end

  | [%type: [%t? typ] list]
    when key_attr_exists attrs "pos_all" ->
    let conv' = match attr_expr attrs "conv" with
    | None -> converter_for ?list_sep ?enum:(attr_expr attrs "enum") typ
    | Some conv -> conv
    in
    [%expr Cmdliner.Arg.(value & pos_all [%e conv'] [] & [%e info'])]
  | [%type: unit] -> failwith "`unit` is not supported in Ppx_deriving_cmdliner"
  | [%type: int] | [%type: int32] | [%type: Int32.t] | [%type: int64]
  | [%type: Int64.t] | [%type: nativeint] | [%type: Nativeint.t]
  | [%type: float] | [%type: string] | [%type: char] | [%type: bytes]
  | [%type: [%t? _] list] | [%type: [%t? _] array]
  | _ ->
    let t = match pos with
    | None -> [%expr Cmdliner.Arg.opt]
    | Some i -> [%expr Cmdliner.Arg.pos [%e int i]]
    in
    begin match default' with
    | None -> [%expr
      Cmdliner.Arg.(required & [%e t] (some [%e conv']) None & [%e info'])]
    | Some d -> [%expr
      Cmdliner.Arg.(value & [%e t] [%e conv'] [%e d] & [%e info'])]
    end

let wrap_runtime decls =
  Ppx_deriving.sanitize ~module_:(Lident "Ppx_deriving_cmdliner_runtime") decls


let ser_sig_of_type_ext ~options ~path type_ext = []


let ser_type_of_decl ~options ~path type_decl =
  ignore (parse_options options);
  let typ = Ppx_deriving.core_type_of_type_decl type_decl in
  let loc = typ.ptyp_loc in
  let polymorphize = Ppx_deriving.poly_arrow_of_type_decl
      (fun var -> [%type: unit -> [%t var] Cmdliner.Term.t ]) type_decl in
  polymorphize [%type: unit -> [%t typ] Cmdliner.Term.t]


let ser_str_of_type ~options ~path ({ ptype_loc = loc } as type_decl) =
  ignore (parse_options options);
  let polymorphize = Ppx_deriving.poly_fun_of_type_decl type_decl in
  let typ = Ppx_deriving.core_type_of_type_decl type_decl in
  match type_decl.ptype_kind with
  | Ptype_open -> begin
    let to_cmdliner_name = Ppx_deriving.mangle_type_decl (`Suffix "cmdliner_term") type_decl in
    let mod_name = Ppx_deriving.mangle_type_decl
      (`PrefixSuffix ("M", "cmdliner_term")) type_decl
    in
    match type_decl.ptype_manifest with
    | Some ({ ptyp_desc = Ptyp_constr ({ txt = lid }, args) } as manifest) ->
      let ser = ser_expr_of_typ manifest [] "" in
      let lid = Ppx_deriving.mangle_lid (`PrefixSuffix ("M", "cmdliner_term")) lid in
      let orig_mod = Mod.ident (mknoloc lid) in
      ([Str.module_ (Mb.mk (mknoloc (Some mod_name)) orig_mod)],
       [Vb.mk (pvar to_cmdliner_name)
              (polymorphize [%expr ([%e ser] : unit -> [%t typ] Cmdliner.Term.t)])])
    | Some _ ->
      raise_errorf ~loc "%s: extensible type manifest should be a type name" deriver
    | None ->
      let poly_vars = List.rev
          (Ppx_deriving.fold_left_type_decl (fun acc name -> name :: acc) [] type_decl)
      in
      let polymorphize_ser  = Ppx_deriving.poly_arrow_of_type_decl
        (fun var -> [%type: unit -> [%t var] Cmdliner.Term.t ]) type_decl
      in
      let ty = Typ.poly poly_vars (polymorphize_ser [%type: unit -> [%t typ] Cmdliner.Term.t]) in
      let default_fun =
        let type_path = String.concat "." (path @ [type_decl.ptype_name.txt]) in
        let e_type_path = Exp.constant (Pconst_string (type_path, loc, None)) in
        [%expr fun _ ->
          invalid_arg ("ppx_deriving_cmdliner: Maybe a [@@deriving cmdliner] is missing when extending the type "^
                       [%e e_type_path])]
      in
      let poly_fun = polymorphize default_fun in
      let poly_fun =
        (Ppx_deriving.fold_left_type_decl (fun exp name -> Exp.newtype name exp) poly_fun type_decl)
      in
      let mod_name = "M_"^to_cmdliner_name in
      let typ = Type.mk ~kind:(Ptype_record [Type.field ~mut:Mutable (mknoloc "f") ty])
                              (mknoloc "t_cmdliner_term")
      in
      let record = Vb.mk (pvar "f") (Exp.record [lid "f", poly_fun] None) in
      let flid = lid (Printf.sprintf "%s.f" mod_name) in
      let field = Exp.field (Exp.ident flid) (flid) in
      let mod_ =
        Str.module_ (Mb.mk (mknoloc (Some mod_name))
                       (Mod.structure [
                           Str.type_ Nonrecursive [typ];
          Str.value Nonrecursive [record];
        ]))
      in
      ([mod_],
       [Vb.mk (pvar to_cmdliner_name) [%expr fun x -> [%e field] x]])
  end
  | kind ->
    let serializer =
      match kind, type_decl.ptype_manifest with
      | Ptype_record labels, _ ->
        let field_names =
          List.map
            (fun { pld_name = { txt = name; } } ->
               name, evar name) labels in
        let sub_terms =
          labels
          |> List.mapi (fun i { pld_name = { txt = name }; pld_type; pld_attributes } ->
              [%expr [%e ser_expr_of_typ pld_type pld_attributes name]]
            )
        in
        let make_type =
          List.fold_left (fun accum { pld_name = { txt = name }; _} ->
              Exp.fun_ Label.nolabel None (pvar name) accum)
            (record field_names) labels
        in
        let term =
          List.fold_left
            (fun expr t -> [%expr Cmdliner.Term.app [%e expr] [%e t] ])
            [%expr Cmdliner.Term.const [% e make_type]]
            (List.rev sub_terms)
        in
        [%expr fun () -> [%e term]]
      | Ptype_abstract, None ->
        raise_errorf ~loc "%s cannot be derived for fully abstract types" deriver
      | _, _ -> raise_errorf ~loc "Can't handle this type yet." deriver
    in
    let ty = ser_type_of_decl ~options ~path type_decl in
    let fv = Ppx_deriving.free_vars_in_core_type ty in
    let poly_type = Typ.force_poly @@ Typ.poly fv @@ ty in
    let var = pvar (Ppx_deriving.mangle_type_decl (`Suffix "cmdliner_term") type_decl) in
    ([],
     [Vb.mk (Pat.constraint_ var poly_type)
        (* The let binding sometimes ends up with a useless `rec`: *)
        ~attrs:[Ppx_deriving.attr_warning [%expr "-39"]]
        (polymorphize [%expr ([%e wrap_runtime serializer])])])


let ser_str_of_type_ext ~options ~path ({ ptyext_path = { loc }} as type_ext) =
  ignore (parse_options options);
  let serializer =
    let pats =
      List.fold_right (fun { pext_name = { txt = name' }; pext_kind; pext_attributes } acc_cases ->
        match pext_kind with
        | Pext_rebind _ ->
          (* nothing to do, since the constructor must be handled in original
             constructor declaration *)
          acc_cases
        | Pext_decl (_ :: _, _, _) ->
          raise_errorf ~loc "%s: Explicit binders for type variables not supported" deriver
        | Pext_decl ([], pext_args, _) ->
          let json_name = attr_string "name" name' pext_attributes in
          let case =
            match pext_args with
            | Pcstr_tuple([]) ->
              Exp.case
                (pconstr name' [])
                [%expr `List [`String [%e str json_name]]]
            | Pcstr_tuple(args) ->
              let arg_exprs =
                List.mapi
                  (fun i typ ->
                     app (ser_expr_of_typ typ pext_attributes name') [evar (argn i)])
                  args
              in
              Exp.case
                (pconstr name' (List.mapi (fun i _ -> pvar (argn i)) args))
                [%expr `List ((`String [%e str json_name]) :: [%e list arg_exprs])]
            | Pcstr_record _ ->
              raise_errorf ~loc "%s: record variants are not supported" deriver
          in
          case :: acc_cases) type_ext.ptyext_constructors []
    in
    let fallback_case =
      Exp.case [%pat? x]
               [%expr [%e Ppx_deriving.poly_apply_of_type_ext type_ext [%expr fallback]] x]
    in
    Exp.function_ (pats @ [fallback_case])
  in
  let mod_name =
    let mod_lid =
      Ppx_deriving.mangle_lid
        (`PrefixSuffix ("M", "cmdliner_term")) type_ext.ptyext_path.txt
    in
    String.concat "." (Longident.flatten_exn mod_lid)
  in
  let polymorphize = Ppx_deriving.poly_fun_of_type_ext type_ext in
  let serializer = polymorphize (wrap_runtime serializer) in
  let flid = lid (Printf.sprintf "%s.f" mod_name) in
  let set_field = Exp.setfield (Exp.ident flid) flid serializer in
  let field = Exp.field (Exp.ident flid) (flid) in
  let body = [%expr let fallback = [%e field] in [%e set_field]] in
  [Str.value ?loc:None Nonrecursive [Vb.mk (Pat.construct (lid "()") None) body]]


let ser_sig_of_type ~options ~path type_decl =
  let loc = type_decl.ptype_loc in 
  let to_cmdliner =
    Sig.value (Val.mk (mknoloc (Ppx_deriving.mangle_type_decl (`Suffix "cmdliner_term") type_decl))
                      (ser_type_of_decl ~options ~path type_decl))
  in
  match type_decl.ptype_kind with
  | Ptype_open ->
    let mod_name = Ppx_deriving.mangle_type_decl
      (`PrefixSuffix ("M", "cmdliner_term")) type_decl
    in
    let poly_vars = List.rev
      (Ppx_deriving.fold_left_type_decl (fun acc name -> name :: acc) [] type_decl)
    in
    let typ = Ppx_deriving.core_type_of_type_decl type_decl in
    let polymorphize_ser  = Ppx_deriving.poly_arrow_of_type_decl
      (fun var -> [%type: unit -> [%t var] Cmdliner.Term.t]) type_decl
    in
    let ty = Typ.poly poly_vars (polymorphize_ser [%type: unit -> [%t typ] Cmdliner.Term.t]) in
    let typ = Type.mk ~kind:(Ptype_record
       [Type.field ~mut:Mutable (mknoloc "f") ty]) (mknoloc "t_cmdliner_term")
    in
    let record = Val.mk (mknoloc "f") (Typ.constr (lid "t_cmdliner_term") []) in
    let mod_ =
      Sig.module_ (Md.mk (mknoloc (Some mod_name))
                  (Mty.signature [
                     Sig.type_ Nonrecursive [typ];
        Sig.value record;
      ]))
    in
    [mod_; to_cmdliner]
  | _ -> [to_cmdliner]


let structure f ~options ~path type_ =
  let (pre, vals) = f ~options ~path type_ in
  match vals with
  | [] -> pre
  | _  -> pre @ [Str.value ?loc:None Recursive vals]

let on_str_decls f ~options ~path type_decls =
  let (pre, vals) = List.split (List.map (f ~options ~path) type_decls) in
  (List.concat pre, List.concat vals)

let on_sig_decls f ~options ~path type_decls =
  List.concat (List.map (f ~options ~path) type_decls)

let () =
  Ppx_deriving.(register
   (create deriver
    ~type_decl_str:(structure (on_str_decls ser_str_of_type))
    ~type_ext_str:ser_str_of_type_ext
    ~type_decl_sig:(on_sig_decls ser_sig_of_type)
    ~type_ext_sig:ser_sig_of_type_ext
    ()
  ));
