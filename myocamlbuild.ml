open Ocamlbuild_plugin

let () = dispatch (fun phase ->
  Ocamlbuild_cppo.dispatcher phase;
  match phase with
  | After_rules ->
    let ppx_deriving_component deriver =
      (Findlib.query "ppx_deriving").Findlib.location ^ "/" ^ deriver
    in
    flag ["ocaml"; "compile"; "use_cmdliner"] &
      S[A"-ppx"; A"ocamlfind ppx_import/ppx_import";
        A"-ppx"; A("ocamlfind ppx_deriving/ppx_deriving "^
                   "src/ppx_deriving_cmdliner.cma "^
                   (ppx_deriving_component "ppx_deriving_show.cma"));
        A"-I"; A(ppx_deriving_component "")];
    flag ["ocaml"; "link"; "use_cmdliner"; "byte"] &
      A(ppx_deriving_component "ppx_deriving_runtime.cma");
    flag ["ocaml"; "link"; "use_cmdliner"; "native"] &
      A(ppx_deriving_component "ppx_deriving_runtime.cmxa");
    ocaml_lib ~dir:"src" "src/ppx_deriving_cmdliner_runtime";

  | _ -> ())
