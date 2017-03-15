
let cmd_test_case ~term ~argv ~expected ~pprinter what =
  let info = Cmdliner.Term.info "cmd" in
  Alcotest.(check (of_pp pprinter)) what expected
    begin match Cmdliner.Term.eval ~argv (term, info) with
    | `Error msg -> assert false
    | `Ok actual -> actual
    | `Version -> assert false
    | `Help -> assert false
    end


type common_types = {
  a1: string;
  b1: int;
  c1: float;
  d1: string option;
  e1: string list;
  f1: int array;
  g1: int list;
  h1: bool;
  i1: bytes;
  j1: string list option;
  k1: int list option;
} [@@deriving cmdliner,show]
let simple () =
  let argv = [|
    "cmd";
    "--a1"; "apple";
    "--b1"; "123";
    "--c1"; "1.20";
    "--d1"; "yes";
    "--e1"; "apple,banana,pear";
    "--f1"; "1,2,3,4,5";
    "--g1"; "100,200,300";
    "--h1";
    "--i1"; "testing";
    "--k1"; "1,2,3,4,5"
  |] in
  let expected = {
    a1 = "apple";
    b1 = 123;
    c1 = 1.20;
    d1 = Some "yes";
    e1 = ["apple";"banana";"pear"];
    f1 = [|1;2;3;4;5|];
    g1 = [100;200;300];
    h1 = true;
    i1 = Bytes.of_string "testing";
    j1 = None;
    k1 = Some [1;2;3;4;5]
  } in
  cmd_test_case "expected simple types to match"
    ~term:(common_types_cmdliner_term ())
    ~argv ~expected ~pprinter:pp_common_types


type default_types = {
  a1: string; [@default "apple"]
  b1: int; [@default 10]
  c1: float; [@default 1.20]
  e1: string list; [@default []]
  f1: int array; [@default [|1;2;3|]]
  g1: int list; [@default [1;2;3]]
  h1: bool; [@default true]
} [@@deriving cmdliner,show]
let defaults () =
  let argv = [|
    "cmd";
  |] in
  let expected = {
    a1 = "apple";
    b1 = 10;
    c1 = 1.20;
    e1 = [];
    f1 = [|1;2;3|];
    g1 = [1;2;3];
    h1 = true
  } in
  cmd_test_case "expected defaults to work"
    ~term:(default_types_cmdliner_term ())
    ~argv ~expected ~pprinter:pp_default_types


type env_types = {
  a1: string; [@env "A_ONE_ENV"]
} [@@deriving cmdliner,show]
let env () =
  let argv = [|
    "cmd";
  |] in
  let expected = {
    a1 = "foobar";
  } in
  Unix.putenv "A_ONE_ENV" "foobar";
  cmd_test_case "expected env variables to work"
    ~term:(env_types_cmdliner_term ())
    ~argv ~expected ~pprinter:pp_env_types


type list_sep_types = {
  a1: int list; [@sep '@']
  b1: string array; [@sep '*']
} [@@deriving cmdliner,show]
let list_sep () =
  let argv = [|
    "cmd";
    "--a1";
    "1@9@3@5";
    "--b1";
    "foo*bar*baz";
  |] in
  let expected = {
    a1 = [1;9;3;5];
    b1 = [|"foo"; "bar"; "baz"|]
  } in
  cmd_test_case "expected custom list sep to work"
    ~term:(list_sep_types_cmdliner_term ())
    ~argv ~expected ~pprinter:pp_list_sep_types


type pos_types = {
  a1: string; [@pos 1]
  b1: int; [@pos 0]
} [@@deriving cmdliner,show]
let positional () =
  let argv = [|
    "cmd";
    "1";
    "second-pos";
  |] in
  let expected = {
    a1 = "second-pos";
    b1 = 1
  } in
  cmd_test_case "expected positional args to work"
    ~term:(pos_types_cmdliner_term ())
    ~argv ~expected ~pprinter:pp_pos_types


type enum_types = {
  a1: int list; [@enum ["one",1;"two",2;"three",3;"four",4]]
  b1: [ `A | `B | `C]; [@enum ["a",`A;"b",`B;"c",`C]]
} [@@deriving cmdliner,show]
let enums () =
  let argv = [|
    "cmd";
    "--a1"; "one,two";
    "--b1"; "b"
  |] in
  let expected = {
    a1 = [1;2];
    b1 = `B
  } in
  cmd_test_case "expected enum args to work"
    ~term:(enum_types_cmdliner_term ())
    ~argv ~expected ~pprinter:pp_enum_types


module M = struct
  type t = int * int [@@deriving show]
  let fst (f,_) = f
  let snd (_,s) = s
  let of_string s =
    try
      let sepi = String.index s '|' in
      let fst = String.sub s 0 sepi in
      let snd = String.sub s (sepi+1) ((String.length s)-sepi-1) in
      Result.Ok (int_of_string fst, int_of_string snd)
    with _ -> Result.Error (`Msg (Printf.sprintf "Couldn't parse `%s`" s))
  let to_string t =
    Printf.sprintf "%d|%d" (fst t) (snd t)
  let cmdliner_converter =
    of_string,
    (fun fmt t -> Format.fprintf fmt "%s" (to_string t))
end
type custom_types = {
  foo: M.t; [@conv M.cmdliner_converter]
  bar: M.t;
} [@@deriving cmdliner,show]
let customs () =
  let argv = [|
    "cmd";
    "--foo"; "11|200";
    "--bar"; "0|13";
  |] in
  let expected = {
    foo = (11,200);
    bar = (0,13)
  } in
  cmd_test_case "expected custom type converter to work"
    ~term:(custom_types_cmdliner_term ())
    ~argv ~expected ~pprinter:pp_custom_types


type opt_all_types = {
  foo: string list; [@opt_all]
} [@@deriving cmdliner,show]
let opt_all () =
  let argv = [|
    "cmd";
    "--foo"; "test";
    "--foo"; "foo"
  |] in
  let expected = {
    foo = ["test"; "foo"]
  } in
  cmd_test_case "expected opt_all list to work"
    ~term:(opt_all_types_cmdliner_term ())
    ~argv ~expected ~pprinter:pp_opt_all_types


type foo = {
  a1: string; b1: string;
} [@@deriving cmdliner,show]
type terms_types = {
  foo: foo; [@term foo_cmdliner_term ()]
} [@@deriving cmdliner,show]
let terms () =
  let argv = [|
    "cmd";
    "--a1"; "apple";
    "--b1"; "pie"
  |] in
  let expected = {
    foo = {a1 = "apple"; b1 = "pie";}
  } in
  cmd_test_case "expected custom @term to work"
    ~term:(terms_types_cmdliner_term ())
    ~argv ~expected ~pprinter:pp_terms_types


type misc_types = {
  a1: string; [@name "renamed"]
  b1: bool; [@enum ["true", true; "false", false]]
  c1: bool; [@enum ["true", true; "false", false]] [@default true]
  d1: bool; [@enum ["true", true; "false", false]] [@default true]
} [@@deriving cmdliner,show]
let miscs () =
  let argv = [|
    "cmd";
    "--renamed"; "test";
    "--b1"; "true"; "--d1"; "false"
  |] in
  let expected = {
    a1 = "test";
    b1 = true;
    c1 = true;
    d1 = false;
  } in
  cmd_test_case "expected `@name` & enum bools to work"
    ~term:(misc_types_cmdliner_term ())
    ~argv ~expected ~pprinter:pp_misc_types


let test_set = [
  "simple types" , `Quick, simple;
  "default types" , `Quick, defaults;
  "ENV types" , `Quick, env;
  "list sep types" , `Quick, list_sep;
  "positional types" , `Quick, positional;
  "enum types" , `Quick, enums;
  "custom types", `Quick, customs;
  "opt_all type", `Quick, opt_all;
  "term type", `Quick, terms;
  "misc types", `Quick, miscs;
]

let () =
  Alcotest.run "Ppx_deriving_cmdliner" [
    "****", test_set;
  ]
