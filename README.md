# [@@deriving cmdliner]

_deriving Cmdliner_ is the easiest way to get a command line interface.

It is also a [ppx_deriving](https://github.com/whitequark/ppx_deriving) plugin
that generates a [Cmdliner](https://github.com/dbuenzli/cmdliner) `Term` for a
given type.

![example workflow](https://github.com/hammerlab/ppx_deriving_cmdliner/actions/workflows/workflow.yml/badge.svg)

## Example


```ocaml
type params = {
  username: string;
  (** Your Github username *)

  api_key: string;
  (** Your Github API key *)

  command: string; [@pos 0] [@docv "CMD"]
  (** The Github API command to run *)

  dry_run: bool;
  (** Don't really run this command *)

  time_to_wait: float; [@default 0.]
  (** Just an example of another type *)
} [@@deriving cmdliner, show]

let main () =
  let f p = show_params p |> print_endline in
  let info = Cmdliner.Cmd.info Sys.argv.(0) in
  let term = Cmdliner.Term.(const f $ params_cmdliner_term ()) in
  let cmd = Cmdliner.Cmd.v info term in
  exit (Cmdliner.Cmd.eval cmd)

let () = main ()
```

Which gives you a CLI like the following:

```
NAME
       awesome-cli

SYNOPSIS
       awesome-cli [OPTION]... CMD

ARGUMENTS
       CMD (required)
            The Github API command to run

OPTIONS
       --api-key=STRING (required)
            Your Github API key

       --dry-run
            Don't really run this command

       --help[=FMT] (default=auto)
           Show this help in format FMT. The value FMT must be one of `auto',
           `pager', `groff' or `plain'. With `auto', the format is `pager` or
           `plain' whenever the TERM env var is `dumb' or undefined.

       --time-to-wait=FLOAT (absent=0.)
            Just an example of another type

       --username=STRING (required)
            Your Github username
```

## Features

### Custom type support

Ppx_deriving_cmdliner supports arbitrary types via a `cmdliner_converter`
interface. For example, the below two methods work for supporting `M.t` (from
`test/tests.ml`)

```ocaml
module M = struct
  type t = int * int
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
} [@@deriving cmdliner]
```

In short, a value of type ``string -> ('a, [ `Msg of string ]) Result.result) *
'a printer`` must be provided (or will be looked for under the name
`cmdliner_converter` if the type is `t`, else `type_name_cmdliner_converter`)
for the given type.

## Attributes supported

1. Docs: `[@doc "Overwrites the docstring"]`, `[@docs "SECTION TWO"]`, `[@docv "VAL"]`
2. Environment variables: `[@env "ENVNAME"]`, `[@env.doc "Docs for the variable"]`, `[@env.docs "SECTION ENVS"]`
3. Other: `[@list_sep '@']`, `[@default 123]`, `[@enum [("a", Foo); ("b", Bar)]]`, `[@aka ["b";"another-flag-name"]]`, `[@conv cmdliner_converter]` (cf. [required argument to `conv`](http://erratique.ch/software/cmdliner/doc/Cmdliner.Arg.html#VALconv) in Cmdliner), `[@opt_all]` only on `a' list` fields, `[@term cmdliner_term]` for assiging an arbitrary `Cmdliner.Term.t` to a field.
