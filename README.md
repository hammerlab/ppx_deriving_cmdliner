# [@@deriving cmdliner]

_deriving Cmdliner_ is the easiest way to get a command line interface. 

It is also a [ppx_deriving](https://github.com/whitequark/ppx_deriving) plugin
that generates a [Cmdliner](https://github.com/dbuenzli/cmdliner) `Term` for a
given type.

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
} [@@deriving cmdliner,show]

let _ =
  let term = Cmdliner.Term.(const show_params $ params_cmdliner_term ()) in
  let info = Cmdliner.Term.info Sys.argv.(0) in
  Cmdliner.Term.eval (term, info)
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

## Attributes supported

1. Docs: `[@doc "Overwrites the docstring"]`, `[@docs "SECTION TWO"]`, `[@docv "VAL"]`
2. Environment variables: `[@env "ENVNAME"]`, `[@env.doc "Docs for the variable"]`, `[@env.docs "SECTION ENVS"]`
3. Other: `[@list_sep '@']`, `[@default 123]`, `[@enum [("a", Foo); ("b", Bar)]]`, `[@aka ["b";"another-flag-name"]]`
