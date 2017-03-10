module P = struct
  type t = {
    name: string; [@docs "SECTION"] [@docv "NAME"]
      (** Name is basldaskdj *)

      color: bytes; [@env "COLOR"] [@env.doc "testing"] [@env.docs "SECTION"]
      [@aka ["c";"more"]]
      (** Color something  *)

      how_much: int; [@default 9]
      (** How much, in dollars?  *)

      callsign: string;
      [@enum [("a", "apple"); ("b","bravo"); ("c","cookie")]] [@default "cookie"]
      (** The callsign used for this. *)

      animals: string list; [@enum [("giraffe", "giraffe"); ("bull", "bull")]]
      (** Animals to take with you. You don't have many options. *)

      weird_list: string array; [@sep '@']

      optional_arg: string option;

    hey: bool; [@default true]
  } [@@deriving cmdliner, show]
end

let run p = print_string (P.show p); print_newline ()

let _ =
  let term = Cmdliner.Term.(const run $ P.cmdliner_term ()) in
  let info = Cmdliner.Term.info Sys.argv.(0) in
  Cmdliner.Term.eval (term, info)
