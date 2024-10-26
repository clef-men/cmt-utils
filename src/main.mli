type arguments =
  { filename: string;
    attributes: bool;
    locations: bool;
  }

exception Error of unit Cmdliner.Term.ret

val main :
  arguments -> unit
