type arguments =
  { filename: string;
    attributes: bool;
    locations: bool;
    indices: bool;
  }

exception Error of unit Cmdliner.Term.ret

val main :
  arguments -> unit
