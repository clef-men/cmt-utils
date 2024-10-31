type arguments =
  { filename: string;
    attributes: bool;
    locations: bool;
    indices: bool;
  }

exception Error of unit Cmdliner.Term.ret

let error ?(usage = true) =
  Fmt.kstr (fun msg ->
    raise @@ Error (`Error (usage, msg))
  )

let main args =
  match Cmt_format.read_cmt args.filename with
  | exception Sys_error err ->
      error "invalid input file: %s" err
  | exception Cmt_format.Error _
  | exception Cmi_format.Error _ ->
      error "invalid input file"
  | cmt ->
      match cmt.cmt_annots with
      | Implementation str ->
          Load_path.(init ~auto_include:no_auto_include ~visible:cmt.cmt_loadpath.visible ~hidden:cmt.cmt_loadpath.hidden) ;
          let config : Dump.config =
            { config_attributes= args.attributes;
              config_locations= args.locations;
              config_indices= args.indices;
            }
          in
          Fmt.pr "%a@." (Dump.structure ~config) str
      | _ ->
          error "invalid input file: not an implementation"
