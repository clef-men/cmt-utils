open Cmdliner

let filename =
  let docv = "file" in
  let doc = "Input .cmt file." in
  Arg.(required & pos 0 (some file) None & info [] ~docv ~doc)

let attributes =
  let doc = "Print attributes." in
  Arg.(value & flag & info ["attributes"] ~doc)

let locations =
  let doc = "Print locations." in
  Arg.(value & flag & info ["locations"] ~doc)

let indices =
  let doc = "Print indices." in
  Arg.(value & flag & info ["indices"] ~doc)

let info =
  let doc = "Utilities for .cmt files" in
  Cmd.info "cmt-utils" ~doc

let main filename attributes locations indices =
  let args : Main.arguments =
    { filename;
      attributes;
      locations;
      indices;
    }
  in
  try `Ok (Main.main args)
  with Main.Error err -> err
let main =
  Term.(ret (const main $ filename $ attributes $ locations $ indices))
let () =
  Cmd.v info main
  |> Cmd.eval
  |> exit
