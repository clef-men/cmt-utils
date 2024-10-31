type config =
  { config_attributes: bool;
    config_locations: bool;
    config_indices: bool;
  }

val structure :
  config:config -> Typedtree.structure Fmt.t
