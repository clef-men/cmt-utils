type config =
  { config_attributes: bool;
    config_locations: bool;
  }

val structure :
  config:config -> Typedtree.structure Fmt.t
