include Ocaml_common.Env

type ident_kind =
  | IdentValue
  | IdentType
  | IdentModule
  | IdentModtype
  | IdentClass
  | IdentCltype

let find_index kind =
  match kind with
  | IdentValue ->
      find_value_index
  | IdentType ->
      find_type_index
  | IdentModule ->
      find_module_index
  | IdentModtype ->
      find_modtype_index
  | IdentClass ->
      find_class_index
  | IdentCltype ->
      find_cltype_index
