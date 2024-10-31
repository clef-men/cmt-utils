include module type of struct
  include Ocaml_common.Env
end

type ident_kind =
  | IdentValue
  | IdentType
  | IdentModule
  | IdentModtype
  | IdentClass
  | IdentCltype

val find_index :
  ident_kind -> Ident.t -> t -> int option
