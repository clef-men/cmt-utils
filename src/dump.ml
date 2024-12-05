type config =
  { config_attributes: bool;
    config_locations: bool;
    config_indices: bool;
  }

type context =
  { context_attributes: bool;
    context_locations: bool;
    context_indices: bool;
    context_env: Env.t;
  }

type case_kind =
  | Result
  | Exception

module Symbol = struct
  let node =
    '+'
  let last_node =
    '`'
  let branch =
    '|'
  let last_branch =
    ' '
end

module Color = struct
  let ident =
    `Green
  let longident =
    `Green
  let path =
    `Green
  let pattern =
    `Yellow
  let expression =
    `Blue
  let module_expr =
    `Magenta
  let structure_item =
    `Red
end

module State = struct
  let state =
    Dynarray.create ()

  let begin_level () =
    Dynarray.add_last state false
  let begin_last_level () =
    Dynarray.add_last state true
  let end_level () =
    Dynarray.pop_last state |> ignore
end

type instruction =
  | Printable : 'a Fmt.t * 'a -> instruction
let printable pp x =
  Printable (pp, x)
let printables pp =
  List.map (printable pp)

let rec exec ppf instrs =
  Dynarray.iter (fun last ->
    Fmt.pf ppf "%c "
      Symbol.(if last then last_branch else branch)
  ) State.state ;
  match instrs with
  | [] ->
      ()
  | [Printable (pp, x)] ->
      Fmt.pf ppf "%c " Symbol.last_node ;
      State.begin_last_level () ;
      pp ppf x ;
      State.end_level () ;
  | Printable (pp, x) :: instrs ->
      Fmt.pf ppf "%c " Symbol.node ;
      State.begin_level () ;
      pp ppf x ;
      State.end_level () ;
      Fmt.cut ppf () ;
      exec ppf instrs
let exec' pp ppf xs =
  xs
  |> printables pp
  |> exec ppf
let subexec ppf = function
  | [] ->
      ()
  | instrs ->
      Fmt.cut ppf () ;
      exec ppf instrs
let subexec' pp ppf xs =
  xs
  |> printables pp
  |> subexec ppf

let location ~ctx ppf (loc : Location.t) =
  if ctx.context_locations then
    let start = loc.loc_start in
    let end_ = loc.loc_end in
    Fmt.pf ppf " <%i,%i-%i,%i>"
      start.pos_lnum
      (start.pos_cnum - start.pos_bol)
      end_.pos_lnum
      (end_.pos_cnum - end_.pos_bol)

let attribute ppf (attr : Parsetree.attribute) =
  Fmt.string ppf attr.attr_name.txt
let attributes ~ctx ppf attrs =
  if ctx.context_attributes && attrs <> [] then
    Fmt.pf ppf " [%a]"
      Fmt.(list ~sep:(const char ',') attribute) attrs

let ident ~ctx kind ppf id =
  Ident.print ppf id ;
  if ctx.context_indices then
    Fmt.(option @@ fun ppf -> pf ppf "#%i") ppf (Env.find_index kind id ctx.context_env)
let ident ~ctx kind =
  Fmt.styled (`Fg Color.ident) (ident ~ctx kind)

let longident =
  Fmt.styled (`Fg Color.longident) Pprintast.longident

let rec path ~ctx kind ppf (p : Path.t) =
  match p with
  | Pident id ->
      Fmt.pf ppf "%a"
        (ident ~ctx kind) id
  | Pdot (p, s)
  | Pextra_ty (p, Pcstr_ty s) ->
      Fmt.pf ppf "%a.%s"
        (path ~ctx kind) p
        s
  | Papply (p1, p2) ->
      Fmt.pf ppf "%a(%a)"
        (path ~ctx kind) p1
        (path ~ctx kind) p2
  | Pextra_ty (p, Pext_ty) ->
      Fmt.pf ppf "%a"
        (path ~ctx kind) p
let path ~ctx kind =
  Fmt.styled (`Fg Color.path) (path ~ctx kind)

let constant ppf (const : Asttypes.constant) =
  match const with
  | Const_int n ->
      Fmt.int ppf n
  | Const_char chr ->
      Fmt.pf ppf "'%c'" chr
  | Const_string (str, _, _) ->
      Fmt.pf ppf {|"%s"|} str
  | Const_float str ->
      Fmt.string ppf str
  | Const_int32 n ->
      Fmt.int32 ppf n
  | Const_int64 n ->
      Fmt.int64 ppf n
  | Const_nativeint n ->
      Fmt.nativeint ppf n

let rec_flag ppf (flag : Asttypes.rec_flag) =
  Fmt.string ppf
    begin match flag with
    | Recursive ->
        "rec"
    | Nonrecursive ->
        "nonrec"
    end

let mutable_flag ppf (flag : Asttypes.mutable_flag) =
  Fmt.string ppf
    begin match flag with
    | Mutable ->
        "mutable"
    | Immutable ->
        "immutable"
    end

let atomic_flag ppf (flag : Asttypes.atomic_flag) =
  Fmt.string ppf
    begin match flag with
    | Atomic ->
        "atomic"
    | Nonatomic ->
        "nonatomic"
    end

let closed_flag ppf (flag : Asttypes.closed_flag) =
  Fmt.string ppf
    begin match flag with
    | Closed ->
        "closed"
    | Open ->
        "open"
    end

let direction_flag ppf (flag : Asttypes.direction_flag) =
  Fmt.string ppf
    begin match flag with
    | Upto ->
        "->"
    | Downto ->
        "<-"
    end

let private_flag ppf (flag : Asttypes.private_flag) =
  Fmt.string ppf
    begin match flag with
    | Private ->
        "private"
    | Public ->
        "public"
    end

let override_flag ppf (flag : Asttypes.override_flag) =
  Fmt.string ppf
    begin match flag with
    | Override ->
        "override"
    | Fresh ->
        "fresh"
    end

let arg_label ?(space = false) ppf (lbl : Asttypes.arg_label) =
  match lbl with
  | Nolabel ->
      ()
  | Labelled lbl ->
      Fmt.pf ppf "%s~%s"
        (if space then " " else "")
        lbl
  | Optional lbl ->
      Fmt.pf ppf "%s?%s"
        (if space then " " else "")
        lbl

let partial ppf (p : Typedtree.partial) =
  Fmt.string ppf
    begin match p with
    | Partial ->
        "partial"
    | Total ->
        "total"
    end

let case_kind ppf kind =
  Fmt.string ppf
    begin match kind with
    | Result ->
        "result"
    | Exception ->
        "exception"
    end

let pattern_kind (type k) (pat : k Typedtree.general_pattern) =
  match pat.pat_desc with
  | Tpat_any ->
      "Tpat_any"
  | Tpat_var _ ->
      "Tpat_var"
  | Tpat_alias _ ->
      "Tpat_alias"
  | Tpat_constant _ ->
      "Tpat_constant"
  | Tpat_tuple _ ->
      "Tpat_tuple"
  | Tpat_construct _ ->
      "Tpat_construct"
  | Tpat_variant _ ->
      "Tpat_variant"
  | Tpat_record _ ->
      "Tpat_record"
  | Tpat_array _ ->
      "Tpat_array"
  | Tpat_lazy _ ->
      "Tpat_lazy"
  | Tpat_value _ ->
      "Tpat_value"
  | Tpat_exception _ ->
      "Tpat_exception"
  | Tpat_or _ ->
      "Tpat_or"
let rec pattern' : type k. ctx:_ -> _ -> k Typedtree.general_pattern -> _ = fun ~ctx ppf pat ->
  match pat.pat_desc with
  | Tpat_any ->
      []
  | Tpat_var (id, _, _) ->
      Fmt.pf ppf " id:%a"
        (ident ~ctx IdentValue) id ;
      []
  | Tpat_alias (pat, id, _, _) ->
      Fmt.pf ppf " id%a"
        (ident ~ctx IdentValue) id ;
      printables (pattern ~ctx) [pat]
  | Tpat_constant const ->
      Fmt.pf ppf " %a"
        constant const ;
      []
  | Tpat_tuple pats ->
      printables (pattern ~ctx) pats
  | Tpat_construct (lid, _, pats, _) ->
      Fmt.pf ppf " lid:%a"
        longident lid.txt ;
      printables (pattern ~ctx) pats
  | Tpat_variant (lbl, pat, _) ->
      Fmt.pf ppf " %s"
        lbl ;
      printables (pattern ~ctx) (Option.to_list pat)
  | Tpat_record (rcd, closed) ->
      Fmt.pf ppf " %a"
        closed_flag closed ;
      let field ~ctx ppf (lid, _, pat) =
        Fmt.pf ppf "Tpat_record-field lid:%a%a"
          longident lid.Location.txt
          (subexec' (pattern ~ctx)) [pat]
      in
      printables (field ~ctx) rcd
  | Tpat_array pats ->
      printables (pattern ~ctx) pats
  | Tpat_lazy pat ->
      printables (pattern ~ctx) [pat]
  | Tpat_value pat ->
      printables (pattern ~ctx) [(pat :> Typedtree.(value general_pattern))]
  | Tpat_exception pat ->
      printables (pattern ~ctx) [pat]
  | Tpat_or (pat1, pat2, _) ->
      printables (pattern ~ctx) [pat1; pat2]
and pattern : type k. ctx:_ -> _ -> k Typedtree.general_pattern -> _ = fun ~ctx ppf pat ->
  Fmt.pf ppf "%a"
    Fmt.(styled (`Fg Color.pattern) string) (pattern_kind pat) ;
  let subs = pattern' ~ctx ppf pat in
  Fmt.pf ppf "%a%a"
    (attributes ~ctx) pat.pat_attributes
    (location ~ctx) pat.pat_loc ;
  subexec ppf subs

let expression_kind (expr : Typedtree.expression) =
  match expr.exp_desc with
  | Texp_ident _ ->
      "Texp_ident"
  | Texp_constant _ ->
      "Texp_constant"
  | Texp_let _ ->
      "Texp_let"
  | Texp_function _ ->
      "Texp_function"
  | Texp_apply _ ->
      "Texp_apply"
  | Texp_match _ ->
      "Texp_match"
  | Texp_try _ ->
      "Texp_try"
  | Texp_tuple _ ->
      "Texp_tuple"
  | Texp_construct _ ->
      "Texp_construct"
  | Texp_variant _ ->
      "Texp_variant"
  | Texp_record _ ->
      "Texp_record"
  | Texp_atomic_loc _ ->
      "Texp_atomic_loc"
  | Texp_field _ ->
      "Texp_field"
  | Texp_setfield _ ->
      "Texp_setfield"
  | Texp_array _ ->
      "Texp_array"
  | Texp_ifthenelse _ ->
      "Texp_ifthenelse"
  | Texp_sequence _ ->
      "Texp_sequence"
  | Texp_while _ ->
      "Texp_while"
  | Texp_for _ ->
      "Texp_for"
  | Texp_send _ ->
      "Texp_send"
  | Texp_new _ ->
      "Texp_new"
  | Texp_instvar _ ->
      "Texp_instvar"
  | Texp_setinstvar _ ->
      "Texp_setinstvar"
  | Texp_override _ ->
      "Texp_override"
  | Texp_letmodule _ ->
      "Texp_letmodule"
  | Texp_letexception _ ->
      "Texp_letexception"
  | Texp_assert _ ->
      "Texp_assert"
  | Texp_lazy _ ->
      "Texp_lazy"
  | Texp_object _ ->
      "Texp_object"
  | Texp_pack _ ->
      "Texp_pack"
  | Texp_letop _ ->
      "Texp_letop"
  | Texp_unreachable ->
      "Texp_unreachable"
  | Texp_extension_constructor _ ->
      "Texp_extension_constructor"
  | Texp_open _ ->
      "Texp_open"
let rec expression' ~ctx ppf (expr : Typedtree.expression) =
  match expr.exp_desc with
  | Texp_ident (p, lid, _) ->
      Fmt.pf ppf " path:%a lid:%a"
        (path ~ctx IdentValue) p
        longident lid.txt ;
      []
  | Texp_constant const ->
      Fmt.pf ppf " %a"
        constant const ;
      []
  | Texp_let (rec_, bdgs, expr') ->
      Fmt.pf ppf " %a"
        rec_flag rec_ ;
      printables (value_binding ~ctx) bdgs @
      printables (expression ~ctx) [expr']
  | Texp_function (params, body) ->
      printables (function_param ~ctx) params @
      printables (function_body ~ctx) [body]
  | Texp_apply (expr', args) ->
      let argument ~ctx ppf (lbl, expr) =
        Fmt.pf ppf "Texp_apply-argument%a%a"
          (arg_label ~space:true) lbl
          (subexec' (expression ~ctx)) (Option.to_list expr)
      in
      printables (expression ~ctx) [expr'] @
      printables (argument ~ctx) args
  | Texp_match (expr', cases1, cases2, p) ->
      Fmt.pf ppf " %a"
        partial p ;
      printables (expression ~ctx) [expr'] @
      printables (case ~ctx Result) cases1 @
      printables (case ~ctx Exception) cases2
  | Texp_try (expr', cases1, cases2) ->
      printables (expression ~ctx) [expr'] @
      printables (case ~ctx Result) cases1 @
      printables (case ~ctx Exception) cases2
  | Texp_tuple exprs ->
      printables (expression ~ctx) exprs
  | Texp_construct (lid, _, exprs) ->
      Fmt.pf ppf " lid:%a"
        longident lid.txt ;
      printables (expression ~ctx) exprs
  | Texp_variant (lbl, expr') ->
      Fmt.pf ppf " %s"
        lbl ;
      printables (expression ~ctx) (Option.to_list expr')
  | Texp_record rcd ->
      let field ~ctx ppf (_, (lbl : Typedtree.record_label_definition)) =
        match lbl with
        | Kept _ ->
            ()
        | Overridden (lid, expr) ->
            Fmt.pf ppf "Texp_record-field lid:%a%a"
              longident lid.txt
              (subexec' (expression ~ctx)) [expr]
      in
      printables (field ~ctx) (Array.to_list rcd.fields) @
      printables (expression ~ctx) (Option.to_list rcd.extended_expression)
  | Texp_atomic_loc (expr', lid, _) ->
      Fmt.pf ppf " lid:%a"
        longident lid.txt ;
      printables (expression ~ctx) [expr']
  | Texp_field (expr', lid, _) ->
      Fmt.pf ppf " lid:%a"
        longident lid.txt ;
      printables (expression ~ctx) [expr']
  | Texp_setfield (expr1, lid, _, expr2) ->
      Fmt.pf ppf " lid:%a"
        longident lid.txt ;
      printables (expression ~ctx) [expr1; expr2]
  | Texp_array exprs ->
      printables (expression ~ctx) exprs
  | Texp_ifthenelse (expr0, expr1, expr2) ->
      printables (expression ~ctx) ([expr0; expr1] @ Option.to_list expr2)
  | Texp_sequence (expr1, expr2) ->
      printables (expression ~ctx) [expr1; expr2]
  | Texp_while (expr1, expr2) ->
      printables (expression ~ctx) [expr1; expr2]
  | Texp_for (id, _pat, expr1, expr2, dir, expr3) ->
      Fmt.pf ppf " %a id:%a"
        direction_flag dir
        (ident ~ctx IdentValue) id ;
      printables (expression ~ctx) [expr1; expr2; expr3]
  | Texp_assert (expr', _) ->
      printables (expression ~ctx) [expr']
  | Texp_lazy expr' ->
      printables (expression ~ctx) [expr']
  | Texp_unreachable ->
      []
  | Texp_extension_constructor (lid, p) ->
      Fmt.pf ppf " lid:%a path:%a"
        longident lid.txt
        (path ~ctx IdentValue) p ;
      []
  | Texp_send _
  | Texp_new _
  | Texp_instvar _
  | Texp_setinstvar _
  | Texp_override _
  | Texp_letmodule _
  | Texp_letexception _
  | Texp_object _
  | Texp_pack _
  | Texp_letop _
  | Texp_open _ ->
      (* TODO *)
      []
and expression ~ctx ppf expr =
  Fmt.pf ppf "%a"
    Fmt.(styled (`Fg Color.expression) string) (expression_kind expr) ;
  let subs = expression' ~ctx ppf expr in
  Fmt.pf ppf "%a%a"
    (attributes ~ctx) expr.exp_attributes
    (location ~ctx) expr.exp_loc ;
  subexec ppf subs

and function_param ~ctx ppf (param : Typedtree.function_param) =
  Fmt.pf ppf "function_param%a id:%a%a"
    (arg_label ~space:true) param.fp_arg_label
    (ident ~ctx IdentValue) param.fp_param
    (location ~ctx) param.fp_loc ;
  match param.fp_kind with
  | Tparam_pat pat ->
      subexec' (pattern ~ctx) ppf [pat]
  | Tparam_optional_default (pat, expr) ->
      subexec ppf [printable (pattern ~ctx) pat; printable (expression ~ctx) expr]

and function_body ~ctx ppf (body : Typedtree.function_body) =
  match body with
  | Tfunction_body expr ->
      Fmt.pf ppf "Tfunction_body%a"
        (subexec' (expression ~ctx)) [expr]
  | Tfunction_cases body ->
      Fmt.pf ppf "Tfunction_cases %a id:%a%a%a%a"
        partial body.partial
        (ident ~ctx IdentValue) body.param
        (location ~ctx) body.loc
        (attributes ~ctx) body.attributes
        (subexec' (case ~ctx Result)) body.cases

and case : type k. ctx:_ -> _ -> _ -> k Typedtree.case -> _ = fun ~ctx kind ppf cas ->
  Fmt.pf ppf "%a-case%s%a%a"
    case_kind kind
    (if cas.c_guard = None then "" else " guarded")
    Fmt.(option @@ fun ppf -> pf ppf " id:%a" (ident ~ctx IdentValue)) cas.c_cont
    subexec (printable (pattern ~ctx) cas.c_lhs :: printables (expression ~ctx) (Option.to_list cas.c_guard @ [cas.c_rhs]))

and value_binding ~ctx ppf (bdg : Typedtree.value_binding) =
  Fmt.pf ppf "value_binding%a%a%a"
    (attributes ~ctx) bdg.vb_attributes
    (location ~ctx) bdg.vb_loc
    subexec [printable (pattern ~ctx) bdg.vb_pat; printable (expression ~ctx) bdg.vb_expr]

let constructor_declaration ~ctx ppf (constr : Typedtree.constructor_declaration) =
  Fmt.pf ppf "constructor id:%a %s%a%a"
    (ident ~ctx IdentValue) constr.cd_id
    ( match constr.cd_args with
      | Cstr_tuple _ -> "tuple"
      | Cstr_record _ -> "record"
    )
    (attributes ~ctx) constr.cd_attributes
    (location ~ctx) constr.cd_loc

let label_declaration ~ctx ppf (lbl : Typedtree.label_declaration) =
  Fmt.pf ppf "label %a %a id:%a%a%a"
    mutable_flag lbl.ld_mutable
    atomic_flag lbl.ld_atomic
    (ident ~ctx IdentValue) lbl.ld_id
    (attributes ~ctx) lbl.ld_attributes
    (location ~ctx) lbl.ld_loc

let type_declaration ~ctx ppf (ty : Typedtree.type_declaration) =
  Fmt.pf ppf "type_declaration %s %a id:%a%a%a"
    ( match ty.typ_kind with
      | Ttype_abstract -> "abstract"
      | Ttype_variant _ -> "variant"
      | Ttype_record _ -> "record"
      | Ttype_open -> "open"
    )
    private_flag ty.typ_private
    (ident ~ctx IdentType) ty.typ_id
    (attributes ~ctx) ty.typ_attributes
    (location ~ctx) ty.typ_loc ;
  match ty.typ_kind with
  | Ttype_variant constrs ->
      subexec' (constructor_declaration ~ctx)  ppf constrs
  | Ttype_record lbls ->
      subexec' (label_declaration ~ctx) ppf lbls
  | Ttype_abstract
  | Ttype_open ->
      ()

let rec module_expr_kind (mod_ : Typedtree.module_expr) =
  match mod_.mod_desc with
  | Tmod_ident _ ->
      "Tmod_ident"
  | Tmod_structure _ ->
      "Tmod_structure"
  | Tmod_functor _ ->
      "Tmod_structure"
  | Tmod_apply _ ->
      "Tmod_apply"
  | Tmod_apply_unit _ ->
      "Tmod_apply_unit"
  | Tmod_constraint _ ->
      "Tmod_constraint"
  | Tmod_unpack _ ->
      "Tmod_unpack"
and module_expr' ~ctx ppf (mod_ : Typedtree.module_expr) =
  match mod_.mod_desc with
  | Tmod_ident (p, lid) ->
      Fmt.pf ppf " path:%a id:%a"
        (path ~ctx IdentModule) p
        longident lid.txt ;
      []
  | Tmod_structure str ->
      let env = Envaux.env_of_only_summary str.str_final_env in
      let ctx = { ctx with context_env= env } in
      printables (structure_item ~ctx) str.str_items
  | Tmod_constraint (mod_, _, _, _) ->
      printables (module_expr ~ctx) [mod_]
  | Tmod_functor _
  | Tmod_apply _
  | Tmod_apply_unit _
  | Tmod_unpack _ ->
      (* TODO *)
      []
and module_expr ~ctx ppf mod_ =
  Fmt.pf ppf "%a"
    Fmt.(styled (`Fg Color.module_expr) string) (module_expr_kind mod_) ;
  let subs = module_expr' ~ctx ppf mod_ in
  Fmt.pf ppf "%a%a"
    (attributes ~ctx) mod_.mod_attributes
    (location ~ctx) mod_.mod_loc ;
  subexec ppf subs

and structure_item_kind (str_item : Typedtree.structure_item) =
  match str_item.str_desc with
  | Tstr_value _ ->
      "Tstr_value"
  | Tstr_type _ ->
      "Tstr_type"
  | Tstr_attribute _ ->
      "Tstr_attribute"
  | Tstr_eval _ ->
      "Tstr_eval"
  | Tstr_primitive _ ->
      "Tstr_primitive"
  | Tstr_typext _ ->
      "Tstr_typext"
  | Tstr_exception _ ->
      "Tstr_exception"
  | Tstr_module _ ->
      "Tstr_module"
  | Tstr_recmodule _ ->
      "Tstr_recmodule"
  | Tstr_modtype _ ->
      "Tstr_modtype"
  | Tstr_open _ ->
      "Tstr_open"
  | Tstr_class _ ->
      "Tstr_class"
  | Tstr_class_type _ ->
      "Tstr_class_type"
  | Tstr_include _ ->
      "Tstr_include"
and structure_item' ~ctx ppf (str_item : Typedtree.structure_item) =
  match str_item.str_desc with
  | Tstr_eval (expr, attrs) ->
      Fmt.pf ppf "%a"
        (attributes ~ctx) attrs ;
      printables (expression ~ctx) [expr]
  | Tstr_value (rec_, bdgs) ->
      Fmt.pf ppf " %a"
        rec_flag rec_ ;
      printables (value_binding ~ctx) bdgs
  | Tstr_type (rec_, tys) ->
      Fmt.pf ppf " %a"
        rec_flag rec_ ;
      printables (type_declaration ~ctx) tys
  | Tstr_module mod_ ->
      Fmt.pf ppf "%a%a"
        Fmt.(option @@ fun ppf -> pf ppf " id:%a" (ident ~ctx IdentModule)) mod_.mb_id
        (attributes ~ctx) mod_.mb_attributes ;
      printables (module_expr ~ctx) [mod_.mb_expr]
  | Tstr_open open_ ->
      Fmt.pf ppf " %a"
        override_flag open_.open_override ;
      printables (module_expr ~ctx) [open_.open_expr]
  | Tstr_attribute attr ->
      Fmt.pf ppf " %a"
        attribute attr ;
      []
  | Tstr_primitive _
  | Tstr_typext _
  | Tstr_exception _
  | Tstr_recmodule _
  | Tstr_modtype _
  | Tstr_class _
  | Tstr_class_type _
  | Tstr_include _ ->
      (* TODO *)
      []
and structure_item ~ctx ppf str_item =
  Fmt.pf ppf "%a"
    Fmt.(styled (`Fg Color.structure_item) string) (structure_item_kind str_item) ;
  let subs = structure_item' ~ctx ppf str_item in
  Fmt.pf ppf "%a"
    (location ~ctx) str_item.str_loc ;
  subexec ppf subs

let structure ~config ppf (str : Typedtree.structure) =
  let env = Envaux.env_of_only_summary str.str_final_env in
  let ctx =
    { context_attributes= config.config_attributes;
      context_locations= config.config_locations;
      context_indices= config.config_indices;
      context_env= env;
    }
  in
  let renderer = Fmt.style_renderer ppf in
  Fmt.set_style_renderer ppf `Ansi_tty ;
  Fmt.pf ppf "@[<v>%a@]"
    (exec' (structure_item ~ctx)) str.str_items ;
  Fmt.set_style_renderer ppf renderer
