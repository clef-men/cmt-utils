type config =
  { config_attributes: bool;
    config_locations: bool;
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

let location ~config ppf (loc : Location.t) =
  if config.config_locations then
    let start = loc.loc_start in
    let end_ = loc.loc_end in
    Fmt.pf ppf " <%i,%i-%i,%i>"
      start.pos_lnum
      (start.pos_cnum - start.pos_bol)
      end_.pos_lnum
      (end_.pos_cnum - end_.pos_bol)

let attribute ppf (attr : Parsetree.attribute) =
  Fmt.string ppf attr.attr_name.txt
let attributes ~config ppf attrs =
  if config.config_attributes && attrs <> [] then
    Fmt.pf ppf " [%a]"
      Fmt.(list ~sep:(const char ',') attribute) attrs

let ident =
  Fmt.styled (`Fg Color.ident) Ident.print

let longident =
  Fmt.styled (`Fg Color.longident) Pprintast.longident

let rec path ppf (p : Path.t) =
  match p with
  | Pident id ->
      ident ppf id
  | Pdot (p, s)
  | Pextra_ty (p, Pcstr_ty s) ->
      Fmt.pf ppf "%a.%s"
        path p
        s
  | Papply (p1, p2) ->
      Fmt.pf ppf "%a(%a)"
        path p1
        path p2
  | Pextra_ty (p, Pext_ty) ->
      path ppf p
let path =
  Fmt.styled (`Fg Color.path) path

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
let rec pattern' : type k. config:_ -> _ -> k Typedtree.general_pattern -> _ = fun ~config ppf pat ->
  match pat.pat_desc with
  | Tpat_any ->
      []
  | Tpat_var (id, _, _) ->
      Fmt.pf ppf " id:%a"
        ident id ;
      []
  | Tpat_alias (pat, id, _, _) ->
      Fmt.pf ppf " id%a"
        ident id ;
      printables (pattern ~config) [pat]
  | Tpat_constant const ->
      Fmt.pf ppf " %a"
        constant const ;
      []
  | Tpat_tuple pats ->
      printables (pattern ~config) pats
  | Tpat_construct (lid, _, pats, _) ->
      Fmt.pf ppf " lid:%a"
        longident lid.txt ;
      printables (pattern ~config) pats
  | Tpat_variant (lbl, pat, _) ->
      Fmt.pf ppf " %s"
        lbl ;
      printables (pattern ~config) (Option.to_list pat)
  | Tpat_record (rcd, closed) ->
      Fmt.pf ppf " %a"
        closed_flag closed ;
      let field ~config ppf (lid, _, pat) =
        Fmt.pf ppf "Tpat_record-field lid:%a%a"
          longident lid.Location.txt
          (subexec' (pattern ~config)) [pat]
      in
      printables (field ~config) rcd
  | Tpat_array pats ->
      printables (pattern ~config) pats
  | Tpat_lazy pat ->
      printables (pattern ~config) [pat]
  | Tpat_value pat ->
      printables (pattern ~config) [(pat :> Typedtree.(value general_pattern))]
  | Tpat_exception pat ->
      printables (pattern ~config) [pat]
  | Tpat_or (pat1, pat2, _) ->
      printables (pattern ~config) [pat1; pat2]
and pattern : type k. config:_ -> _ -> k Typedtree.general_pattern -> _ = fun ~config ppf pat ->
  Fmt.pf ppf "%a"
    Fmt.(styled (`Fg Color.pattern) string) (pattern_kind pat) ;
  let subs = pattern' ~config ppf pat in
  Fmt.pf ppf "%a%a"
    (attributes ~config) pat.pat_attributes
    (location ~config) pat.pat_loc ;
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
let rec expression' ~config ppf (expr : Typedtree.expression) =
  match expr.exp_desc with
  | Texp_ident (p, lid, _) ->
      Fmt.pf ppf " path:%a lid:%a"
        path p
        longident lid.txt ;
      []
  | Texp_constant const ->
      Fmt.pf ppf " %a"
        constant const ;
      []
  | Texp_let (rec_, bdgs, expr') ->
      Fmt.pf ppf " %a"
        rec_flag rec_ ;
      printables (value_binding ~config) bdgs @
      printables (expression ~config) [expr']
  | Texp_function (params, body) ->
      printables (function_param ~config) params @
      printables (function_body ~config) [body]
  | Texp_apply (expr', args) ->
      let argument ~config ppf (lbl, expr) =
        Fmt.pf ppf "Texp_apply-argument%a%a"
          (arg_label ~space:true) lbl
          (subexec' (expression ~config)) (Option.to_list expr)
      in
      printables (expression ~config) [expr'] @
      printables (argument ~config) args
  | Texp_match (expr', cases1, cases2, p) ->
      Fmt.pf ppf " %a"
        partial p ;
      printables (expression ~config) [expr'] @
      printables (case ~config Result) cases1 @
      printables (case ~config Exception) cases2
  | Texp_try (expr', cases1, cases2) ->
      printables (expression ~config) [expr'] @
      printables (case ~config Result) cases1 @
      printables (case ~config Exception) cases2
  | Texp_tuple exprs ->
      printables (expression ~config) exprs
  | Texp_construct (lid, _, exprs) ->
      Fmt.pf ppf " lid:%a"
        longident lid.txt ;
      printables (expression ~config) exprs
  | Texp_variant (lbl, expr') ->
      Fmt.pf ppf " %s"
        lbl ;
      printables (expression ~config) (Option.to_list expr')
  | Texp_record rcd ->
      let field ~config ppf (_, (lbl : Typedtree.record_label_definition)) =
        match lbl with
        | Kept _ ->
            ()
        | Overridden (lid, expr) ->
            Fmt.pf ppf "Texp_record-field lid:%a%a"
              longident lid.txt
              (subexec' (expression ~config)) [expr]
      in
      printables (field ~config) (Array.to_list rcd.fields) @
      printables (expression ~config) (Option.to_list rcd.extended_expression)
  | Texp_atomic_loc (expr', lid, _) ->
      Fmt.pf ppf " lid:%a"
        longident lid.txt ;
      printables (expression ~config) [expr']
  | Texp_field (expr', lid, _) ->
      Fmt.pf ppf " lid:%a"
        longident lid.txt ;
      printables (expression ~config) [expr']
  | Texp_setfield (expr1, lid, _, expr2) ->
      Fmt.pf ppf " lid:%a"
        longident lid.txt ;
      printables (expression ~config) [expr1; expr2]
  | Texp_array exprs ->
      printables (expression ~config) exprs
  | Texp_ifthenelse (expr0, expr1, expr2) ->
      printables (expression ~config) ([expr0; expr1] @ Option.to_list expr2)
  | Texp_sequence (expr1, expr2) ->
      printables (expression ~config) [expr1; expr2]
  | Texp_while (expr1, expr2) ->
      printables (expression ~config) [expr1; expr2]
  | Texp_for (id, _pat, expr1, expr2, dir, expr3) ->
      Fmt.pf ppf " %a id:%a"
        direction_flag dir
        ident id ;
      printables (expression ~config) [expr1; expr2; expr3]
  | Texp_assert (expr', _) ->
      printables (expression ~config) [expr']
  | Texp_lazy expr' ->
      printables (expression ~config) [expr']
  | Texp_unreachable ->
      []
  | Texp_extension_constructor (lid, p) ->
      Fmt.pf ppf " lid:%a path:%a"
        longident lid.txt
        path p ;
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
and expression ~config ppf expr =
  Fmt.pf ppf "%a"
    Fmt.(styled (`Fg Color.expression) string) (expression_kind expr) ;
  let subs = expression' ~config ppf expr in
  Fmt.pf ppf "%a%a"
    (attributes ~config) expr.exp_attributes
    (location ~config) expr.exp_loc ;
  subexec ppf subs

and function_param ~config ppf (param : Typedtree.function_param) =
  Fmt.pf ppf "function_param%a id:%a%a"
    (arg_label ~space:true) param.fp_arg_label
    ident param.fp_param
    (location ~config) param.fp_loc ;
  match param.fp_kind with
  | Tparam_pat pat ->
      subexec' (pattern ~config) ppf [pat]
  | Tparam_optional_default (pat, expr) ->
      subexec ppf [printable (pattern ~config) pat; printable (expression ~config) expr]

and function_body ~config ppf (body : Typedtree.function_body) =
  match body with
  | Tfunction_body expr ->
      Fmt.pf ppf "Tfunction_body%a"
        (subexec' (expression ~config)) [expr]
  | Tfunction_cases body ->
      Fmt.pf ppf "Tfunction_cases %a id:%a%a%a%a"
        partial body.partial
        ident body.param
        (location ~config) body.loc
        (attributes ~config) body.attributes
        (subexec' (case ~config Result)) body.cases

and case : type k. config:_ -> _ -> _ -> k Typedtree.case -> _ = fun ~config kind ppf cas ->
  Fmt.pf ppf "%a-case%s%a%a"
    case_kind kind
    (if cas.c_guard = None then "" else " guarded")
    Fmt.(option @@ fun ppf -> pf ppf " id:%a" ident) cas.c_cont
    subexec (printable (pattern ~config) cas.c_lhs :: printables (expression ~config) (Option.to_list cas.c_guard @ [cas.c_rhs]))

and value_binding ~config ppf (bdg : Typedtree.value_binding) =
  Fmt.pf ppf "value_binding%a%a%a"
    (attributes ~config) bdg.vb_attributes
    (location ~config) bdg.vb_loc
    subexec [printable (pattern ~config) bdg.vb_pat; printable (expression ~config) bdg.vb_expr]

let constructor_declaration ~config ppf (constr : Typedtree.constructor_declaration) =
  Fmt.pf ppf "constructor id:%a %s%a%a"
    ident constr.cd_id
    ( match constr.cd_args with
      | Cstr_tuple _ -> "tuple"
      | Cstr_record _ -> "record"
    )
    (attributes ~config) constr.cd_attributes
    (location ~config) constr.cd_loc

let label_declaration ~config ppf (lbl : Typedtree.label_declaration) =
  Fmt.pf ppf "label %a %a id:%a%a%a"
    mutable_flag lbl.ld_mutable
    atomic_flag lbl.ld_atomic
    ident lbl.ld_id
    (attributes ~config) lbl.ld_attributes
    (location ~config) lbl.ld_loc

let type_declaration ~config ppf (ty : Typedtree.type_declaration) =
  Fmt.pf ppf "type_declaration %s %a id:%a%a%a"
    ( match ty.typ_kind with
      | Ttype_abstract -> "abstract"
      | Ttype_variant _ -> "variant"
      | Ttype_record _ -> "record"
      | Ttype_open -> "open"
    )
    private_flag ty.typ_private
    ident ty.typ_id
    (attributes ~config) ty.typ_attributes
    (location ~config) ty.typ_loc ;
  match ty.typ_kind with
  | Ttype_variant constrs ->
      subexec' (constructor_declaration ~config)  ppf constrs
  | Ttype_record lbls ->
      subexec' (label_declaration ~config) ppf lbls
  | Ttype_abstract
  | Ttype_open ->
      ()

let structure_item_kind (str_item : Typedtree.structure_item) =
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
let structure_item ~config ppf (str_item : Typedtree.structure_item) =
  match str_item.str_desc with
  | Tstr_eval (expr, _) ->
      printables (expression ~config) [expr]
  | Tstr_value (rec_, bdgs) ->
      Fmt.pf ppf " %a"
        rec_flag rec_ ;
      printables (value_binding ~config) bdgs
  | Tstr_type (rec_, tys) ->
      Fmt.pf ppf " %a"
        rec_flag rec_ ;
      printables (type_declaration ~config) tys
  | Tstr_attribute attr ->
      Fmt.pf ppf " %a"
        attribute attr ;
      []
  | Tstr_primitive _
  | Tstr_typext _
  | Tstr_exception _
  | Tstr_module _
  | Tstr_recmodule _
  | Tstr_modtype _
  | Tstr_open _
  | Tstr_class _
  | Tstr_class_type _
  | Tstr_include _ ->
      (* TODO *)
      []
let structure_item ~config ppf str_item =
  Fmt.pf ppf "%a"
    Fmt.(styled (`Fg Color.structure_item) string) (structure_item_kind str_item) ;
  let subs = structure_item ~config ppf str_item in
  Fmt.pf ppf "%a%a"
    (attributes ~config) (match str_item.str_desc with Tstr_eval (_, attrs) -> attrs | _ -> [])
    (location ~config) str_item.str_loc ;
  subexec ppf subs

let structure ~config ppf (str : Typedtree.structure) =
  let renderer = Fmt.style_renderer ppf in
  Fmt.set_style_renderer ppf `Ansi_tty ;
  Fmt.pf ppf "@[<v>%a@]"
    (exec' (structure_item ~config)) str.str_items ;
  Fmt.set_style_renderer ppf renderer
