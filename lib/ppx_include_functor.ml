open Ppxlib
open Ast_builder.Default


class include_functor_impl = object (self)
  inherit Ast_traverse.map as super

  val mutable fresh_name_count = 0

  method private fresh_name =
    let name = "Include_functor_" ^ string_of_int fresh_name_count in
    fresh_name_count <- fresh_name_count + 1;
    name

  method! structure str =
    List.fold_left (fun acc stri ->
        match stri with
        | [%stri [%%functor include [%m? me]]] ->
          let loc = stri.pstr_loc in
          let name = self#fresh_name in
          let m = pmod_ident ~loc (Located.lident ~loc name) in
          (* TODO: use extended open to hide M from outside? *)
          let def = pstr_module ~loc {
            pmb_name = Located.mk ~loc (Some name);
            pmb_expr = pmod_structure ~loc (List.rev acc);
            pmb_attributes = [];
            pmb_loc = loc}
          in
          [%str
            (* module M = [%m pmod_structure ~loc (List.rev acc)] *)
            [%%i def]
            include [%m m]
            include [%m me] ([%m m])
          ]
          |> List.rev
        | _ -> super#structure_item stri :: acc (* TODO: should use self? *)
      ) [] str
    |> List.rev
end

let impl = (new include_functor_impl)#structure

let () =
  Driver.register_transformation ~impl "include_functor"
