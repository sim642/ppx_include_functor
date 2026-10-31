open Ppxlib
open Ast_builder.Default


class include_functor_impl = object (self)
  inherit Ast_traverse.map_with_path as super

  val mutable fresh_name_count = 0

  method private fresh_name path =
    let path = String.map (function
        | 'A'..'Z'
        | 'a'..'z' as c -> c
        | _ -> '_'
      ) path
      |> String.capitalize_ascii
    in
    let name = "Include_functor_" ^ path ^ "_" ^ string_of_int fresh_name_count in
    fresh_name_count <- fresh_name_count + 1;
    name

  method! structure path str =
    List.fold_left (fun acc stri ->
        match stri with
        | [%stri [%%functor include [%m? me]]] ->
          let loc = stri.pstr_loc in
          (* TODO: this probably screws up intermixing of opens with other things *)
          let acc_opens, acc_non_opens = List.partition (function
              | [%stri open [%m? _]] -> true
              | _ -> false
            ) acc
          in
          let name = self#fresh_name path in
          let m = pmod_ident ~loc (Located.lident ~loc name) in
          (* TODO: use extended open to hide M from outside? *)
          let def = pstr_module ~loc {
            pmb_name = Located.mk ~loc (Some name);
            pmb_expr = pmod_structure ~loc (List.rev acc_non_opens);
            pmb_attributes = [];
            pmb_loc = loc}
          in
          [%str
            (* module M = [%m pmod_structure ~loc (List.rev acc)] *)
            [%%i def]
            include [%m m]
            include [%m me] ([%m m])
          ]
          |> List.rev_append acc_opens (* keep opens outside *)
          |> List.rev
        | _ -> super#structure_item path stri :: acc (* TODO: should use self? *)
      ) [] str
    |> List.rev
end

let impl str = (new include_functor_impl)#structure !Ocaml_common.Location.input_name str

let () =
  Driver.register_transformation ~impl "include_functor"
