open Ppxlib
open Ast_builder.Default


class include_functor_impl = object (self)
  inherit Ast_traverse.map as super

  method! structure str =
    List.fold_left (fun acc stri ->
        match stri with
        | [%stri [%%functor include [%m? me]]] ->
          let loc = stri.pstr_loc in
          (* TODO: generate fresh module name instead of M *)
          (* TODO: use extended open to hide M from outside? *)
          [%str
            module M = [%m pmod_structure ~loc (List.rev acc)]
            include M
            include [%m me] (M)
          ]
          |> List.rev
        | _ -> super#structure_item stri :: acc
      ) [] str
    |> List.rev
end

let impl = (new include_functor_impl)#structure

let () =
  Driver.register_transformation ~impl "include_functor"
