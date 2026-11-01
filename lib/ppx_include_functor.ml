open Ppxlib
open Ast_builder.Default

let fresh_name_count = ref 0
let fresh_name () =
  let name = "Include_functor_" ^ string_of_int !fresh_name_count in
  incr fresh_name_count;
  name

class ppat_vars = object
  inherit [label loc list] Ast_traverse.fold as super

  method! pattern pat acc =
    match pat.ppat_desc with
    | Ppat_var x ->
      x :: acc
    | _ -> super#pattern pat acc
end

let ppat_vars pat = (new ppat_vars)#pattern pat []

let forward stri =
  let loc = stri.pstr_loc in
  match stri.pstr_desc with
  | Pstr_type (_, tds) ->
    [pstr_type ~loc Nonrecursive (
        List.map (fun td ->
            {td with ptype_kind = Ptype_abstract; ptype_manifest = Some (core_type_of_type_declaration td); ptype_attributes = []}
          ) tds
      )]
  | Pstr_value (_, vbs) ->
    let vbs' = List.concat_map (fun vb ->
        ppat_vars vb.pvb_pat
        |> List.map (fun x ->
            {vb with pvb_expr = pexp_ident ~loc (Located.map_lident x)}
          )
      ) vbs
    in
    begin match vbs' with
      | [] -> []
      | _ -> [pstr_value ~loc Nonrecursive vbs']
    end
  | Pstr_module mb ->
    [pstr_module ~loc {mb with pmb_expr = pmod_ident ~loc (Located.map (function Some s -> Lident s | None -> failwith "TODO") mb.pmb_name)}]
  | Pstr_modtype mtd ->
    [pstr_modtype ~loc {mtd with pmtd_type = Some (pmty_ident ~loc (Located.map_lident mtd.pmtd_name))}]
  (* | Pstr_open od -> [pstr_open ~loc {od with popen_override = Override}] (* TODO: drop? *) *)
  | Pstr_open _ -> []
  (* | Pstr_include id -> [pstr_include ~loc id] *)
  | Pstr_include _ ->
    [stri]
  | Pstr_attribute _ -> [] (* e.g. [@@@ocaml.text "..."] *) (* TODO: fine to ignore? *)
  | Pstr_primitive _ -> [stri] (* TODO: fine to just duplicate? *)
  | _ -> [pstr_extension ~loc (Location.error_extensionf ~loc "include_functor: unsupported structure item") []]

type acc = {
  forwarded: structure;
  unforwarded: structure;
  forwards: structure;
}

class include_functor_impl = object (* (self) *)
  inherit Ast_traverse.map as super

  method! structure str =
    List.fold_left (fun (acc: acc) stri ->
        match stri with
        | [%stri [%%functor include [%m? me]]] ->
          let loc = stri.pstr_loc in
          let (forwarded', forwards') =
            List.fold_left (fun (forwarded, forwards) stri ->
                match forward stri with
                | [] -> (stri :: forwarded, forwards)
                | forward ->
                  let name = fresh_name () in
                  let forward_open =
                    let def = pstr_module ~loc {
                      pmb_name = Located.mk ~loc (Some name);
                      pmb_expr = pmod_structure ~loc forward;
                      pmb_attributes = [];
                      pmb_loc = loc}
                    in
                    [%stri
                      open struct
                        [%%i def]
                      end
                    ]
                  in
                  let forwarded' = forward_open :: stri :: forwarded in
                  let forward_include =
                    let m = pmod_ident ~loc (Located.lident ~loc name) in
                    [%stri include [%m m]]
                  in
                  let forwards' = forward_include :: forwards in
                  (forwarded', forwards')
              ) (acc.forwarded, acc.forwards) (List.rev acc.unforwarded)
          in
          let stri' =
            [%stri
              include [%m me] ([%m pmod_structure ~loc (List.rev forwards')])
            ]
          in
          let unforwarded' = [stri'] in
          {forwarded = forwarded'; unforwarded = unforwarded'; forwards = forwards'}
        | _ ->
          let stri' = super#structure_item stri in (* TODO: should use self? *)
          {acc with unforwarded = stri' :: acc.unforwarded}
      ) {forwarded = []; unforwarded = []; forwards = []} str
    |> fun {forwarded; unforwarded; _} -> List.rev_append forwarded (List.rev unforwarded)
end

let impl = (new include_functor_impl)#structure

let () =
  Driver.register_transformation ~impl "include_functor"
