open Ppxlib
open Ast_builder.Default

let fresh_name_count = ref 0
let fresh_name () =
  let name = "Include_functor_" ^ string_of_int !fresh_name_count in
  incr fresh_name_count;
  name

let dup x = (x, x)

let forward stri =
  let loc = stri.pstr_loc in
  match stri.pstr_desc with
  | Pstr_type (_, tds) ->
    [pstr_type ~loc Nonrecursive (
        List.map (fun td ->
            {td with ptype_manifest = Some (core_type_of_type_declaration td)}
          ) tds
      )]
    |> dup
  | Pstr_value (_, vbs) ->
    let vbs' = List.filter_map (fun vb ->
        match vb.pvb_pat.ppat_desc with
        | Ppat_var x ->
          Some {vb with pvb_expr = pexp_ident ~loc (Located.map_lident x)}
        | Ppat_construct ({txt = Lident "()"; _}, _) ->
          None
        | _ -> Location.raise_errorf ~loc "include_functor: unsupported pattern"
      ) vbs
    in
    begin match vbs' with
      | [] -> []
      | _ -> [pstr_value ~loc Nonrecursive vbs']
    end
    |> dup
  | Pstr_module mb ->
    [pstr_module ~loc {mb with pmb_expr = pmod_ident ~loc (Located.map (function Some s -> Lident s | None -> failwith "TODO") mb.pmb_name)}]
    |> dup
  | Pstr_modtype mtd ->
    [pstr_modtype ~loc {mtd with pmtd_type = Some (pmty_ident ~loc (Located.map_lident mtd.pmtd_name))}]
    |> dup
  (* | Pstr_open od -> [pstr_open ~loc {od with popen_override = Override}] (* TODO: drop? *) *)
  | Pstr_open _ -> []
    |> dup
  (* | Pstr_include id -> [pstr_include ~loc id] *)
  | Pstr_include id ->
    ([[%stri module M = [%m id.pincl_mod]]], [[%stri include M]])
  | _ -> [pstr_extension ~loc (Location.error_extensionf ~loc "include_functor: unsupported structure item") []]
    |> dup


class include_functor_impl = object (* (self) *)
  inherit Ast_traverse.map_with_path as super

  method! structure path str =
    List.fold_left (fun (acc, acc_forward) stri ->
        match stri with
        | [%stri [%%functor include [%m? me]]] ->
          let loc = stri.pstr_loc in
          let outer_with_opens =
            List.rev_map2 (fun stri (name, (stri_forward, _)) ->
                stri ::
                match stri_forward with
                | [] -> []
                | _ ->
                  [
                    let def = pstr_module ~loc {
                      pmb_name = Located.mk ~loc (Some name);
                      pmb_expr = pmod_structure ~loc stri_forward;
                      pmb_attributes = [];
                      pmb_loc = loc}
                    in
                    [%stri
                      open struct
                        [%%i def]
                      end
                    ]
                  ]
              ) acc acc_forward
            |> List.concat
            |> List.rev
          in
          (* let acc' = outer_with_opens in *)
          let forward =
            acc_forward
            |> List.rev_map (fun (name, (_, stri_forward)) ->
                match stri_forward with
                | [] -> []
                | _ ->
                  let m = pmod_ident ~loc (Located.lident ~loc name) in
                  [%stri open [%m m]] :: stri_forward
                  (* TODO: is this right and safer? *)
                  |> fun str -> [%str include [%m pmod_structure ~loc str]]
              )
            |> List.concat
            |> pmod_structure ~loc in
          let acc' = List.rev_append [%str
              include [%m me] ([%m forward])
            ] outer_with_opens
          in
          let acc_forward' = acc_forward in
          (acc', acc_forward')
        | _ ->
          let acc' = super#structure_item path stri :: acc (* TODO: should use self? *) in
          let name = fresh_name () in
          let acc_forward' = (name, forward stri) :: acc_forward in
          (acc', acc_forward')
      ) ([], []) str
    |> fst
    |> List.rev
end

let impl str = (new include_functor_impl)#structure !Ocaml_common.Location.input_name str

let () =
  Driver.register_transformation ~impl "include_functor"
