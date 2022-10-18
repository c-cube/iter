module C = Configurator.V1

let shims_pre_408 =
  "\nopen! Bigarray\nlet bigarray_map_file = Bigarray.Array1.map_file\n"

let shims_post_408 =
  "\n\
   let bigarray_map_file fd ty lay b len =\n\
  \  Unix.map_file fd ty lay b [| len |] |> Bigarray.array1_of_genarray\n"

let () =
  C.main ~name:"mkshims" (fun c ->
      let version = C.ocaml_config_var_exn c "version" in
      let major, minor =
        Scanf.sscanf version "%u.%u" (fun maj min -> maj, min)
      in
      print_endline
        (if (major, minor) >= (4, 8) then
          shims_post_408
        else
          shims_pre_408))
