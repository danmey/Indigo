open Ocamlbuild_plugin;;
open Command;;

dispatch begin function
| After_rules ->
  flag ["ocaml"; "native"; "link"; "program"] & A"config_file.cmx";
| _ -> ()
end;;
