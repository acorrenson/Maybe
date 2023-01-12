open Maybe

let usage_msg = "maybe -infer <file>"

let input_files = ref []

let do_infer = ref false

let anon_fun filename =
  input_files := filename::!input_files

let speclist =
   [("-infer", Arg.Set do_infer, "Perform inference")]

let () =
  Arg.parse speclist anon_fun usage_msg;
  match !input_files with
  | [file] ->
    let prog = Parser.parse_file file in
    if !do_infer then
      Lang.infer prog
      |> Printf.printf "\nsupport:\n%a\n" Lang.print_support
    else
      Lang.exec (fun _ -> 0) prog
      |> Printf.printf "\nresult: %d\n"
  | _ ->
    Printf.eprintf "\t%s\n" usage_msg;
    exit 1
