open Three_address_code
open Control_flow

let input_file = ref None
let optimize = ref false
let verbose = ref false
let dot_cfg = ref false
let dot_dom = ref false
let dot_ssa = ref false

let parse_args () =
  let prog = Sys.argv.(0) in
  let usage = Printf.sprintf "Usage: %s [OPTION] filename" prog in
  let options =
    ["-O",        Arg.Set optimize, "Enable optimization";
     "-v",        Arg.Set verbose,  "Print information during optimization";
     "--dot-cfg", Arg.Set dot_cfg,  "Print CFG of function as DOT graph (in .dot file)";
     "--dot-dom", Arg.Set dot_dom,  "Print dominator tree of function as DOT graph (in .dot file)";
     "--dot-ssa", Arg.Set dot_ssa,  "Print SSA graph of function as DOT graph (in .dot file)"]
  in
  Arg.parse options (fun filename -> input_file := Some filename) usage

let () =
  parse_args ();
  match !input_file with
  | Some filename ->
    let fundecl, stmts = Parse.parse_file filename in
    let bbs = Basic_block.create_basic_blocks stmts in
    let cfg = Cfg.construct bbs in
    let ssa = Ssa.construct cfg in
    let cfg = if !optimize then Optim.optimize cfg ssa ~dump:!verbose else cfg in
    if !dot_cfg then begin
      let output = Filename.(remove_extension (basename filename)) ^ "_cfg.dot" in
      Printf.eprintf "Writing %s\n%!" output;
      Cfg.output_dot cfg ~filename:output
    end;
    if !dot_dom then begin
      let open Dom in
      let _ = dominators cfg in
      let _ = immediate_dominators cfg in
      let output = Filename.(remove_extension (basename filename)) ^ "_dom.dot" in
      Printf.eprintf "Writing %s\n%!" output;
      Domtree.(output_dot (create cfg) ~filename:output)
    end;
    if !dot_ssa then begin
      let output = Filename.(remove_extension (basename filename)) ^ "_ssa.dot" in
      Printf.eprintf "Writing %s\n%!" output;
      Ssa.Graph.output_dot ssa ~filename:output
    end;
    Llvm.emit_function (Option.get fundecl) cfg
  | None -> ()
