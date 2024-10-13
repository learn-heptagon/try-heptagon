open Obc_interp

module type Interpreter = sig
  val reset : unit -> unit
  val step : value option list -> value option list
end

module type Simulator = sig
  val start : string -> unit
  val stop : string -> unit -> unit
end

open Js_of_ocaml
open Page

(* TODO truth table *)

module FilterSimul(I : Interpreter) : Simulator = struct
  let start divid =
    let chartdivid = divid^"-chart" in
    ignore ((by_id divid)##appendChild (of_node T.(canvas ~a:[a_id chartdivid] [])));

    (* Run node *)
    let nbIter = 50 in
    I.reset ();
    let vs = Array.make nbIter (0, 0.0, 0.0) in
    for i = 0 to nbIter - 1 do
      let res = I.step [] in
      match res with
      | [Some (Vint n); Some (Vfloat src); Some (Vfloat filt)] ->
         print_endline (string_of_float src);
         vs.(i) <- (n, src, filt)
      | _ -> ()
    done;

    (* Create chart *)
    let source = Chartjs.empty_line_dataset () in
    source##.data := Js.array (Array.map (fun (_, f, _) -> f) vs);
    source##.label := Js.string "Source";
    source##.borderColor := Chartjs.Color.of_string "red";
    source##.backgroundColor := Chartjs.Color.of_string "rgba(255, 0, 0, 0)";

    let filtered = Chartjs.empty_line_dataset () in
    filtered##.data := Js.array (Array.map (fun (_, _, f) -> f) vs);
    filtered##.label := Js.string "Filtered";
    filtered##.borderColor := Chartjs.Color.of_string "blue";
    filtered##.backgroundColor := Chartjs.Color.of_string "rgba(0, 0, 255, 0)";

    let data = Chartjs.empty_data () in
    data##.datasets := Js.array [|source; filtered|];
    data##.labels := Js.array (Array.map (fun (n, _, _) -> Js.string (string_of_int n)) vs);

    let axis = Chartjs.empty_category_axis () in
    let scales = Chartjs.empty_line_scales () in
    axis##.display := Chartjs.Axis_display.auto;
    scales##.xAxes := Js.array [|axis|];
    let options = Chartjs.empty_line_options () in
    options##.scales := scales;
    let chart = Chartjs.chart_from_id Chartjs.Chart.line data options chartdivid in
    Js.Unsafe.global##.chart := chart

  let stop divid () = () (* TODO *)
end
