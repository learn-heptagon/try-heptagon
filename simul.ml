open Obc_interp

module type Interpreter = sig
  val reset : unit -> unit
  val step : value option list -> value option list
end

module type Simulator = sig
  val init : string -> (unit -> unit)
end

open Js_of_ocaml
open Page

(* TODO truth table *)

module FilterSimul(I : Interpreter) : Simulator = struct
  let init divid =
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
    Js.Unsafe.global##.chart := chart;

    fun () -> ()
end

open Js_of_ocaml
open Lwt.Syntax
open Js_of_ocaml_lwt
open Graphics_js

module StopwatchSimul(I : Interpreter) : Simulator = struct
  exception Stop

  let chronoyellow = rgb 255 127 0
  let black = rgb 0 0 0
  let energized = rgb 255 0 0

  let startstop = ref false and draw_startstop = ref false
  let reset = ref false and draw_reset = ref false
  let freeze = ref false and draw_freeze = ref false

  let svalue_of_bool b = Some (Vbool b)

  let bool_of_svalue s =
    match s with
    | Some (Vbool b) -> b
    | _ -> invalid_arg "bool_of_svalue"

  let chrono_x = 150 and chrono_y = 250 and chrono_r = 150
  let chrono_off = int_of_float((float_of_int chrono_r) /. Float.sqrt(2.))
  let stst_x = chrono_x - chrono_off and stst_y = chrono_y + chrono_off and stst_r = 20
  let res_x = chrono_x + chrono_off and res_y = chrono_y + chrono_off and res_r = 20
  let freeze_x = chrono_x - chrono_off and freeze_y = chrono_y - chrono_off and freeze_r = 20

  let draw_chrono () =
    let off = chrono_off in
    if !draw_startstop then set_color energized else set_color black;
    fill_circle stst_x stst_y stst_r;
    if !draw_reset then set_color energized else set_color black;
    fill_circle res_x res_y res_r;
    if !draw_freeze then set_color energized else set_color black;
    fill_circle freeze_x freeze_y freeze_r;
    set_color chronoyellow;
    fill_circle chrono_x chrono_y chrono_r;
    set_color black;
    moveto (chrono_x - off + 10) (chrono_y + off - 10); draw_string "On/Off";
    moveto (chrono_x + off - 50) (chrono_y + off - 10); draw_string "Reset";
    moveto (chrono_x - off + 10) (chrono_y - off + 10); draw_string "Freeze";
    draw_startstop := false; draw_reset := false; draw_freeze := false

  let draw_dot x y =
    set_color energized;
    fill_poly [|(x,y+4);(x+4,y);(x,y-4);(x-4,y)|]

  let draw_vert_segment x y =
    set_color energized;
    fill_poly [|(x,y+20);(x+4,y+16);(x+4,y-16);(x,y-20);(x-4,y-16);(x-4,y+16)|]

  let draw_hori_segment x y =
    set_color energized;
    fill_poly [|(x-20,y);(x-16,y+4);(x+16,y+4);(x+20,y);(x+16,y-4);(x-16,y-4)|]

  let mk_segments i =
    match i with
    | 0 -> [|true;true;true;true;true;true;false|]
    | 1 -> [|false;true;true;false;false;false;false|]
    | 2 -> [|true;true;false;true;true;false;true|]
    | 3 -> [|true;true;true;true;false;false;true|]
    | 4 -> [|false;true;true;false;false;true;true|]
    | 5 -> [|true;false;true;true;false;true;true|]
    | 6 -> [|true;false;true;true;true;true;true|]
    | 7 -> [|true;true;true;false;false;false;false|]
    | 8 -> [|true;true;true;true;true;true;true|]
    | 9 -> [|true;true;true;true;false;true;true|]
    | _ -> invalid_arg "mk_segments"

  (* Segments are: *)
  (*   0   *)
  (*  5 1  *)
  (*   6   *)
  (*  4 2  *)
  (*   3   *)
  let draw_seven_segments x y segments =
    if(segments.(0)) then draw_hori_segment x (y + 44);
    if(segments.(1)) then draw_vert_segment (x + 22) (y + 22);
    if(segments.(2)) then draw_vert_segment (x + 22) (y - 22);
    if(segments.(3)) then draw_hori_segment x (y - 44);
    if(segments.(4)) then draw_vert_segment (x - 22) (y - 22);
    if(segments.(5)) then draw_vert_segment (x - 22) (y + 22);
    if(segments.(6)) then draw_hori_segment x y

  let draw_screen x y w h segments =
    set_color black;
    fill_rect x y w h;
    draw_dot (x+120) (y+70);
    draw_dot (x+120) (y+30);
    draw_seven_segments (x+28) (y+50) segments.(0);
    draw_seven_segments (x+84) (y+50) segments.(1);
    draw_seven_segments (x+156) (y+50) segments.(2);
    draw_seven_segments (x+212) (y+50) segments.(3)

  let is_in_circle cx cy r x y =
    let d = Float.sqrt (Float.of_int ((cx-x)*(cx-x) + (cy-y)*(cy-y))) in
    d < Float.of_int r

  let init divid =
    let canvas = Page.create_canvas divid (Page.width divid) 450 in
    open_canvas canvas;
    clear_graph ();

    let continue = ref true in

    I.reset ();

    let rec chrono_loop () : unit Lwt.t =
      let* _ = Lwt_js.sleep 0.01 in
      if !continue then (
        draw_chrono ();

        let time = I.step [svalue_of_bool !startstop; svalue_of_bool !reset; svalue_of_bool !freeze] in

        (match time with
         | [Some (Vint i)] ->
            draw_screen 30 170 240 100 [|(mk_segments ((i / 1000) mod 10));
                                         (mk_segments ((i / 100) mod 10));
                                         (mk_segments ((i / 10) mod 10));
                                         (mk_segments (i mod 10))|];
         | _ -> ());

        startstop := false;
        reset := false;
        freeze := false;

        (* let segments = Array.of_list (List.map bool_of_svalue segments) in *)
        chrono_loop ()
      ) else Lwt.return ()
    in Lwt.async (fun _ -> chrono_loop ());

    Graphics_js.loop [ Key_pressed ; Button_down ] (fun s ->
        if not !continue then raise Stop;
        if s.keypressed then (
          match s.key with
          | 's' -> (
              startstop := true;
              draw_startstop := true
            )
          | 'r' -> (
              reset := true;
              draw_reset := true
            )
          | 'f' -> (
            freeze := true;
            draw_freeze := true
          )
          | _ -> ()
        )
      else if s.button then (
        if (is_in_circle stst_x stst_y stst_r s.mouse_x s.mouse_y) then (
          startstop := true;
          draw_startstop := true
        ) else if (is_in_circle res_x res_y res_r s.mouse_x s.mouse_y) then (
          reset := true;
          draw_reset := true
        )  else if (is_in_circle freeze_x freeze_y freeze_r s.mouse_x s.mouse_y) then (
          freeze := true;
          draw_freeze := true
        )
      ));

    (fun _ -> continue := false)
end
