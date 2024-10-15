open Obc_interp

module type Interpreter = sig
  val reset : unit -> unit
  val step : value list -> value list
end

module type Simulator = sig
  val init : string -> (unit -> unit)
end

open Js_of_ocaml
open Page

(* TODO truth table *)

let create_dataset data label color =
  let source = Chartjs.empty_line_dataset () in
  source##.data := Js.array data;
  source##.label := Js.string label;
  source##.borderColor := Chartjs.Color.of_string color;
  source##.backgroundColor := Chartjs.Color.of_string "rgba(0,0,0,0)";
  source

let create_options () =
  let axis = Chartjs.empty_category_axis () in
  let scales = Chartjs.empty_line_scales () in
  axis##.display := Chartjs.Axis_display.of_bool false;
  (* axis##.gridLines##.display := Js.bool false; *)
  scales##.xAxes := Js.array [|axis|];
  let options = Chartjs.empty_line_options () in
  options##.scales := scales;
  options


module FilterSimul(I : Interpreter) : Simulator = struct
  let init divid =
    let chartdivid = divid^"-chart" in
    ignore ((by_id divid)##appendChild (of_node T.(canvas ~a:[a_id chartdivid] [])));

    (* Run node *)
    let nbIter = 50 in
    I.reset ();
    let vs = Array.make nbIter (0, 0.0, 0.0) in
    print_endline "before loop";
    for i = 0 to nbIter - 1 do
      print_endline "before step";
      let res = I.step [] in
      match res with
      | [Vint n; Vfloat src; Vfloat filt] ->
         vs.(i) <- (n, src, filt)
      | _ -> ()
    done;

    (* Create chart *)
    let source = create_dataset (Array.map (fun (_, f, _) -> f) vs) "Source" "red" in
    let filtered = create_dataset (Array.map (fun (_, _, f) -> f) vs) "Filtered" "blue" in

    let data = Chartjs.empty_data () in
    data##.datasets := Js.array [|source; filtered|];
    data##.xLabels := Js.array (Array.map (fun (n, _, _) -> "") vs);

    let options = create_options () in

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

  let svalue_of_bool b = Vbool b

  let bool_of_svalue s =
    match s with
    | Vbool b -> b
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
         | [Vint i] ->
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
    in

    Lwt.async (fun _ -> chrono_loop ());

    (try
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
     with Stop -> ());

    (fun _ -> continue := false)
end

class type audioBuffer =
  object
    method getChannelData : int -> Typed_array.float32Array Js.meth
    method copyToChannel : Typed_array.float32Array -> int -> unit Js.meth
  end

class type ['a, 'b] promise =
  object
    method then_ : ('a -> 'b) Js.callback -> 'b Js.meth
  end

let opt_get o = Js.Opt.get o (fun _ -> failwith "get")
let optdef_get o = Js.Optdef.get o (fun _ -> failwith "get")

let tarray_get array i = optdef_get (Typed_array.get array i)

let prevInput : Dom_html.inputElement Js.t option ref = ref None
let prevLoaded = ref None

module AudioFilterSimul(I : Interpreter) = struct
  let init divid =

    (** File input *)

    let inputid = divid^"-fileinput" in

    let input =
      match !prevInput with
      | Some input -> input
      | None ->
         let input = of_node T.(input ~a:[a_id inputid; a_input_type `File] ()) in
         let input = Js.Unsafe.coerce input in
         prevInput := Some input; input
    in
    ignore ((by_id divid)##appendChild (Js.Unsafe.coerce input));

    (** Play buttons *)

    let playsrc = T.(of_node (button ~a:[a_disabled ()] [txt "Play source"]))
    and playfilt = T.(of_node (button ~a:[a_disabled ()] [txt "Play filtered"])) in
    Dom.appendChild (by_id divid) playsrc;
    Dom.appendChild (by_id divid) playfilt;

    (* (\** Chart *\) *)
    (* let chartdivid = divid^"-chart" in *)
    (* ignore ((by_id divid)##appendChild (of_node T.(canvas ~a:[a_id chartdivid] []))); *)

    (** Logic *)

    let fileHandler file =
      let (bufferprom : (_, unit) promise Js.t) = Js.Unsafe.fun_call (Js.Unsafe.js_expr "loadAudio") [|Js.Unsafe.inject file|] in
      let loadCallback = fun buffer ->

        let chan0 = buffer##getChannelData 0 and chan1 = buffer##getChannelData 1 in
        let len = chan0##.length in

        (* Play source *)
        (Js.Unsafe.coerce playsrc)##.onclick :=
          Js.wrap_callback (fun _ ->
              buffer##copyToChannel chan0 0;
              buffer##copyToChannel chan1 1;
              Js.Unsafe.fun_call (Js.Unsafe.js_expr "playBuffer") [|Js.Unsafe.inject buffer|];
            );
        (Js.Unsafe.coerce playsrc)##.disabled := Js.bool false;

        let chan0_cpy = new%js Typed_array.float32Array len
        and chan1_cpy = new%js Typed_array.float32Array len in

        (* Run the node *)
        I.reset ();

        for i = 0 to chan0##.length - 1 do
          let f0 = tarray_get chan0 i and f1 = tarray_get chan1 i in
          let res = I.step [Vfloat f0; Vfloat f1] in
          match res with
          | [Vfloat f0; Vfloat f1] ->
             Typed_array.set chan0_cpy i f0;
             Typed_array.set chan1_cpy i f1;
          | _ -> ()
        done;

        (* Play filtered *)
        (Js.Unsafe.coerce playfilt)##.onclick :=
          Js.wrap_callback (fun _ ->
              buffer##copyToChannel chan0_cpy 0;
              buffer##copyToChannel chan1_cpy 1;
              Js.Unsafe.fun_call (Js.Unsafe.js_expr "playBuffer") [|Js.Unsafe.inject buffer|];
            );
        (Js.Unsafe.coerce playfilt)##.disabled := Js.bool false;

        (* (\* Create chart *\) *)
        (* let decimate_factor = 1 in *)
        (* let decimate data = *)
        (*   let rec sum start k = *)
        (*     if k = decimate_factor then 0. *)
        (*     else tarray_get data (start + k) +. sum start (k + 1) *)
        (*   in *)
        (*   Array.init (len/decimate_factor) (fun i -> (sum (i * decimate_factor) 0) /. (float_of_int decimate_factor)) *)
        (* in *)

        (* let src1 = create_dataset (decimate chan0) "Source 1" "red" in *)
        (* let src2 = create_dataset (decimate chan1) "Source 2" "orange" in *)
        (* let filt1 = create_dataset (decimate chan0_cpy) "Filtered 1" "blue" in *)
        (* let filt2 = create_dataset (decimate chan1_cpy) "Filtered 2" "cyan" in *)

        (* let data = Chartjs.empty_data () in *)
        (* data##.datasets := Js.array [|src1;src2;filt1;filt2|]; *)
        (* data##.xLabels := Js.array (Array.make (len/decimate_factor/2) ""); *)
        (* let options = create_options () in *)
        (* Firebug.console##log options; *)
        (* options##.elements := Chartjs.empty_elements (); *)
        (* options##.elements##.line := Chartjs.empty_line_element (); *)
        (* options##.elements##.line##.tension := 0.; *)
        (* options##.elements##.line##.fill := Chartjs.Fill.zero; *)
        (* options##.elements##.point := Chartjs.empty_point_element (); *)
        (* options##.elements##.point##.radius := 0; *)
        (* let chart = Chartjs.chart_from_id Chartjs.Chart.line data options chartdivid in *)
        (* Js.Unsafe.global##.chart := chart *)
      in
      ignore (bufferprom##then_ (Js.wrap_callback loadCallback))
    in

    (match !prevLoaded with
     | Some file -> fileHandler file
     | _ -> ());

    let inputhandler =
      (fun _ ->
        let file = opt_get ((optdef_get (input##.files))##item 0) in
        prevLoaded := Some file;
        fileHandler file;
        Js.bool false) in
    input##.oninput := Dom.handler inputhandler;

    (fun _ -> ())
end
