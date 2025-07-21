open Js_of_ocaml

let rec js_of_modul (m : Names.modul) : Js.Unsafe.any =
  match m with
    | Pervasives ->
      Js.Unsafe.obj [|
        ("tag", Js.Unsafe.inject (Js.string "Pervasives"))
      |]
    | LocalModule ->
      Js.Unsafe.obj [|
        ("tag", Js.Unsafe.inject (Js.string "LocalModule"))
      |]
    | Module name ->
      Js.Unsafe.obj [|
        ("tag", Js.Unsafe.inject (Js.string "Module"));
        ("name", Js.Unsafe.inject (Js.string name))
      |]
    | QualModule q ->
      Js.Unsafe.obj [|
        ("tag", Js.Unsafe.inject (Js.string "QualModule"));
        ("qualname", Js.Unsafe.inject (js_of_qualname q))
      |]

and js_of_qualname (q : Names.qualname) : Js.Unsafe.any =
  Js.Unsafe.obj [|
    ("qual", Js.Unsafe.inject (js_of_modul q.qual));
    ("name", Js.Unsafe.inject (Js.string q.name))
  |]

let rec js_of_obc (v : Obc_interp.value) : Js.Unsafe.any =
  match v with
    | Vbool b -> Js.Unsafe.inject (Js.bool b)
    | Vint i -> Js.Unsafe.inject (Js.number_of_float (float_of_int i))
    | Vfloat f -> Js.Unsafe.inject (Js.number_of_float f)
    | Vconstructor c -> Js.Unsafe.inject (js_of_qualname c)
    | Varray arr ->
      let js_arr = Array.map js_of_obc arr in
      Js.Unsafe.inject (Js.array js_arr)
    | Vundef -> Js.Unsafe.inject Js.null

let rec obc_of_js (t : Types.ty) (v : Js.Unsafe.any) : Obc_interp.value =
  match t with
    | Tprod tys ->
      let js_arr = Js.to_array (Js.Unsafe.coerce v) in
      let values = List.mapi (fun i ty ->
        if i < Array.length js_arr then
          obc_of_js ty js_arr.(i)
        else
          Vundef
      ) tys in
      Varray (Array.of_list values)

    | Tid { name = "bool"; _ } ->
      let b = Js.to_bool (Js.Unsafe.coerce v) in
      Vbool b

    | Tid { name = "int"; _ } ->
      let i = Js.float_of_number (Js.Unsafe.coerce v : Js.number Js.t) in
      Vint (int_of_float i)

    | Tid { name = "float"; _ }
    | Tid { name = "real"; _ } ->
      let f = Js.float_of_number (Js.Unsafe.coerce v : Js.number Js.t) in
      Vfloat f

    | Tid _ -> failwith "TODO Tid"

    | Tarray (base_ty, _) ->
      let js_arr = Js.to_array (Js.Unsafe.coerce v) in
      let values = Array.map (obc_of_js base_ty) js_arr in
      Varray values

    | Tinvalid -> Vundef
