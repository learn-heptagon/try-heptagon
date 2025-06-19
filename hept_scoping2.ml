(* Functions that could be in hept_scoping.ml *)

(** Conversion from a Types.static_exp record into a Hept_parsetree.static_exp record **)
let rec translate_into_hept_parsetree_static_exp (se : Types.static_exp) : Hept_parsetree.static_exp =
  Hept_parsetree.{
    se_desc = translate_into_hept_parsetree_static_exp_desc se.se_desc;
    se_loc  = se.se_loc;
  }

(** Conversion from a Types.static_exp_desc value into a Hept_parsetree.static_exp_desc value **)
and translate_into_hept_parsetree_static_exp_desc (d : Types.static_exp_desc) : Hept_parsetree.static_exp_desc =
  match d with
  | Types.Svar c -> Hept_parsetree.Svar (Hept_parsetree.Q c)
  | Types.Sint i -> Hept_parsetree.Sint i
  | Types.Sfloat f -> Hept_parsetree.Sfloat f
  | Types.Sbool b -> Hept_parsetree.Sbool b
  | Types.Sstring s -> Hept_parsetree.Sstring s
  | Types.Sconstructor c -> Hept_parsetree.Sconstructor (Hept_parsetree.Q c)
  | Types.Sfield f -> Hept_parsetree.Sfield (Hept_parsetree.Q f)
  | Types.Stuple sel -> Hept_parsetree.Stuple (List.map translate_into_hept_parsetree_static_exp sel)
  | Types.Sarray sel -> Hept_parsetree.Sarray (List.map translate_into_hept_parsetree_static_exp sel)
  | Types.Sarray_power (e, sel) ->
      Hept_parsetree.Sarray_power (
        translate_into_hept_parsetree_static_exp e,
        List.map translate_into_hept_parsetree_static_exp sel
      )
  | Types.Srecord fields ->
      Hept_parsetree.Srecord
        (List.map (fun (f, e) -> (Hept_parsetree.Q f, translate_into_hept_parsetree_static_exp e)) fields)
  | Types.Sop (fn, args) ->
      Hept_parsetree.Sop (Hept_parsetree.Q fn, List.map translate_into_hept_parsetree_static_exp args)

(** Conversion from a Types.ty value into a Hept_parsetree.ty value **)
let rec translate_into_hept_parsetree_ty (ty : Types.ty) : Hept_parsetree.ty =
  match ty with
  | Types.Tprod tyl ->
      Hept_parsetree.Tprod (List.map translate_into_hept_parsetree_ty tyl)
  | Types.Tid qn ->
      Hept_parsetree.Tid (Hept_parsetree.Q qn)
  | Types.Tarray (ty1, se) ->
      let hpt_ty = translate_into_hept_parsetree_ty ty1 in
      (** Conversion from a Types.static_exp record into a Hept_parsetree.exp record:
      *** 1st step: conversion from a Types.static_exp record to a to Hept_parsetree.static_exp record
      *** -> translate_into_hept_parsetree_static_exp se
      ***
      *** 2nd step: encapsulation of a Hept_parsetree.static_exp record in a Hept_parsetree.edesc value
      *** -> Hept_parsetree.Econst (translate_into_hept_parsetree_static_exp se)
      ***
      *** 3rd step: construction of a Hept_parsetree.exp record
      *** -> let e = Hept_parsetree.{ ... }
      **)
      let e = Hept_parsetree.{
        e_desc = Hept_parsetree.Econst (translate_into_hept_parsetree_static_exp se);
        e_ct_annot = None;
        e_loc = se.se_loc
      } in
      Hept_parsetree.Tarray (hpt_ty, e)
  | Types.Tinvalid ->
      Hept_parsetree.Tinvalid
