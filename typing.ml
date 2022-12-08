
open Format
open Lib
open Ast
open Tast

let debug = ref false

let dummy_loc = Lexing.dummy_pos, Lexing.dummy_pos

exception Error of Ast.location * string
exception Anomaly of string

let error loc e = raise (Error (loc, e))

module Dico = Hashtbl
(* TODO environnement pour les types structure *)

let env_struct = Dico.create 11

(*  environnement pour les fonctions *)
let env_fonc = Dico.create 11


let rec type_type = function
  | PTident { id = "int" } -> Tint
  | PTident { id = "bool" } -> Tbool
  | PTident { id = "string" } -> Tstring
  | PTptr ty -> Tptr (type_type ty)
  | PTident { id } ->
    try 
      Tstruct (Dico.find env_struct id)
    with Not_found -> error dummy_loc ("unknown struct " ^ id)
  | _ -> error dummy_loc ("unknown struct ")

let rec eq_type ty1 ty2 = match ty1, ty2 with
  | Tint, Tint | Tbool, Tbool | Tstring, Tstring -> true
  | Tstruct { s_name = id1 ; _ }, Tstruct {s_name = id2 ; _ } -> id1 == id2
  | Tptr ty1, Tptr ty2 -> eq_type ty1 ty2
  | Twild, _ | _,Twild -> true  (* pas 100% sur que l'on doive traiter les Twild comme ça *)
  | Tmany [], Tmany [] -> true
  | Tmany (t1::q1), Tmany (t2::q2) -> (eq_type t1 t2) && (eq_type (Tmany(q1)) (Tmany(q2)))
  | _ -> false

let fmt_used = ref false
let fmt_imported = ref false

let evar v = { expr_desc = TEident v; expr_typ = v.v_typ }

let new_var =
  let id = ref 0 in
  fun x loc ?(used=false) ty ->
    incr id;
    { v_name = x; v_id = !id; v_loc = loc; v_typ = ty; v_used = used; v_addr = 0; v_depth = 0 }

module Env = struct
  module M = Map.Make(String)
  type t = var M.t
  let empty = M.empty
  let find = M.find
  let add env v = M.add v.v_name v env

  let all_vars = ref []
  let check_unused () =
    let check v =
      if v.v_name <> "_" && (* TODO used *) true then error v.v_loc "unused variable" in
    List.iter check !all_vars


  let var x loc ?used ty env =
    let v = new_var x loc ?used ty in
    all_vars := v :: !all_vars;
    add env v, v

  (* TODO type () et vecteur de types *)
end

let tvoid = Tmany []
let make d ty = { expr_desc = d; expr_typ = ty }
let stmt d = make d tvoid

let rec expr env e =
 let e, ty, rt = expr_desc env e.pexpr_loc e.pexpr_desc in
  { expr_desc = e; expr_typ = ty }, rt

and expr_desc env loc = function 
(* renvoie le triplet:                                     
description de l'expression, en utilisant le type du tast,
typage de l'expression, en utilisant les types du tast possiblement tvoid,
booléen valant true ssi il y a un retour dans l'expression   *)
  | PEskip ->
     TEskip, tvoid, false
  | PEconstant c ->
    (* TODO *) TEconstant c, tvoid, false
  | PEbinop (op, e1, e2) ->
    (* TODO *) assert false
  | PEunop (Uamp, e1) ->
    (* TODO *) assert false
  | PEunop (Uneg | Unot | Ustar as op, e1) ->
    (* TODO *) assert false
  | PEcall ({id = "fmt.Print"}, el) ->
    (* TODO *) TEprint [], tvoid, false
  | PEcall ({id="new"}, [{pexpr_desc=PEident {id}}]) ->
     let ty = match id with
       | "int" -> Tint | "bool" -> Tbool | "string" -> Tstring
       | _ -> (* TODO *) error loc ("no such type " ^ id) in
     TEnew ty, Tptr ty, false
  | PEcall ({id="new"}, _) ->
     error loc "new expects a type"
  | PEcall (id, el) ->
     (* TODO *) assert false
  | PEfor (e, b) ->
     (* TODO *) assert false
  | PEif (e1, e2, e3) ->
     (* TODO *) assert false
  | PEnil ->
     (* TODO *) assert false
  | PEident {id=id} ->
     (* TODO *) (try let v = Env.find id env in TEident v, v.v_typ, false
      with Not_found -> error loc ("unbound variable " ^ id))
  | PEdot (e, id) ->
     (* TODO *) assert false
  | PEassign (lvl, el) ->
     (* TODO *) TEassign ([], []), tvoid, false 
  | PEreturn el ->
     (* TODO *) TEreturn [], tvoid, true
  | PEblock el ->
     (* TODO *) TEblock [], tvoid, false
  | PEincdec (e, op) ->
     (* TODO *) assert false
  | PEvars _ ->
     (* TODO *) assert false 

let found_main = ref false


let struct_vide s_name =
  {
    s_name;
    s_fields = Dico.create 11;
    s_size = -1;  (* taille calculee en octets *)
  }


(* 1. declare structures *)
let phase1 = function
  | PDstruct { ps_name = { id = id; loc = loc }; _ } -> 
    if Dico.mem env_struct id then error loc ("duplicate struct " ^ id);
    Dico.add env_struct id (struct_vide id)
  | PDfunction _ -> ()

let rec sizeof = function
  | Tint | Tbool | Tstring | Tptr _ -> 8
  | Tstruct { s_size = n ; _ } -> n
  | Tmany [] -> 0
  | Tmany (t::q) -> sizeof t + sizeof(Tmany q)
  | _ -> assert false 

let pparam_to_var ({ id = id; loc = loc } , pty ) = (* pas 100% sur de cette fonction *)
  let ty = type_type pty in
  let v = new_var id loc ty in
  v

let ajout dico field =
  let {loc = loc ; id =id }, ty = field in
  if Dico.mem dico id then error loc ("duplicate field " ^ id);
  Dico.add dico id { f_name = id ; f_typ = type_type ty; f_ofs = 0}

  (* 2. declare functions and type fields *)
let phase2 = function
  | PDfunction { pf_name={id; loc}; pf_params=pl; pf_typ=tyl; _ } ->
    if id = "main" then found_main := true;
    if Dico.mem env_fonc id then error loc ("duplicate function " ^ id);
    let f = { fn_name = id; fn_params = List.map pparam_to_var pl; fn_typ = List.map type_type tyl; } in
    Dico.add env_fonc id f
     
  | PDstruct { ps_name = {id; _}; ps_fields = fl } ->
    let { s_name = nom ; s_fields = dictio ; _ } = Dico.find env_struct id in
    List.iter (ajout dictio) fl
        
let addressing_fields dico l_f =
  let rec aux lst ad =
    match lst with
    | [] -> ad
    | ({ id = name ; _}, _ )::q -> let field = Dico.find dico name in
                                    field.f_ofs <- ad;
                                    aux q (ad + sizeof field.f_typ);
  in
  aux l_f 0

    (* 3. type check function bodies *)
let decl = function
  | PDfunction { pf_name={id; loc}; pf_body = e; pf_typ=tyl } ->
    (* TODO check name and type *) 
    let f = { fn_name = id; fn_params = []; fn_typ = []} in
    let e, rt = expr Env.empty e in
    TDfunction (f, e)
  
  | PDstruct {ps_name={id} ; ps_fields = l_fields } ->
    let s = Dico.find env_struct id in
    let size = addressing_fields s.s_fields l_fields in
    s.s_size <- size;
    TDstruct s

let file ~debug:b (imp, dl) =
  debug := b;
  fmt_imported := imp;
  List.iter phase1 dl;
  List.iter phase2 dl;
  if not !found_main then error dummy_loc "missing method main";
  let dl = List.map decl dl in
  Env.check_unused (); (* TODO variables non utilisees *)
  if imp && not !fmt_used then error dummy_loc "fmt imported but not used";
  dl
