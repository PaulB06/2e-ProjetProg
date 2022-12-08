
(* arbres issus du parseur *)

type location = Lexing.position * Lexing.position (* on s'en occupe pas *)

type ident = { loc : location; id : string } (* loc + id qui définit le nom du truc *)

type unop = (* opération unaire *)
  | Uneg | Unot | Uamp | Ustar

type binop =  (* opération binaire *)
  | Badd | Bsub | Bmul | Bdiv | Bmod
  | Beq | Bne | Blt | Ble | Bgt | Bge
  | Band | Bor (* && || *)

type constant =  (* constante *)
  | Cbool of bool
  | Cint of int64
  | Cstring of string

type ptyp =  (* type général de ast *)
  | PTident of ident (* bool, int, struct id *)
  | PTptr   of ptyp

type incdec = Inc | Dec (* ++ -- *)

type pexpr = (* l'expression et sa loc *)
  { pexpr_desc : pexpr_desc;
    pexpr_loc  : location; }

and pexpr_desc = (* différents types d'expressions *)
  | PEskip
  | PEconstant of constant
  | PEbinop of binop * pexpr * pexpr
  | PEunop of unop * pexpr
  | PEnil
  | PEcall of ident * pexpr list (* si la string de ident vaut "nex" ou "print", distinguer le cas *)
  | PEident of ident
  | PEdot of pexpr * ident
  | PEassign of pexpr list * pexpr list
  | PEvars of ident list * ptyp option * pexpr list
  | PEif of pexpr * pexpr * pexpr
  | PEreturn of pexpr list
  | PEblock of pexpr list
  | PEfor of pexpr * pexpr
  | PEincdec of pexpr * incdec

and pparam = ident * ptyp  (* paramètres des fonctions *)

type pfunc = { (* type fonction *)
  pf_name   : ident; (* nom de la fonction *)
  pf_params : pparam list; (* liste des paramètres *)
  pf_typ    : ptyp list; (* types de retour *)
  pf_body   : pexpr; (* corps de la fonction *)
}
 
type pfield = ident * ptyp (* champ des struct, nom + type *)

type pstruct = { (* type struct *)
  ps_name   : ident; (* nom de la struct *)
  ps_fields : pfield list; (* liste des champs *)
}

type pdecl = (* type déclaration, struct ou fonction *)
  | PDfunction of pfunc
  | PDstruct   of pstruct

type import_fmt = bool (* true si on importe fmt | fmt est importé *)

type pfile = import_fmt * pdecl list (* import fmt + liste des déclarations *)