
(* arbres issus du typeur *)

type unop = Ast.unop  (* opération unaire *)

type binop = Ast.binop  (* opération binaire *)

type constant = Ast.constant  (* constante *)

type incdec = Ast.incdec  (* ++ -- *)

type function_ = {  (* type des fonctions *)
    fn_name: string; (* nom de la fonction *)
  fn_params: var list; (* paramètres de la fonction, sous forme de liste de variables *)
     fn_typ: typ list;  (* types de retour de la fonction *)
}

and field = { (* champ d'une structure *)
         f_name: string; (* nom de la structure *)
          f_typ: typ;  (* type du champ *)
  mutable f_ofs: int; (* relatif à l'adresse de l'objet, somme des tailles des champs précédents *) 
}

and structure = { (* type structure *)
          s_name: string; (* nom de la structure *)
        s_fields: (string, field) Hashtbl.t;  (* dictionnaire des champs, indexés par leur nom *)
  mutable s_size: int; (* taille calculee en octets *)
}

and typ =  (* tout les types possibles *)
  | Tint | Tbool | Tstring (* types de bases *)
  | Tstruct of structure (* type structure *)
  | Tptr of typ (* pointeur vers un type, pour faire des tableaux *)
  | Twild (* type wildcard, tout type *)
  | Tmany of typ list (* 0 pour type retour instructions et >=2 pour retour functions *)

and var = { (* type des variables *)
          v_name: string; (* nom de la variable *)
            v_id: int;  (* identifiant unique *)
           v_loc: Ast.location; (* location, on s'en fout *)
           v_typ: typ; (* type de la variable *)
         v_depth: int;  (* index de portee *)
  mutable v_used: bool;  (* utilistaion de la variable ou non *) 
  mutable v_addr: int;  (* adresse relative au pointer de frame (rbp) *)
}

and expr = (* type des expressions *)
  { expr_desc: expr_desc; (* description de l'expression *)
    expr_typ : typ; (* type de l'expression *)
    }

and expr_desc =
  | TEskip
  | TEconstant of constant
  | TEbinop of binop * expr * expr
  | TEunop of unop * expr
  | TEnil
  | TEnew of typ (* cas particulier de PEcall *)
  | TEcall of function_ * expr list
  | TEident of var
  | TEdot of expr * field
  | TEassign of expr list * expr list
  | TEvars of var list * expr list
  | TEif of expr * expr * expr
  | TEreturn of expr list
  | TEblock of expr list
  | TEfor of expr * expr
  | TEprint of expr list (* cas particulier de PEcall *)
  | TEincdec of expr * incdec

type tdecl = (* declaration des fonctions et des struct *)
  | TDfunction of function_ * expr
  | TDstruct of structure

type tfile = tdecl list (* liste des déclarations *)
