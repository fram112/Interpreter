
(* Sintassi astratta, parole del linguaggio *)
type ide = string;;

type exp = 
        |   CstInt of int
        |   CstTrue
        |   CstFalse
        |   CstStr of ide
        |   Den of ide
        |   Equals of exp * exp
        |   Minor of exp * exp
        |   Greater of exp * exp
        |   Minoreq of exp * exp
        |   Greatereq of exp * exp
        |   Sum of exp * exp
        |   Diff of exp * exp
        |   Times of exp * exp
        |   Div of exp * exp
        |   Mod of exp * exp
        |   And of exp * exp
        |   Or of exp * exp
        |   Not of exp
        |   Minus of exp
        |   Concat of exp * exp
        |   Ifthenelse of exp * exp * exp           
        |   Let of ide * exp * exp                  
        |   Letrec of ide * ide list * exp * exp         
        |   Fun of ide list * exp     (* funzione anonima con lista di parametri *)                
        |   Apply of exp * exp list   (* applicazione di funzione *)
        |   Dict of dictionary        (* creazione dizionario *)
        |   Insert of ide * exp * exp (* inserisce la chiave e il valore associato *)
        |   Delete of exp * ide       (* rimuove dal dizionario la chiave-valore con chiave uguale a quella passata come parametro *)
        |   Has_key of ide * exp      (* controlla l'esistenza di una chiave nel dizionario *)
        |   Iterate of exp * exp      (* applica una funzione a tutte le coppie chiave-valore del dizionario, restituendo un nuovo dizionario *)
        |   Fold of exp * exp         (* calcola il valore ottenuto applicando la funzione sequenzialmente a tutti gli elementi del dizionario *)
        |   Filter of ide list * exp  (* elimina tutte le coppie chiave-valore dal dizionario per cui la chiave non appartiene alla lista *)
        
        and dictionary  =  
                          Empty 
                        | Item of (ide * exp) * dictionary


(* Ambiente polimorfo *)
type 't env = (ide * 't) list;; 


(* Tipi esprimibili *)
type evT = 
        |   Unbound
        |   Int of int
        |   Bool of bool
        |   String of string
        |   Closure of ide list * exp * evT env 
        |   RecClosure of ide * ide list * exp * evT env 
        |   Dictionary of (ide * evT) list;;

exception WrongBindList

(* continuiamo con l'ambiente *)
let emptyEnv = [("", Unbound)];;

let bind (r : 't env) (i : ide) (v : evT) = (i, v)::r;;

let rec bindlist (r:evT env) (il: ide list) (el: evT list) =
    match (il, el) with
    | ([], []) -> r
    | (i::il1, e::el1) -> bindlist (bind r i e) il1 el1
    | _ -> raise WrongBindList

let rec lookup (r : 't env) (i : ide) =
    match r with
    |   [] -> Unbound
    |   (name, value)::tail when name = i -> value
    |   _::tail -> lookup tail i;; 


(* Supporto a RunTime *)
(* Controllo dei tipi *)
let typecheck (s : string) (v : evT) : bool = 
    match s with
    |   "int" -> (match v with
                    Int n -> true
                  | _ -> false )
    |   "bool" -> (match v with
                    Bool p -> true
                  | _ -> false )
    |   "string" -> (match v with
                    String s -> true 
                    | _ -> false)
    |   "dictionary" -> (match v with
                         Dictionary d -> true 
                        | _ -> false )
    |   _ -> failwith ("Type error");;

(* Eccezioni *)
exception GenericError of string;;
exception KeyNotFound;;
exception ExistingKey of string;;

(* Funzioni basilari *)
let et x y = 
    if typecheck "bool" x && typecheck "bool" y
    then (
            match (x, y) with
            |   (Bool n, Bool u) -> Bool(n && u)
            |   (_,_) -> raise (GenericError "et function")
    )
    else failwith("Type error");;

let times x y =
    if typecheck "int" x && typecheck "int" y 
    then (
            match (x, y) with
            |   (Int n, Int u) -> Int(n * u)
            |   (_,_) -> raise (GenericError "prod function")
    )
    else failwith("Type error");;

let div x y =
    if typecheck "int" x && typecheck "int" y 
    then (
            match (x, y) with
            |   (Int n, Int u) -> if u = 0 then failwith ("Division by zero")
                               else Int(n / u)
            |   (_,_) -> raise (GenericError "div function")
    )
    else failwith("Type error");;

let modul x y =
    if typecheck "int" x && typecheck "int" y 
    then (
            match (x, y) with
            |   (Int n, Int u) -> if u = 0 then failwith ("Division by zero")
                                  else Int(n mod u)
            |   (_,_) -> raise (GenericError "div function")
    )
    else failwith("Type error");;

let sum x y =
    if typecheck "int" x && typecheck "int" y
    then (
        match (x, y) with
        |   (Int n, Int u) -> Int(n + u)       
        |   (_,_) -> raise (GenericError "sum function")
    ) 
    else failwith("Type error");;

let diff x y =
    if typecheck "int" x && typecheck "int" y 
    then (
            match (x, y) with
            |   (Int n, Int u) -> if n > u then Int(n - u)
                                  else Int(u - n)
            |   (_,_) -> raise (GenericError "diff function")
    )
    else failwith("Type error");;

let equals x y = 
    if typecheck "int" x && typecheck "int" y
    then (
            match (x, y) with
            |   (Int n, Int u) -> Bool(n = u)
            |   (_,_) -> raise (GenericError "eq function")
    )
    else failwith("Type error");;

let minoreq x y = 
    if typecheck "int" x && typecheck "int" y
    then (
            match (x, y) with
            |   (Int n, Int u) -> Bool(n <= u)
            |   (_,_) -> raise (GenericError "minoreq function")
    )
    else failwith("Type error");;

let minor x y = 
    if typecheck "int" x && typecheck "int" y
    then (
            match (x, y) with
            |   (Int n, Int u) -> Bool(n < u)
            |   (_,_) -> raise (GenericError "minor function")
    )
    else failwith("Type error");;

let greatereq x y = 
    if typecheck "int" x && typecheck "int" y
    then (
            match (x, y) with
            |   (Int n, Int u) -> Bool(n >= u)
            |   (_,_) -> raise (GenericError "majoreq function")
    )
    else failwith("Type error");;

let greater x y = 
    if typecheck "int" x && typecheck "int" y
    then (
            match (x, y) with
            |  (Int n, Int u) -> Bool(n > u)
            |   (_,_) -> raise (GenericError "major function")
    )
    else failwith("Type error");;

let vel x y = 
    if typecheck "bool" x && typecheck "bool" y
    then (
            match (x, y) with
            |   (Bool n, Bool u) -> Bool(n || u)
            |   (_,_) -> raise (GenericError "vel function")
    )
    else failwith("Type error");;

let non x = 
    if typecheck "bool" x = true 
    then (
            match x with
            |  Bool true -> Bool false
            |  Bool false -> Bool true
            |  _ -> raise (GenericError "non function")
    )
    else failwith ("Type error");;

let minus x =
    if typecheck "int" x = true
    then (
            match x with   
            |   Int n -> Int(-n)
            |   _ -> raise (GenericError "minus function")
    )
    else failwith ("Type error");;

let concat s ss = 
    if typecheck "string" s && typecheck "string" ss
    then (
            match (s, ss) with
            |   (String z, String zz) -> String(z ^ zz)
            |   _ -> raise (GenericError "concat function")
    )
    else failwith ("Type error");;

(* Funzioni di supporto per il dizionario *)

(* gets the string of evT type *)
let getType x = 
    match x with
        |   Unbound -> "Unbound"
        |   Int n -> "Int"
        |   Bool n -> "Bool"
        |   String n -> "String"
        |   Closure (a,b,c) -> "Closure"
        |   RecClosure (a,b,c,d) -> "RecClosure"
        |   Dictionary d -> "Dictionary";;

(* gets the default value for a evT type *)
let defaultValueType typeS =
    match typeS with
            |   "Int" -> Int(0)
            |   "Bool" -> Bool(true)
            |   "String" -> String("")
            |   "Dictionary" -> Dictionary([])
            |   _ -> Unbound;;

(* selects the correct sum between two evT type, if is defined *)
let selectSum x y = 
    match (x,y) with
    |   Int n, Int m -> sum (Int(n)) (Int(m))
    |   Bool n, Bool m -> vel (Bool(n)) (Bool(m)) 
    |   String n, String m -> concat (String(n)) (String(m)) 
    |   _, _ -> raise (GenericError "Sum not defined for this type");;

(* counts the occurrences of a key in the dictionary *)
let rec occurrences dict key counter = 
    match dict with
    |   Empty -> counter
    |   Item ((k, v), tail) -> if k = key 
                               then occurrences tail key (1+counter)
                               else occurrences tail key counter;;

(* inserts a new item in the dictionary *)
let rec insert dict key value =
    match dict with
    |   [] -> (key, value)::[]
    |   (k, v)::tail -> if k = key
                        then raise (ExistingKey "The key is already present in the dictionary")
                        else (k, v)::(insert tail key value);;

(* deletes an item from the dictionary *)
let rec delete dict key =
    match dict with
    |   [] -> raise KeyNotFound
    |   (k, v)::tail -> if k = key
                        then tail
                        else (k, v)::(delete tail key);;

(* returns a list of all the keys in the dictionary *)
let rec keySet dict = 
    match dict with 
    |   [] -> []
    |   (k, v)::tail -> k::keySet tail;;
        
(* verifies if a key is present in a list of keys *)
let rec containsKey key set = 
    match set with
        |   [] -> false
        |   k::tail -> if k = key
                        then true
                        else containsKey key tail;;

(* filter the dictionary, retaining only items with key present in list *)
let rec filter list cursor dict =
    match cursor with
        |   [] -> dict
        |   (k, v)::tail -> if containsKey k list
                            then filter list tail dict
                            else filter list tail (delete dict k);;

(* L'interprete con regola di scoping statico *)
let rec eval (e: exp) (r: 't env) : evT = 
    match e with
    |   CstInt n -> Int n
    |   CstTrue -> Bool true
    |   CstFalse -> Bool false
    |   CstStr s -> String s
    |   Den i -> lookup r i
    |   Minoreq (a, b) -> minoreq (eval a r) (eval b r)
    |   Minor (a, b) -> minor (eval a r) (eval b r)
    |   Greatereq (a, b) -> greatereq (eval a r) (eval b r)
    |   Greater (a, b) -> greater (eval a r) (eval b r)
    |   Times (a, b) -> times (eval a r) (eval b r)
    |   Div (a, b) -> div (eval a r) (eval b r)
    |   Mod (a, b) -> modul (eval a r) (eval b r)
    |   Sum (a, b) -> sum (eval a r) (eval b r) 
    |   Diff (a, b) -> diff (eval a r) (eval b r)
    |   Minus a -> minus (eval a r)
    |   And (a, b) -> et (eval a r) (eval b r) 
    |   Or (a, b) -> vel (eval a r) (eval b r)
    |   Not a -> non (eval a r)
    |   Concat (s, ss) -> concat (eval s r) (eval ss r)
    |   Equals (a, b) -> equals (eval a r) (eval b r)
    |   Ifthenelse (guard, thEn, eLse) ->
            let guardia = eval guard r in 
                if typecheck "bool" guardia = true
                then
                    match guardia with
                    |   Bool true -> eval thEn r
                    |   Bool false -> eval eLse r
                    |   _ -> raise (GenericError "Unexpected") 
                else failwith ("Non boolean guard")
    |   Let (i, e1, e2) -> eval (e2) (bind r i (eval e1 r))
    |   Letrec (f, paramL, fbody, lbody) -> 
            let bEnv = bind r f (RecClosure (f,paramL,fbody,r)) in 
                eval lbody bEnv
    |   Fun (paramL, fbody) -> Closure (paramL, fbody, r) 
    |   Apply (eF, eArgL) -> 
            let fclosure = eval eF r in (
                match fclosure with
                |   Closure (args, fbody, fDecEnv) -> 
                        let aVals = evalList eArgL r in 
                            let aEnv = bindlist fDecEnv args aVals in 
                                eval fbody aEnv 
                |   RecClosure (f, args, fbody, fDecEnv) -> 
                        let aVals = evalList eArgL r in 
                            let rEnv = bind fDecEnv f fclosure in 
                                let aEnv = bindlist rEnv args aVals in 
                                    eval fbody aEnv
                |   _ -> failwith ("Non functional value")
            )
    |   Dict dict -> Dictionary (evalD dict r) 
    |   Insert (key, value, dict) -> Dictionary (
            let eD = eval dict r in
                let eValue = eval value r in
                    if typecheck "dictionary" eD  
                    then match eD with
                        |   Dictionary ((k,v)::tail) ->
                                if getType(v) = getType(eValue)
                                then insert ((k,v)::tail) key eValue
                                else failwith ("Incompatible type")
                        |   _ -> raise (GenericError "eval function")
                    else failwith ("Type error")   
        )
    |   Delete (dict, key) -> Dictionary (
            let eD = eval dict r in
                if typecheck "dictionary" eD
                then match eD with
                    |   Dictionary d -> delete d key 
                    |   _ -> raise (GenericError "eval function")
                else failwith ("Type error")
        )
    |   Has_key (key, dict) -> Bool (
            let eD = eval dict r in
                if typecheck "dictionary" eD
                then match eD with
                    |   Dictionary d -> containsKey key (keySet d)
                    |   _ -> raise (GenericError "eval function")
                else failwith ("Type error")
        )
    |   Iterate (f, dict) -> Dictionary (
            let eD = eval dict r in 
                if typecheck "dictionary" eD 
                then match eD with 
                    |   Dictionary d -> (
                            match eval f r with
                            |   Closure (arg, fbody, fDecEnv) -> iterate f d r
                            |   RecClosure (fn, arg, fbody, fDecEnv) -> iterate f d r
                            |   _ -> failwith ("Non functional value")
                        )
                    |   _ -> raise (GenericError "eval function")
                else failwith ("Type error")
        )
    |   Fold (f, dict) -> (
            let eD = eval dict r in 
                if typecheck "dictionary" eD 
                then match eD with 
                    |   Dictionary ((k, v)::tail) -> (
                            match eval f r with
                            |   Closure (arg, fbody, fDecEnv) -> fold f (defaultValueType (getType v)) ((k, v)::tail) r
                            |   RecClosure (fn, arg, fbody, fDecEnv) -> fold f (defaultValueType (getType v)) ((k, v)::tail) r
                            |   _ -> failwith ("Non functional value")
                        )
                    |   _ -> raise (GenericError "eval function")
                else failwith ("Type error")
        )
    |   Filter (list, dict) -> Dictionary (
            let eD = eval dict r in
                if typecheck "dictionary" eD
                then match eD with
                    |   Dictionary d -> filter list d d
                    |   _ -> raise (GenericError "eval function")
                else failwith ("Type error")
        )
        and sametypecheck typeC dict r =
            match typeC with
            |   "Int" -> (match dict with
                            |   Empty -> true
                            |   Item ((k, v), tail) -> match eval v r with
                                                |   Int n -> sametypecheck typeC tail r
                                                |   _ -> false)
            |   "Bool" -> (match dict with
                            |   Empty -> true
                            |   Item ((k, v), tail) -> (match eval v r with
                                                |   Bool n -> sametypecheck typeC tail r
                                                |   _ -> false))
            |   "String" -> (match dict with
                            |   Empty -> true
                            |   Item ((k, v), tail) -> match eval v r with
                                                |   String n -> sametypecheck typeC tail r
                                                |   _ -> false)
            |   "Dictionary" -> (match dict with
                            |   Empty -> true
                            |   Item ((k, v), tail) -> match eval v r with
                                                |   Dictionary n -> sametypecheck typeC tail r
                                                |   _ -> false)
            |   _ -> failwith("Invalid type for a dictionary value") 

        and evalD dict r =
            match dict with
            |   Empty -> []
            |   Item((k, v), tail) -> if occurrences dict k 0 = 1
                                      then if (sametypecheck (getType (eval v r)) dict r)
                                           then (k, eval v r)::(evalD tail r)
                                           else failwith ("The dictionary cannot contain values of different type")
                                      else failwith ("The dictionary cannot contain duplicate keys")

        and evalList list r =
            match list with
            |   [] -> []
            |   l::ll -> (eval l r)::(evalList ll r)

        and applyAux f v r = 
            match eval f r with
            |   Closure ((arg::[]), fbody, fDecEnv) -> (
                    let aEnv = bind fDecEnv arg v in 
                            eval fbody aEnv 
                ) 
            |   RecClosure (fn, (arg::[]), fbody, fDecEnv) -> (
                    let rEnv = bind fDecEnv fn (eval f r) in 
                        let aEnv = bind rEnv arg v in 
                            eval fbody aEnv
                )
            |   _ -> failwith ("Not applicable fun")

        and iterate f d r = 
            match d with
                |   [] -> []
                |   (k, v)::tail -> (k, (applyAux f v r))::(iterate f tail r) 

        and fold f acc dict r = 
            match dict with
                |   [] -> acc
                |   (k, v)::tail -> fold f (selectSum acc (applyAux f v r)) tail r
    ;;