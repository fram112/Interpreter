
open Interpreter;;

(* Creazione dizionari *)
(* > Creo un dizionario di prova a valori interi. Non mi aspetto nessuna eccezione*)
let m = Dict(Item(("mele", CstInt 430), Item(("banane", CstInt 312), Item(("arance", CstInt 525), Item(("pere", CstInt 217), Empty)))));;

(* > Creo un dizionario di prova con valori di tipo diverso. Mi aspetto una failure. *)
let mw = Dict(Item(("mele", CstInt 430), Item(("banane", CstInt 312), Item(("arance", CstStr "arancine"), Item(("pere", CstInt 217), Empty)))));;

(* > Creo un dizionario di prova con chiavi duplicate. Mi aspetto una failure. *)
let md = Dict(Item(("mele", CstInt 430), Item(("banane", CstInt 312), Item(("arance", CstInt 525), Item(("mele", CstInt 217), Empty)))));;

(* > Creo un dizionario di prova a valori booleani.Non mi aspetto nessuna eccezione *)
let b = Dict(Item(("mele", CstTrue), Item(("banane", CstFalse), Item(("arance", CstFalse), Item(("pere", CstTrue), Empty)))));;

(* > Creo un dizionario di prova a valori stringhe. Non mi aspetto nessuna eccezione *)
let s = Dict(Item(("mele", CstStr "a"), Item(("banane", CstStr "b"), Item(("arance", CstStr "c"), Item(("pere", CstStr "d"), Empty)))));;

(* > Creo un dizionario di prova a valori dizionari. Non mi aspetto nessuna eccezione *)
let mm = Dict(Item(("m", m), Item(("empty", Dict(Empty)), Item(("s", s), Item(("b", b), Empty)))));;



(* Defionizione funzioni *)
(* funzione anonima unaria sugli interi *)
let fi = Fun(["x"], Sum(Den "x", CstInt 1));;

(* funzione anonima unaria sui booleani *)
let fb = Fun(["x"], And(Den "x", CstFalse));;

(* funzione anonima unaria sulle stringhe *)
let fs = Fun(["x"], Concat(Den "x", CstStr "X"));;

(* funzione anonima binaria sugli interi *)
let fbin = Fun(["x"; "y"], Sum(Den "x", Den "y"));;

(* applicazione di funzione binaria *)
let afbin = Apply(fbin, [CstInt 3; CstInt 4]);;



(* Test inserimento *)
(*    > Inserisco un elemento di tipo diverso da quello del dizionario. Mi aspetto una failure.*)
let ins1 = Insert("kiwi", CstStr "ciao", m);;

(*    > Inserisco un elemento con una chiave gia' presente nel dizionario. Mi aspetto l'eccezione ExistingKey.*)
let ins2 = Insert("pere", CstInt 300, m);;

(*    > Inserisco un elemento correttamente nel dizionario. Non mi aspetto nessuna eccezione.*)
let ins3 = Insert("kiwi", CstInt 300, m);;



(* Test cancellazione *)
(*    > Cancello un elemento non esistente dal dizionario. Mi aspetto l'eccezione KeyNotFound.*)
let del1 = Delete(m, "inesistente");;

(*    > Cancello un elemento esistente dal dizionario. Non mi aspetto nessuna eccezione.*)
let del2 = Delete(m, "pere");;



(* Test esistenza chiave *)
(*    > Cerco una chiave non esistente nel dizionario. Non mi aspetto nessuna eccezione. *)
let has1 = Has_key("inesistente", m);;

(*    > Cerco una chiave esistente dal dizionario. Non mi aspetto nessuna eccezione.*)
let has2 = Has_key("banane", m);;



(*Test iterate *)
(* > Applico una funzione unaria su interi a tutti gli elementi del dizionario a valori interi. Non mi aspetto nessuna eccezione. *)
let it1 = Iterate(fi, m);;

(* > Applico una funzione unaria su stringhe a tutti gli elementi del dizionario a valori stringhe. Non mi aspetto nessuna eccezione. *)
let it2 = Iterate(fs, s);;

(* > Applico una funzione unaria su interi a tutti gli elementi del dizionario a valori stringhe. Mi aspetto una failure. *)
let it3 = Iterate(fi, s);;

(* > Applico una funzione binaria su interi a tutti gli elementi del dizionario a valori interi. Mi aspetto una failure. *)
let it4 = Iterate(fbin, m);;



(* Test fold *)
(* > Applico la fold con una funzione su interi sul dizionario a valori interi. Non mi aspetto nessuna eccezione*)
let foldi = Fold(fi, m);;

(* > Applico la fold con una funzione su booleani sul dizionario a valori booleani. Non mi aspetto nessuna eccezione*)
let foldb = Fold(fb, b);;

(* > Applico la fold con una funione su stringhe sul dizionario a valori stringhe. Non mi aspetto nessuna eccezione*)
let folds = Fold(fs, s);;

(* > Applico la fold con una funzione su interi sul dizionario a valori dizionari. Mi aspetto una failure.*)
let foldmm = Fold(fi, mm);;
(* La failure viene lanciata in quanto la funzione sugli interi non e' applicabile a valori di 
tipo dizionario, se invece fosse stata usata una funzione definita per valori di tipo dizionario,
sarebbe stata lanciata un'altra failure perche' la somma tra valori dizionario non e' definita *)



(* Test filter *)
(* > Filtro il dizionario con una lista di chiavi. Non mi aspetto nessuna eccezione. *)
let fil1 = Filter(["mele"; "pere"], m);;

(* > Filtro il dizionario con una lista vuota. Non mi aspetto nessuna eccezione. *)
let fil2 = Filter([], m);;