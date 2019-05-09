(* map_modules.ml: provides two modules for maps.
   1. StringStringMap which maps strings to strings
   2. IntpairBoolMap which maps pairs of ints to bools.
   Both modules are created by creating a short module adhering to the
   Treemap.KEYVAL_SIG signature and then invoking the Treemap.Make
   functor. *)



open Printf;;

(* Interface module for maps of string to string *)
module StringStringKV = struct
  type key_t = string;;
  type value_t = string;;
  let compare_keys = String.compare;;
  let keyval_string k v = sprintf "{%s -> %s}" k v;;
    ;;
end;;

(* A map module from string keys to string values. *)
module StringStringMap = Treemap.Make(StringStringKV);;


(* Interface module for maps of int pairs to bool *)
module IntpairBoolKV  = struct
type key_t = int*int;;
type value_t = bool;;
let compare_keys x y =
  if (fst x != fst y)  then
    fst x - fst y
  else
    snd x - snd y
let keyval_string (k , k2) v =
  sprintf "{%d > %d : %B}" k k2 v;;
;;
end;;


(* A map module from int pair keys to bool values. *)
module IntpairBoolMap = Treemap.Make(IntpairBoolKV);;
