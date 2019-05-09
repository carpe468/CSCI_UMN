open List;;
open Printf;;
open Document;;

(* doccol.ml: Type and functions for a collection of named documents.
   Tracks a current document and its name along with an association
   list of all docs in the collection.  Preserves uniqueness of names
   in the collection. Makes use of built-in List functions to
   ad/remove/get docs from the association list. *)

(* Type to track a collection of named documents in an association
   list. *)
type 'a doccol = {
  mutable count   : int;                                  (* count of docs in list *)
  mutable curdoc  : 'a Document.document;                 (* current list being edited *)
  mutable curname : string;                               (* name of current list *)
  mutable docs    : (string * 'a Document.document) list; (* association list of names/docs *)
};;

let make name doc =                 (* creates a new doccol with given name/doc. sets count to 1 and assoc list to given pair only. *)
    {count = 1;
    curdoc = doc;
    curname = name;
    docs = [(name,doc)]}
;;
(* val make : string -> 'a Document.document -> 'a doccol
   Create a doccol. The parameters name and doc become the current
   doc and the only pair in the docs association list. *)

let add doccol name doc =                                     (* checks if name already exisits in doccol, if not then adds new name/doc to assoc list of doccol *)
    if ((List.assoc_opt name doccol.docs) = None) then
      begin
        doccol.docs <- ((name,doc)  :: doccol.docs);
        doccol.count <- doccol.count + 1;
        true
      end
    else
      false
;;
(* val add : 'a doccol -> string -> 'a Document.document -> bool
   If there is already a doc with name in doccol, do nothing and
   return false.  Otherwise, add the given doc to doccol with the
   given name, update the count of docs and return true. Uses
   association list functions from the List module. *)

let remove doccol name =                                        (* checks if name is curdoc, then checks if name is not in doccol.  *)
    if (name = doccol.curname) then                             (* if name is not curdoc and inside doccol, removes name/doc pair from doccol assoc list *)
        false
    else if ((List.assoc_opt name doccol.docs) = None) then
        false
    else
      begin
        doccol.docs <- List.remove_assoc name doccol.docs;
        doccol.count <- doccol.count - 1;
        true
      end
;;
(* val remove : 'a doccol -> string -> bool
   If name is equal to curname for the doccol, do nothing and return
   false.  If there is no doc with name in doccol, do nothing and
   return false.  Otherwise, remove the named doc from doccol,
   decrement the count of docs, and return true. Uses association list
   functions from the List module. *)

let has doccol name =                                           (* uses an option to return whether or not given name is in doccol *)
    if ((List.assoc_opt name doccol.docs) = None) then
        false
    else
        true
;;
(* val has : 'a doccol -> string -> bool
   Returns true if the named doc is in the doccol and false otherwise. *)

let switch doccol name =                                            (* checks if name is in doccol. if so, switches it with curdoc *)
    if ((List.assoc_opt name doccol.docs) = None) then
        false
    else
      begin
        doccol.curname <- name;
        doccol.curdoc <- (assoc name doccol.docs);
        true
      end
;;
(* val switch : 'a doccol -> string -> bool
   Change the current document/name to the named document and return
   true. If the named document does not exist, return false and make
   no changes to doccol. *)

let string_of_doccol doccol =                                           (* creates a formatted string of all names in assoc list *)
    let newlist = List.map (fun (a,b) -> "- "^a) doccol.docs in
    let returnstring = (Printf.sprintf "%d docs \n" doccol.count) ^ String.concat "\n" newlist in
    returnstring
;;
(* val string_of_col : 'a doccol -> string
   Creates a string representation of doccol showing the count of
   docs and the names of all docs. Each doc is listed on its own
   line. It has the following format:

   4 docs
   - test-dir/heros.txt
   - places.txt
   - stuff.txt
   - default.txt

   Does not define any helper functions. Makes use of higher order
   functions such as List.map and/or List.fold. May also use string
   processing functions such asString.concat and/or Printf.sprintf *)
