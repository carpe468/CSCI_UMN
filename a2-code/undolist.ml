(* undolist.ml : This module manages a sorted list strings. add,
   remove, and merge operations are provided. undo/redo operations are
   provided to alter list. Type annotaions are required on the
   module-level values as refs to 'a list are not allowed due to weak
   typing.

   All functions in this file pertain to PROBLEM 3
*)

let curr_list : string list ref = ref []  ;;                      (* initializes as a ref array *)
(* The current list of strings. *)

let undo_stack : string list list ref = ref [] ;;                 (* initializes as a ref array *)
(* List of previous curr_lists to enable undo. *)

let redo_stack : string list list ref = ref [] ;;                 (* initializes as a ref array *)
(* List of undone curr_lists to enable redo. *)

let reset_all () =                                          (* sets all three ref lists to empty *)
  curr_list := [];
  undo_stack := [];
  redo_stack := [];
;;
(* Reset curr_list, undo_stack, redo_stack to all be empty lists. Used
   only in testing, not in main programs. *)

let set_to_list new_list =                                        (* Sets a new list to curr_list while appending the previous one to the undo stack. *)
  undo_stack := !curr_list :: !undo_stack;
  curr_list := new_list;
  redo_stack := [];                                               (* Empties the renew stack (sets it to an empty list) *)
;;
(* curr_list is moved to the top of the undo_Stack. Then curr_list is
   set to the new_list. Empties redo_stack. *)

let add_elem elem =                                               (* calls insert from sortedlist.ml to add an element to the curr_list *)
set_to_list (Sortedlist.insert !curr_list elem)
;;
(* Add the given elem to curr_list producing a new_list.  Calls
   set_to_list on result to unable undoing. *)

let remove_elem elem =
set_to_list (Sortedlist.remove !curr_list elem)                   (* calls remove from Sortedlist.ml to remove an element from curr_list *)
;;
(* Remove the given elem from curr_list producing a new list. Calls
   set_to_list on result to unable undoing.  *)

let merge_with_list list =                                        (* calls merge from Sortedlist.ml using curr_list and a given list *)
set_to_list (Sortedlist.merge !curr_list list)
;;
(* Merge the param list with the current list. Calls set_to_list on
   the result to enable undoing. *)

let undo () =                                                     (* appends curr_list to the redo stack and sets curr_list to the head of undo_stack *)
if !undo_stack = [] then                                          (* removes head from undo_stack. returns true if there was an undo to perform, otherwise false. *)
  false
else
  begin
    redo_stack := !curr_list :: !redo_stack;
    curr_list := (List.hd !undo_stack);
    undo_stack := (List.tl !undo_stack);
    true
  end;
;;
(* If the undo_stack is not empty, undo the last operation. curr_list
   is moved to the redo_stack and the top element of undo_stack is
   removed and becomes curr_list.  Returns true if changes are made
   and false if undo_stack is empty and no changes are made. Operates
   in constant time. *)

let redo () =                                                     (* appends curr_list to the undo stack. Pops the head off of the redo_stack and sets *)
  if !redo_stack = [] then                                        (* curr_list to it. Returns true if redo was able to perform, otherwise false. *)
    false
  else
    begin
      undo_stack := !curr_list :: !undo_stack;
      curr_list := (List.hd !redo_stack);
      redo_stack := (List.tl !redo_stack);
      true
    end;
;;
(* If the redo_stack is not empty, redo the last operation. curr_list
   is moved to the undo_stack and the top element of redo_stack is
   removed and becomes curr_list.  Returns true if changes are made
   and false if redo_stack is empty and no changes are made. Operates
   in constant time. *)
