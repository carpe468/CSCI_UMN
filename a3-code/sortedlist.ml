(* sortedlist.ml : Provides operations for sorted lists of any type. *)
open Printf;;
let rec insert list elem =                                            (* insert function. Matches lists to decide whether or not to further *)
  match list with                                                     (* recurse on the list or add the element in when it is less than the head *)
  | [] -> [elem]                                                      (* has a base case for an empty list and stops recursion when once the element *)
  | head :: tail when elem > head -> head :: (insert tail elem)       (* has been inserted. Has a failsafe in place with final case of match. *)
  | head :: tail when elem = head -> list
  | head :: tail when elem < head -> elem :: list
  | _ -> failwith "out of bounds"
;;
(* val insert : 'a list -> 'a -> 'a list
   PROBLEM 1: Insert elem into list which is sorted.  The insertion
   preserves the sorted order of the list.  No duplicates are allowed
   in the list: if elem is equal to an element of list, the resulting
   list is identical to the original list. Uses pattern matching, not
   tail recursive. Runs in linear time on length of list.



  REPL EXAMPLES
  # insert [1;3;5;7] 8;;
  - : int list = [1; 3; 5; 7; 8]
  # insert [1;3;5;7] 2;;
  - : int list = [1; 2; 3; 5; 7]
  # insert [1;3;5;7] 5;;
  - : int list = [1; 3; 5; 7]
  # insert ["b";"d";"f"] "a";;
  - : string list = ["a"; "b"; "d"; "f"]
  # insert ["b";"d";"f"] "g";;
  - : string list = ["b"; "d"; "f"; "g"]
  # insert ["b";"d";"f"] "b";;
  - : string list = ["d"; "f"]
  # insert [] "g";;
  - : string list = ["g"]
*)

let rec remove list elem =                                            (* Removes an element from a given list. Removes the element once elem = head of *)
  match list with                                                     (* the list being checked. has a base case of an empty list and stops recursion once the  *)
  | [] -> []                                                          (* element has been removed. Has a failsafe for last match case. *)
  | head :: tail when elem > head -> head :: (remove tail elem)
  | head :: tail when elem = head -> remove tail elem
  | head :: tail when elem < head -> list
  | _ -> failwith "out of bounds"

;;
(* val remove : 'a list -> 'a -> 'a list
   PROBLEM 1: Create a new list with elem removed from the parameter
   list. If elem is not present in list, the result is identical to
   the original list. Uses pattern matching, not tail recursive.
   Runs in linear time on length of list.



   REPL EXAMPLES
   # remove [1;3;5;7] 1;;
   - : int list = [3; 5; 7]
   # remove [1;3;5;7] 5;;
   - : int list = [1; 3; 7]
   # remove [1;3;5;7] 6;;
   - : int list = [1; 3; 5; 7]
   # remove ["b";"d";"f"] "b";;
   - : string list = ["d"; "f"]
   # remove ["b";"d";"f"] "z";;
   - : string list = ["b"; "d"; "f"]
*)

let rec print strlist =                           (* Prints the entire given list. Sets newstring to the head of  *)
if strlist != [] then                             (* the element through a match block. Tail recursive. I purposely commented out *)
  let newstring =                                 (* the failsafe in order to eliminate the warning during compilation, although I *)
    match strlist with                            (* left it in the code to show that I was accounting for it. *)
    | [] -> ""
    | head :: tail -> head;
(*    | _ -> failwith "out of bounds" *)
  in
  printf "%s \n" newstring;
  print (List.tl strlist)

;;
(* val print_strlist : string list -> unit
   PROBLEM 1: Print all elements of a string list to standard
   output. Makes use of standard printing functions such as
   print_endline or printf to print. Uses pattern matching. This
   function IS tail recursive. Runs in linear time on length of list.

   REPL EXAMPLES
   # print ["apple";"orange";"banana"];;
   apple
   orange
   banana
   - : unit = ()
   # print ["grape";"pear"];;
   grape
   pear
   - : unit = ()
*)

let rec merge lista listb =                                                   (* Merges two given lists, eliminating any duplicates. *)
  if lista = [] then                                                          (* Base case for empty lists of either given list. *)
    listb
  else if listb = [] then
    lista
  else
    let tup = (List.hd lista,List.tl lista,List.hd listb,List.tl listb) in    (* Split the lists into a tuple in order to effectively use a matching block *)
      match tup with
        | (a,b,c,d) when a > c -> c :: (merge (a::b) d)                       (* Three cases for sorting the elements while merging. once it finds the smallest *)
        | (a,b,c,d) when c > a -> a :: (merge b (c::d))                       (* element, it is appended and the recursed onto the untouched list and the tail of the *)
        | (a,b,c,d) when c = a -> a :: (merge b d)                            (* list the element was taken from. If both lists have the same element, it is appended once *)
        | _ -> failwith "out of bounds"                                       (* and the extra is discarded. Last match case is a failsafe. *)
;;
(* val merge : 'a list -> 'a list -> 'a list
   PROBLEM 2: Merge two sorted lists: lista and listb.  Elemetns that
   appear in both lists appear only once in the result.  Operates in
   linear time on the length of lists: does not do repeated
   insertion. Not tail recursive. May use pattern matching OR if/else
   clauses. Runs in linear time on combined length of lists.

   REPL EXAMPLES
   # merge [] [2;4;6];;
   - : int list = [2; 4; 6]
   # merge [1;3;5] [2;4;6];;
   - : int list = [1; 2; 3; 4; 5; 6]
   # merge [1;3;5] [];;
   - : int list = [1; 3; 5]
   # merge [1;3;5] [2;3;4;6;8];;
   - : int list = [1; 2; 3; 4; 5; 6; 8]
   # merge ["a";"c";"e"] ["b";"d"];;
   - : string list = ["a"; "b"; "c"; "d"; "e"]
   # merge ["a";"c";"e"] ["b";"c";"d";"e"];;
   - : string list = ["a"; "b"; "c"; "d"; "e"]
*)
