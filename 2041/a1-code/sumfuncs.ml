let array_sum arr =
    let len = Array.length arr in                     (* length of input array *)
    let sum = ref 0 in                                (* ref to be the sum of all elements of array *)
    for i = 0 to (len - 1) do                         (* for loop to traverse the array in order, adding each element to the ref sum *)
        sum := !sum + arr.(i);
    done;
    !sum                                              (* returning the value in sum *)
 ;;
(* val array_sum : int array -> int
   Return the sum of int array arr. Uses Array.length to calculate its
   length. Uses a ref to a summing int and a loop over the array
   elements.

   REPL EXAMPLES:
   # array_sum [|1; 3; 5|];;
   - : int = 9
   # array_sum [|4; -3; 12; 2|];;
   - : int = 15
   # array_sum [||];;
   - : int = 0
*)

let rec list_sum lst =
    if lst = [] then                                  (* if the list is empty, return [] *)
        0
    else
        List.hd lst + (list_sum (List.tl lst))        (* recursive call to add all elements contained in the list.  Will recurse down to the last element, and begin adding each element to it until the original head is reached *)
 ;;
(* val list_sum : int list -> int
   Return the sum of int list lst. Uses recursion and NO mutation.
   Uses List.hd and List.tl to get the head and tail of a list.

   REPL EXAMPLES:
   # list_sum [1; 3; 5];;
   - : int = 9
   # list_sum [4; -3; 12; 2];;
   - : int = 15
   # list_sum [];;
   - : int = 0
*)
