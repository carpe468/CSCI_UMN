let array_above thresh arr =
    let len = Array.length arr in                     (* length of input array *)
    if len = 0 then                                   (* returns original array if it is empty *)
      arr
    else
    let newlen = ref 0 in                             (* ref for length of new array to be incremented as elements surpass the thresh *)
      for i = 0 to (len - 1) do                       (* loop to count and check if elements surpass the tresh. If they do, increments newlen by one for building the new array. *)
        if arr.(i) > thresh then
          newlen := !newlen + 1
          done;
          let newarr = Array.make !newlen thresh in   (* creation of new array of length newlen containing the thresh to match types with the input array *)
          let count = ref 0 in                        (* ref count to help move to the next index of the new array once the current one has been filled *)
          for k = 0 to len-1 do                       (* loop to check for elements that surpass the thresh. If they do, they are placed inside the new array and count is incrememnted by one. *)
            if arr.(k) > thresh then
            begin
              newarr.(!count) <- arr.(k);
              count := !count + 1;
            end
          done;
    newarr                                            (* return of the new array containing only elements that surpass the thresh *)
 ;;
(* val array_above : 'a -> 'a array -> 'a array

   Creates a new array which has only elements which are greater than
   parameter thresh in it. Elements from arr that are larger than
   thresh appear in the same order the return array as they do in arr.
   Uses two passes to count the elements above in arr, allocates
   another array of appropriate size, and then copies in elements from
   arr.  Does not modify the original array arr.

   REPL EXAMPLES:
   # array_above 0 [|0; 1; 2; 0|];;
   - : int array = [|1; 2|]
   # array_above 0 [|4; -2; -1; 7; 0; 3|];;
   - : int array = [|4; 7; 3|]
   # array_above 3 [|4; -2; -1; 7; 0; 3|];;
   - : int array = [|4; 7|]
   # array_above 1.5 [|4.2; 0.5; 1.2; 7.6; 8.9; 0.8; 8.5|];;
   - : float array = [|4.2; 7.6; 8.9; 8.5|]
   # array_above 0.0 [|4.2; 0.5; 1.2; 7.6; 8.9; 0.8; 8.5|];;
   - : float array = [|4.2; 0.5; 1.2; 7.6; 8.9; 0.8; 8.5|]
   # array_above 9.0 [|4.2; 0.5; 1.2; 7.6; 8.9; 0.8; 8.5|];;
   - : float array = [||]
   # array_above false [|false; true; false; true; true;|];;
   - : bool array = [|true; true; true|]
*)

let rec list_above thresh lst =
    if lst = [] then                                                    (* if the list is empty it returns [] *)
      []
    else if (List.hd lst) > thresh then                                 (* if the head element surpasses the thresh, it is appended to the recursive call of the tail  *)
      let final = (List.hd lst) :: list_above thresh (List.tl lst) in
      final
    else
      let final = list_above thresh (List.tl lst) in                     (* if the element does not surpass the thresh, the function recurses without appending an element *)
      final
 ;;
(* val list_above : 'a -> 'a list -> 'a list

   Create a list which has only elements from lst that are larger than
   thresh.  Uses recursion to accomplish this in a single pass over
   the original list. Does not modify the original list lst.

   REPL EXAMPLES:
   # list_above 0 [0; 1; 2];;
   - : int list = [1; 2]
   # list_above 0 [0; 1; 2; 0];;
   - : int list = [1; 2]
   # list_above 0 [4; -2; -1; 7; 0; 3];;
   - : int list = [4; 7; 3]
   # list_above 3 [4; -2; -1; 7; 0; 3];;
   - : int list = [4; 7]
   # list_above 1.5 [4.2; 0.5; 1.2; 7.6; 8.9; 0.8; 8.5];;
   - : float list = [4.2; 7.6; 8.9; 8.5]
   # list_above 0.0 [4.2; 0.5; 1.2; 7.6; 8.9; 0.8; 8.5];;
   - : float list = [4.2; 0.5; 1.2; 7.6; 8.9; 0.8; 8.5]
   # list_above 9.0 [4.2; 0.5; 1.2; 7.6; 8.9; 0.8; 8.5];;
   - : float list = []
   # list_above false [false; true; false; true; true;];;
   - : bool list = [true; true; true]
*)
