let array_rev arr =
    let len = Array.length arr in                    (* length of input array *)
    let count = ref 1 in                             (*  counter to subtract from len to swap elements *)
      if len mod 2 = 0 then                          (* checking for even length. OCaml takes the floor of integer division, which must be accounted for. If it were the ceiling, it'd be fine and both would be len/2 - 1 *)
      (
        for i = 0 to len/2 - 1 do                    (* swapping loop. Makes 2 temps for the two elements to be swapped, then swaps them in place in the array. *)
            let temp1 = arr.(i) in
            let temp2 = arr.(len - !count) in
            arr.(i) <- temp2;
            arr.(len - !count) <- temp1;
            count := !count + 1                      (* increment count to prepare for next swap *)
        done;
        arr
      )
      else                                           (* same as above, except for odd array lengths  *)
      (
        for i = 0 to (len/2) do
          let temp1 = arr.(i) in
          let temp2 = arr.(len - !count) in
          arr.(i) <- temp2;
          arr.(len - !count) <- temp1;
          count := !count + 1
        done;
        arr
      )
 ;;
(* val array_rev : 'a array -> unit

   Reverses the given array in place. Uses iteration and mutation to
   do so efficiently. DOES NOT generate any internal copies of the
   parameter array.

   REPL EXAMPLES:
   # let a1 = [|1; 2; 3;|];;
   val a1 : int array = [|1; 2; 3|]
   # array_rev a1;;
   - : unit = ()
   # a1;;
   - : int array = [|3; 2; 1|]
   # let a2 = [|"a"; "b"; "c"; "d"; "e"; "f"|];;
   val a2 : string array = [|"a"; "b"; "c"; "d"; "e"; "f"|]
   # array_rev a2;;
   - : unit = ()
   # a2;;
   - : string array = [|"f"; "e"; "d"; "c"; "b"; "a"|]
   # let a3 = [|true; true; false; false; true;|];;
   val a3 : bool array = [|true; true; false; false; true|]
   # array_rev a3;;
   - : unit = ()
   # a3;;
   - : bool array = [|true; false; false; true; true|]
*)

let list_rev lst =
      let rec helper old newl =                      (* recursive helper function for reversal *)
        if old = [] then                             (* empty list check, will return the new list once the old has been recursed through *)
            newl
        else
            let final = (List.hd old) :: newl in     (* appends the head of the old list onto the new list, which will reverse it as it recurses *)
            helper (List.tl old) final               (* recurse on the tail of the original list *)
    in
    helper lst []                                    (* original call of the helper function, with an empty list in the newlist slot to be appended onto *)
 ;;
(* val list_rev : 'a list -> 'a list

   Return a reversed copy of the given list. Does not (and cannot)
   modify the original list. Uses an internal recursive function to
   build the reversed list. The internal function is tail-recursive.

   REPL EXAMPLES:
   # list_rev lst1;;
   - : int list = [3; 2; 1]
   # lst1;;
   - : int list = [1; 2; 3]
   # let lst2 = ["a"; "b"; "c"; "d"; "e"; "f"];;
   val lst2 : string list = ["a"; "b"; "c"; "d"; "e"; "f"]
   # list_rev lst2;;
   - : string list = ["f"; "e"; "d"; "c"; "b"; "a"]
   # lst2;;
   - : string list = ["a"; "b"; "c"; "d"; "e"; "f"]
   # let lst3 = [true; true; false; false; true];;
   val lst3 : bool list = [true; true; false; false; true]
   # list_rev lst3;;
   - : bool list = [true; false; false; true; true]
   # lst3;;
   - : bool list = [true; true; false; false; true]
*)
