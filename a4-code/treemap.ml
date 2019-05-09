open Printf;;

module type KEYVAL_SIG =
sig
  type key_t
  type value_t
  val compare_keys : key_t -> key_t -> int
  val keyval_string : key_t -> value_t -> string
end



(* Functor which creates a module for maps with key/value types
   specified by the parameter module KVMod. *)
module Make (KVMod : KEYVAL_SIG) = struct


    type treemap =
      | Empty               (* no data: bottom of tree   *)
      | Node of {           (* node of anonymous record  *)
          key   : KVMod.key_t;   (* key for this node         *)
          value : KVMod.value_t;   (* value associated with key *)
          left  : treemap;    (* left branch               *)
          right : treemap;    (* right branch              *)
        }
    ;;


    let empty = Empty;;


    let rec add map key value =
      match map with
      | Empty ->                                             (* bottom of tree: didn't find *)
         Node{key=key; value=value;                          (* make a new node with key/val binding *)
              left=Empty; right=Empty}
      | Node(node) ->                                        (* at a node *)
         let diff = KVMod.compare_keys key node.key in           (* compute a difference *)
         if diff = 0 then                                    (* 0 indicates equal *)
           Node{node with value=value}                       (* replace value binding with new value *)
         else if diff < 0 then                               (* negative indicates str less than data *)
           Node{node with left=(add node.left key value)}    (* create a new node with new left branch *)
         else                                                (* positive indicates str greater than data *)
           Node{node with right=(add node.right key value)}  (* create new node with new right branch *)
    ;;


    let tree_string map =
      let buf = Buffer.create 256 in                    (* extensibel character buffer *)
      let rec build tree depth =                        (* recursive helper *)
        match tree with
        | Empty -> ()                                   (* out of tree, done with this branch *)
        | Node(node) ->                                 (* have a node *)
           build node.right (depth+1);                  (* recurse on right branch *)
           for i=1 to depth do                          (* indent according to depth of this node *)
             Buffer.add_string buf "  ";
           done;
           let datastr =                                (* string with depth and data  *)
             sprintf "%2d: %s\n" depth (KVMod.keyval_string node.key node.value)
           in
           Buffer.add_string buf datastr;               (* add to buffer *)
           build node.left (depth+1);                   (* recurse on left branch *)
      in                                                (* end helper *)
      build map 0;                                      (* recurse from root *)
      Buffer.contents buf                               (* return string from Buffer *)
    ;;


    let rec getopt map key =
      match map with
      | Node(node) ->
         let diff = KVMod.compare_keys key node.key in
         if diff = 0 then
           Some node.value
         else if diff < 0 then
            getopt node.left key
         else
            getopt node.right key
      | Empty -> None
    ;;


    let contains_key map str =
      let opt = (getopt map str) in
        match opt with
        | Some str -> true
        | None -> false

    ;;



    let rec iter func map =
      match map with
      | Empty ->  ()
      | Node(node) ->
          if (node.left != Empty) then
            iter func node.left;
            func node.key node.value;
          if (node.right != Empty) then
            iter func node.right;

    ;;


    let rec fold func cur map =
        match map with
        | Empty -> cur
        | Node(node) ->
          if (node.right != Empty && node.left != Empty) then
            let le = fold func cur node.left in
            let re = func le node.key node.value in
            fold func re node.right;
          else if (node.left != Empty) then
            let le = fold func cur node.left in
            func le node.key node.value;
          else if (node.right != Empty) then
            let re = func cur node.key node.value in
            fold func re node.right;
          else
            func cur node.key node.value;

    ;;


    let to_string map =
      let buf = Buffer.create 256 in
        let rec search map =
          match map with
          | Empty -> ()
          | Node(node) ->
            if (node.left != Empty) then
              search node.left;
            let toadd =
              KVMod.keyval_string node.key node.value in
              Buffer.add_string buf toadd;
            if (node.right != Empty) then
              search node.right;
      in
        search map;
        if (Buffer.length buf > 0) then
          Buffer.truncate buf ((Buffer.length buf) - 2);
        Buffer.add_string buf "]";
        let buff = Buffer.create 256 in
        let buffbeg = "[" in
        Buffer.add_string buff buffbeg;
        Buffer.add_buffer buff buf;
        Buffer.contents buff
    ;;


    let rec findmin_keyval map =
      match map with
      | Empty -> failwith ("No minimum in an empty tree");
      | Node(node) ->
        if (node.left != Empty) then
          findmin_keyval node.left
        else
          (node.key, node.value)
    ;;



    let rec remove_key map key =
      match map with
      | Empty -> map
      | Node(node) ->
         let diff = KVMod.compare_keys key node.key in
         if diff = 0 then
            if (node.right = Empty && node.left = Empty) then
              Empty
            else if (node.right = Empty && node.left != Empty) then
              node.left
            else if (node.right != Empty && node.left = Empty) then
              node.right
            else
            let pair = findmin_keyval node.right in
              Node{node with key = (fst (pair)); value = (snd (pair)); right = (remove_key node.right (fst(pair)))}
         else if diff < 0 then
           Node{node with left=(remove_key node.left key)}
         else
           Node{node with right=(remove_key node.right key)}
    ;;

end
