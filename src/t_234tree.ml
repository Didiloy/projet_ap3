

(** Question 3 **)

module type Bt_SIG =
  sig
    type 'a t_234tree

    val bt_empty : unit -> 'a t_234tree
      
    val bt_add : 'a * 'a t_234tree -> 'a t_234tree
      
    val bt_root : 'a t_234tree -> 'a list

    val bt_sub : 'a -> 'a t_234tree
      
    val bt_isempty : 'a t_234tree -> bool
      
  end
  
;;

module BtSum : Bt_SIG =
  struct
    type 'a t_234tree = Empty
                      | Rooted of 'a list * 'a t_234tree list ;;
    
    let bt_empty() : 'a t_234tree =
      Empty
    ;;

    let bt_add(a, tree : 'a *'a t_234tree) : 'a t_234tree =
      (* if 234t == Empty then Rooted('a list = [a];'a t_234tree list = []) *)
        if tree == Empty then Rooted([a], [])
          (* TODO *)
      else failwith("TODO")
    ;;

    let bt_root(bt1 : 'a t_234tree) : 'a list =
      match bt1 with
      |Empty -> failwith("err")
      (* TODO *)
      |Rooted(a,_) -> a
    ;;

    let bt_subleft(bt1 : 'a t_234tree) : 'a t_234tree =
      match bt1 with
      |Empty -> failwith("err")
      (* TODO *)
      |Rooted(_,left) -> left
    ;;

    let bt_subright(bt1 : 'a t_234tree) : 'a t_234tree =
      match bt1 with
      |Empty -> failwith("err")
      (* TODO *)
      |Rooted(_,right) -> right
    ;;

    let bt_isempty(bt1 : 'a t_234tree) : bool =
      match bt1 with
      |Empty -> true
      |_ -> false
    ;;
    
  end
;;
