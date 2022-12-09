

(** Question 3 
    1)
    **)

module type Bt_SIG =
  sig
    type 'a t_234tree

    val bt_empty : unit -> 'a t_234tree
      
    (* val bt_add : 'a * 'a t_234tree -> 'a t_234tree   Pas de add vu que c'est la question 6*)
      
    val bt_root : 'a t_234tree -> 'a list

    val bt_sub : 'a -> 'a t_234tree
      
    val bt_isempty : 'a t_234tree -> bool
      
  end
  
;;

module BtSum : Bt_SIG =
  struct
    type 'a t_234tree = Empty
                      | Rooted_1 of 'a * 'a t_234tree * 'a t_234tree
                      | Rooted_2 of 'a * 'a *  'a t_234tree * 'a t_234tree * 'a t_234tree
                      | Rooted_3 of 'a * 'a * 'a * 'a t_234tree * 'a t_234tree * 'a t_234tree * 'a t_234tree ;;
    
    let bt_empty() : 'a t_234tree =
      Empty
    ;;

    (* let bt_add(a, tree : 'a *'a t_234tree) : 'a t_234tree =
        if tree == Empty then Rooted_1(a, Empty, Empty)
          (* TODO *)
      else failwith("TODO")
    ;; *)

    let bt_root(bt1 : 'a t_234tree) : 'a list =
      match bt1 with
      |Empty -> failwith("Empty tree")
      |Rooted_1(a,_, _) -> [a]
      |Rooted_2(a,a2,_,_,_) -> [a; a2]
      |Rooted_3(a,a2,a3,_, _,_,_) -> [a; a2; a3]
    ;;

    let bt_sub(a : 'a) : 'a t_234tree =
    match a with
    |_ -> failwith("err")
  ;;
    let bt_isempty(bt1 : 'a t_234tree) : bool =
      match bt1 with
      |Empty -> true
      |_ -> false
    ;;
    
  end
;;
