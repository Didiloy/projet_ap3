

(** Question 3 
    1)
    **)

module type Bt_SIG =
  sig
    type 'a t_234tree

    val rooted_1 : 'a * 'a t_234tree * 'a t_234tree -> 'a t_234tree
    val rooted_2 : 'a * 'a *  'a t_234tree * 'a t_234tree * 'a t_234tree -> 'a t_234tree
    val rooted_3 : 'a * 'a * 'a * 'a t_234tree * 'a t_234tree * 'a t_234tree * 'a t_234tree -> 'a t_234tree

    val bt_empty : unit -> 'a t_234tree
      
    (* val bt_add : 'a * 'a t_234tree -> 'a t_234tree   Pas de add vu que c'est la question 6*)
      
    val bt_root : 'a t_234tree -> 'a list

    val bt_sub : 'a -> 'a t_234tree
      
    val bt_isempty : 'a t_234tree -> bool
      
    (* val bt_add : 'a * 'a t_234tree -> 'a t_234tree *)
  end
  
;;

module BtSum : Bt_SIG =
  struct
    type 'a t_234tree = Empty
                      | Rooted_1 of 'a * 'a t_234tree * 'a t_234tree
                      | Rooted_2 of 'a * 'a *  'a t_234tree * 'a t_234tree * 'a t_234tree
                      | Rooted_3 of 'a * 'a * 'a * 'a t_234tree * 'a t_234tree * 'a t_234tree * 'a t_234tree ;;

    let rooted_1(v, l, r: 'a* 'a t_234tree * 'a t_234tree) = Rooted_1(v, l, r)
    let rooted_2(v,v2, l, m, r: 'a* 'a * 'a t_234tree * 'a t_234tree * 'a t_234tree) = Rooted_2(v, v2, l, m, r)
    let rooted_3(v,v2, v3, l, m, n, r: 'a * 'a * 'a * 'a t_234tree * 'a t_234tree * 'a t_234tree * 'a t_234tree) = Rooted_3(v, v2, v3, l, m, n, r)
    
    let bt_empty() : 'a t_234tree =
      Empty
    ;;

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
    
      (* Define an auxiliary function that takes a tree as input and returns a new tree with the value inserted.
         This function uses pattern matching to handle the different cases of the input tree:
         - If the input tree is empty, it returns a new tree rooted at a single node containing the value `a`.
         - If the input tree is a Rooted_1 node, it compares the value `a` with the value in the node. If `a` is
           smaller, it inserts `a` into the left subtree. If `a` is larger, it inserts `a` into the right subtree.
           If the subtree is unchanged after the insertion, it returns the original tree. Otherwise, it returns
           a new tree with the value inserted and the subtree replaced.
         - If the input tree is a Rooted_2 node, it compares the value `a` with the values in the node. If `a` is
           smaller than the smallest value, it inserts `a` into the left subtree. If `a` is larger than the largest
           value, it inserts `a` into the right subtree. If `a` is between the two values, it inserts `a` into the
           middle subtree. If the subtree is unchanged after the insertion, it returns the original tree. Otherwise,
           it returns a new tree with the value inserted and the subtree replaced.
         - If the input tree is a Rooted_3 node, it compares the value `a` with the values in the node. If `a` is
           smaller than the smallest value, it inserts `a` into the left subtree. If `a` is between the smallest and
           middle value, it inserts `a` into the middle subtree. If `a` is between the middle and largest value, it
           inserts `a` into the right subtree. If `a` is larger than the largest value, it inserts `a` into the right
           subtree. If the subtree is unchanged after the insertion, it returns the original tree. Otherwise, it
           returns a new tree with the value inserted and the subtree replaced.
      *)
      (* let bt_add(a, t : 'a * 'a t_234tree) : 'a t_234tree =
        let rec add_aux t =
          match t with
          | Empty -> Rooted_1(a, Empty, Empty)
          | Rooted_1(x, l, r) ->
            if a < x then
              let l' = add_aux l in
              if l' == l then t else Rooted_2(x, a, l', r, Empty)
            else
              let r' = add_aux r in
              if r' == r then t else Rooted_2(x, a, l, r', Empty)
          | Rooted_2(x1, x2, l, m, r) ->
            if a < x1 then
              let l' = add_aux l in
              if l' == l then t else Rooted_3(x1, x2, a, l', m, r, Empty)
            else if a < x2 then
              let m' = add_aux m in
              if m' == m then t else Rooted_3(x1, x2, a, l, m', r, Empty)
            else
              let r' = add_aux r in
              if r' == r then t else Rooted_3(x1, x2, a, l, m, r', Empty)
          | Rooted_3(x1, x2, x3, l, m, n, r) ->
            if a < x1 then
              let l' = add_aux l in
              if l' == l then t else Rooted_2(x1, x2, l', Rooted_2(x3, a, m, n, r), Empty)
            else if a < x2 then
              let m' = add_aux m in
              if m' == m then t else Rooted_2(x1, x2, Rooted_2(x1, a, l, m', Empty), Rooted_2(x3, x2, n, r, Empty))
            else if a < x3 then
              let n' = add_aux n in
              if n' == n then t else Rooted_2(x1, x2, Rooted_2(x1, x2, l, m, Empty), Rooted_2(x3, a, n', r, Empty))
            else
              let r' = add_aux r in
              if r' == r then t else Rooted_2(x1, x2, Rooted_2(x1, x2, l, m, n), Rooted_2(x3, a, r', Empty, Empty))
        in add_aux t
      ;; *)

  end
;;


(* Question 3 : représentation des arbres de la question 1 *)
open BtSum;;

let a1 : int t_234tree = rooted_2(
  10, 
  20, 
  rooted_3(1, 5, 6, bt_empty(), bt_empty(), bt_empty(), bt_empty()), 
  rooted_3(14, 18, 19, bt_empty(), bt_empty(), bt_empty(), bt_empty()), 
  rooted_2(23, 35, bt_empty(), bt_empty(), bt_empty())
);;

let a2 : int t_234tree = rooted_1(
  15, 
  rooted_3(1, 5, 6, bt_empty(), bt_empty(), bt_empty(), bt_empty()), 
  rooted_2(23, 35, bt_empty(), bt_empty(), bt_empty())
);;

let a3 : int t_234tree = rooted_2(
  11, 
  28, 
  rooted_1(1, bt_empty(), bt_empty()), 
  rooted_3(14, 18, 27, bt_empty(), bt_empty(), bt_empty(), bt_empty()), 
  rooted_2(33, 35, bt_empty(), bt_empty(), bt_empty())
);;