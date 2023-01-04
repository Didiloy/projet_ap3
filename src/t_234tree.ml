

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
      
    val bt_root : 'a t_234tree -> 'a list

    val bt_sub : 'a -> 'a t_234tree
      
    val bt_isempty : 'a t_234tree -> bool

    val t234_search : 'a  * 'a t_234tree -> bool (*  Question 4 *)
      
    val bt_add : 'a * 'a t_234tree -> 'a t_234tree  (* Question 6 *)
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

    let t234_search(e, t: 'a * 'a t_234tree) : bool =
      let rec search_aux t = 
        match t with
        | Empty -> false
        | Rooted_1(v, l, r) -> if e == v then true else if e < v then search_aux l else search_aux r
        | Rooted_2(v, v2, l, m, r) ->
            if e == v || e == v2 then true
            else if e < v then search_aux l
            else if e > v2 then search_aux r
            else search_aux m
        | Rooted_3(v, v2, v3, l, m, n, r) ->
          if e == v || e == v2 || e == v3 then true
          else if e < v then search_aux l
          else if e > v3 then search_aux r
          else if e > v && e > v2 then search_aux m
          else search_aux n
      in search_aux t
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
      let rec bt_add(a, t : 'a * 'a t_234tree) : 'a t_234tree =
        let rec add_aux t =
          match t with
          | Empty -> Rooted_1(a, Empty, Empty)
          | Rooted_1(x, l, r) ->
            if a < x then Rooted_1(x,bt_add(a, l), r)
            else Rooted_1(x,l,bt_add(a,r))
            
          | Rooted_2(x1, x2, l, m, r) ->
            if a < x1 then Rooted_2(x1,x2,bt_add(a, l),m, r)
              
            else if a < x2 then Rooted_2(x1,x2,l,bt_add(a, m), r)
              
            else Rooted_2(x1,x2,l,m,bt_add(a, r))
              
          | Rooted_3(x1, x2, x3, l, m, n, r) ->
            if a < x1 then Rooted_3(x1, x2, x3, bt_add(a,l), m, n, r)
              
            else if a < x2 then Rooted_3(x1, x2, x3, l, bt_add(a,m), n, r)
            else if a < x3 then Rooted_3(x1, x2, x3, l, m, bt_add(a,n), r)
            else Rooted_3(x1, x2, x3, l, m, n, bt_add(a,r))
            in add_aux t
      ;;
  end
;;

(* Question 3 : représentation des arbres de la question 1 *)
open BtSum;;


(* test Question 6 *)

let test : int t_234tree = bt_add(6,bt_add(11,bt_add(20,bt_add(40,rooted_2(5,15,bt_empty(),bt_empty(),bt_empty())))));;

test;;



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

t234_search(10, a1);; (*true*)
t234_search(10, a2);; (*false*)
t234_search(35, a3);; (*true*)
