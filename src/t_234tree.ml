

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
      
    val bt_isempty : 'a t_234tree -> bool

    val t234_search : 'a  * 'a t_234tree -> bool (*  Question 4 *)
      
    (* Question 6 *)
    val bt_add : 'a * 'a t_234tree -> 'a t_234tree  

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

  
    let rec bt_add(a, t : 'a * 'a t_234tree) : 'a t_234tree =
      let rec add_aux t =
        match t with
        | Empty -> Rooted_1(a, Empty, Empty)
        | Rooted_1(x, l, r) ->
          if a < x then 
            (match l with
            | Empty -> Rooted_2(a, x, Empty, Empty, Empty)
            | Rooted_3(x1, x2, x3, l2, m2, n2, r2) -> 
                if a < x2 then Rooted_2(x2, x, bt_add(a,Rooted_1(x1,l2,m2)), Rooted_1(x2,n2,r2), r)
                else Rooted_2(x2, x, Rooted_1(x1,l2,m2), bt_add(a,Rooted_1(x2,n2,r2)), r)
            | _ -> Rooted_1(x,bt_add(a,l),r))
            
            
          else 
            (match r with
            | Empty -> Rooted_2(x,a, Empty, Empty, Empty)
            | Rooted_3(x1, x2, x3, l2, m2, n2, r2) -> 
              if a < x2 then Rooted_2(x, x2, l, bt_add(a,Rooted_1(x1,l2,m2)), Rooted_1(x3,n2,r2))
              else Rooted_2(x, x2, l, Rooted_1(x1,l2,m2), bt_add(a,Rooted_1(x3,n2,r2)))
            
            | _ -> Rooted_1(x,l,bt_add(a,r)))
          
        | Rooted_2(x1, x2, l, m, r) ->
          if a < x1 then 
            (match l with
              | Empty -> Rooted_3(a,x1,x2,Empty,Empty,Empty,Empty)
              | Rooted_3(x12, x22, x32, l2, m2, n2, r2) ->
                if a < x22 then Rooted_3(x22,x1,x2,bt_add(a,Rooted_1(x12,l2,m2)), Rooted_1(x32,n2,r2),m,r)
                else Rooted_3(x22, x1,x2, Rooted_1(x1,l2,m2), bt_add(a,Rooted_1(x32,n2,r2)),m,r)
              | _ -> Rooted_2(x1, x2, bt_add(a,l), m, r))
            
          else if a < x2 then
            (match m with
              | Empty -> Rooted_3(x1,a,x2,Empty,Empty,Empty,Empty)
              | Rooted_3(x12, x22, x32, l2, m2, n2, r2) ->
                if a < x22 then Rooted_3(x1,x22,x2,l,bt_add(a,Rooted_1(x12,l2,m2)), Rooted_1(x32,n2,r2),r)
                else Rooted_3(x1,x22,x2,l,Rooted_1(x1,l2,m2), bt_add(a,Rooted_1(x32,n2,r2)),r)
              | _ -> Rooted_2(x1,x2,l,bt_add(a, m), r))
            
          else 
            (match r with
              | Empty -> Rooted_3(x1,x2,a,Empty,Empty,Empty,Empty)
              | Rooted_3(x12, x22, x32, l2, m2, n2, r2) ->
                if a < x2 then Rooted_3(x1,x2,x22,l,m,bt_add(a,Rooted_1(x12,l2,m2)), Rooted_1(x32,n2,r2))
                else Rooted_3(x1,x2,x22,l,m,Rooted_1(x1,l2,m2), bt_add(a,Rooted_1(x32,n2,r2)))
              | _ -> Rooted_2(x1,x2,l,m,bt_add(a, r)))
            
        | Rooted_3(x1, x2, x3, l, m, n, r) ->
          if a < x2 then Rooted_1(x2,bt_add(a,Rooted_1(x1,l,m)),Rooted_1(x3,n,r))
          else Rooted_1(x2,Rooted_1(x1,l,m),bt_add(a,Rooted_1(x3,n,r)))
          in add_aux t
    ;;

  end
;;

open BtSum;;

(* Question 3 : représentation des arbres de la question 1 *)
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
    
    (* Test question 4 *)
    t234_search(10, a1);; (*true*)
    t234_search(10, a2);; (*false*)
    t234_search(35, a3);; (*true*)
    
    
    (* test Question 6 *)
    let test : int t_234tree ref =  ref(bt_add(6,bt_add(11,bt_add(20,bt_add(42,rooted_2(5,15,bt_empty(),bt_empty(),bt_empty()))))));;
    t234_search(40, !test);; (*false*)

    (* Question 7 *)
    test := bt_add(4, !test);;
    test := bt_add(35, !test);;
    test := bt_add(10, !test);;
    test := bt_add(13, !test);;
    test := bt_add(3, !test);;
    test := bt_add(30, !test);;
    test := bt_add(15, !test);;
    test := bt_add(12, !test);;
    test := bt_add(7, !test);;
    test := bt_add(40, !test);;
    test := bt_add(20, !test);;
    test := bt_add(11, !test);;
    test := bt_add(6, !test);;

    t234_search(40, !test);;
    t234_search(4, !test);;
    t234_search(35, !test);;
    t234_search(10, !test);;
    t234_search(3, !test);;

    (* Arbres rouge noir : Question 2 *)
module type RBT_SIG =
  sig
    type color
    type 'a t_rbtree

    val black : unit -> color 
     
    val red : unit -> color

    val node : color * 'a * 'a t_rbtree * 'a t_rbtree -> 'a t_rbtree

    val rbt_empty : unit -> 'a t_rbtree
      
    val rbt_root : 'a t_rbtree -> 'a 
      
    val rbt_isempty : 'a t_rbtree -> bool

    val rbt_search : 'a  * 'a t_rbtree -> bool 

    val rbt_balance : color * 'a * 'a t_rbtree * 'a t_rbtree -> 'a t_rbtree (*Question 3*)
      
    (* Question 6 *)
    val t_rbtree_insert : 'a * 'a t_rbtree -> 'a t_rbtree  

  end
  
;;

module RBtSum : RBT_SIG =
  struct
  type color = Red | Black
  type 'a t_rbtree = Leaf | Node of color * 'a * 'a t_rbtree * 'a t_rbtree

  let black() = Black

  let red() = Red

  let node(c, v, l, r: color * 'a* 'a t_rbtree * 'a t_rbtree) = Node(c, v, l, r)
    
    let rbt_empty() : 'a t_rbtree =
    Leaf
    ;;

    let rbt_root(bt1 : 'a t_rbtree) : 'a =
      match bt1 with
      |Leaf -> failwith("Empty tree")
      |Node(c,a, l, r) -> a 
    ;;

    
    let rbt_isempty(bt1 : 'a t_rbtree) : bool =
      match bt1 with
      |Leaf -> true
      |_ -> false
    ;;

    let rbt_search(e, t: 'a * 'a t_rbtree) : bool =
      let rec search_aux t = 
        match t with
        | Leaf -> false
        | Node(c, v, l, r) -> if e == v then true else if e < v then search_aux l else search_aux r
      in search_aux t
    ;; 


(* Les 4 cas dans lesquels on doit balance:
B pour Black et R pour Red
           Bz            Bz            Bx            Bx
          / \           / \           / \           / \
         Ry  d         Rx  d         a   Rz        a   Ry
       /  \           / \               /  \          /  \
      Rx   c        a   Ry           Ry   d         b    Rz
    /  \              /  \          / \                 /  \
  a    b             b    c       b   c               c    d 
    *)

    (* Ce que on doit toujours obtenir a la fin:
               Ry
              /  \
            Bx    Bz
          / \    / \
        a    b  c   d   
    *)

    let rbt_balance(c, v, l, r : color * 'a * 'a t_rbtree * 'a t_rbtree ) : 'a t_rbtree =
      let balance_aux = function
      | Black, z, Node (Red, y, Node (Red, x, a, b), c), d
      | Black, z, Node (Red, x, a, Node (Red, y, b, c)), d
      | Black, x, a, Node (Red, z, Node (Red, y, b, c), d)
      | Black, x, a, Node (Red, y, b, Node (Red, z, c, d)) ->
        Node (Red, y, Node (Black, x, a, b), Node (Black, z, c, d))
      | a, b, c, d -> Node (a, b, c, d)
      in balance_aux(c, v, l, r)
    ;;
  

   
    let rec t_rbtree_insert(a, t : 'a * 'a t_rbtree) : 'a t_rbtree =
      let rec add_aux t =
        match t with
        | Leaf -> Node(Red, a, Leaf, Leaf)
        | Node(c, v, l, r) as n->
          if a < v then rbt_balance (c, v, add_aux l, r)
          else if a > v then rbt_balance (c, v, l, add_aux r)
          else n
      in match add_aux t with
      | Node (_, v,l, r) -> Node (Black, v, l, r)
      | Leaf -> failwith "impossible d'insérer la valeur"
    ;;
  end
;;

open RBtSum;;
(* Question 2.2 *)
 let b1 : int t_rbtree = node(black(), 20, 
                            node(black(), 10, 
                                node(red(), 5, rbt_empty(), rbt_empty()), 
                                node(red(), 15, rbt_empty(), rbt_empty())), 
                            node(red(), 30,
                                node(black(), 25, rbt_empty(), rbt_empty()),
                                node(black(), 35, rbt_empty(), rbt_empty())));;

  let b2 : int t_rbtree = node(black(), 10, 
                                node(black(), 7, 
                                    node(red(), 2, rbt_empty(), rbt_empty()), 
                                    rbt_empty()), 
                                node(black(), 13,
                                    rbt_empty(),
                                    node(red(), 15, rbt_empty(), rbt_empty())));;

  let b3 : int t_rbtree = node(black(), 10, 
                                node(black(), 5, 
                                    rbt_empty(), 
                                    rbt_empty()), 
                                node(red(), 15,
                                    node(black(), 13, rbt_empty(), rbt_empty()),
                                    node(black(), 20, rbt_empty(), rbt_empty())));;

(* test Question 4 *)
let test2 : int t_rbtree ref =  ref(node(black(), 10, 
                                        node(black(), 5, 
                                            rbt_empty(), 
                                            rbt_empty()), 
                                        node(red(), 15,
                                            node(black(), 13, rbt_empty(), rbt_empty()),
                                            node(black(), 20, rbt_empty(), rbt_empty()))));;
rbt_search(40, !test2);; (*false*)

    (* Question 5 *)
    test2 := t_rbtree_insert(4, !test2);;
    test2 := t_rbtree_insert(35, !test2);;
    test2 := t_rbtree_insert(10, !test2);;
    test2 := t_rbtree_insert(13, !test2);;
    test2 := t_rbtree_insert(3, !test2);;
    test2 := t_rbtree_insert(30, !test2);;
    test2 := t_rbtree_insert(15, !test2);;
    test2 := t_rbtree_insert(12, !test2);;
    test2 := t_rbtree_insert(7, !test2);;
    test2 := t_rbtree_insert(40, !test2);;
    test2 := t_rbtree_insert(20, !test2);;
    test2 := t_rbtree_insert(11, !test2);;
    test2 := t_rbtree_insert(6, !test2);;

    rbt_search(40, !test2);;
    rbt_search(4, !test2);;
    rbt_search(35, !test2);;
    rbt_search(10, !test2);;
    rbt_search(3, !test2);;