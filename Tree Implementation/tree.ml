type 'a tree =
  | Node of 'a tree * 'a * 'a tree
  | Leaf

(* Given In Discussion *) 

let rec tree_fold f b t =
  match t with
  | Leaf -> b
  | Node (l, v, r) -> let res_l = tree_fold f b l in
      let res_r = tree_fold f b r in
      f res_l v res_r

let map tree f = 
  let funcCall l v r = Node (l, f v, r) in 
  tree_fold funcCall Leaf tree

let mirror tree = 
  let flip l v r = Node(r, v, l) in 
  tree_fold flip Leaf tree

let in_order tree =
  let inorder l v r = l @ [v] @ r in 
  tree_fold inorder [] tree

let pre_order tree = 
  let preorder l v r = [v] @ l @ r in
  tree_fold preorder [] tree
  (*let preorder l v r = Node(v,Leaf,Leaf) :: l @ r
  tree_fold preorder [] tree *)

let depth tree = 
  let depthSearch l v r = 
    if l = 0 && r = 0 then 1 
    else 1 + max l r 
  in 
  tree_fold depthSearch 0 tree

let compose tree = 
  let compose2 l v r =
    (fun x -> r(v(l x)))
  in
  tree_fold  compose2 (fun x-> x) tree 

let trim tree n =  
  let trimFunc (x,l) v (y,r) = 
    if x >= (depth tree - n) then (x+1,Node(l,v,r))
    else (x+1,Leaf)
  in 
  match tree_fold (trimFunc) (0, Leaf) tree with (a,b) -> b

let rec trim tree n =
  match tree with
    | Leaf -> Leaf
    | Node (l, v, r) ->
      let depth_l = depth l in
      let depth_r = depth r in
      if depth_l >= (depth tree - n) then
        Node(trim l n, v, r)
      else if depth_r >= (depth tree - n) then
        Node(l, v, trim r n)
      else
        Leaf

(*    Node           3         level =  1
      /  \                       
    Node   Node      2                     
    /         \             
  Leaf        Leaf    1
*)

let tree_init f v =
  let rec aux v = 
    match f v with 
    |None -> Leaf
    |Some (v1, v2, v3) -> Node (aux v1, v2, aux v3)
  in 
  aux v 

let find lst v = 
  let rec aux lst v index = 
    match lst with 
    |[] -> -1
    | h :: t ->
      if h == v then index else aux t v index+1
  in
  aux lst v 0

let reverse lst = 
  let rec helper ls acc = 
    match ls with
    |[] -> acc 
    |h::t -> helper t (h::acc)
  in helper lst []

let split lst v =
  let rec aux left right = function (*Anonymous function *)
    | [] -> (reverse left, [])
    | x::xs ->
        if x = v then (reverse left, xs)
          else aux (x :: left) right xs
    in
    aux [] [] lst;;


let rec bound rl rest = 
  let contains = fun x -> List.exists (fun y -> x = y) rl in
  match rest with 
  | [] -> []
  | h :: t -> if contains h then h::(bound rl t) else bound rl t;;

let rec from_pre_in pre in_ord =
  match pre with
  | [] -> Leaf 
  | root :: rest ->
    let left, right = split in_ord root in
    let left_tree = from_pre_in (bound left rest) left in
    let right_tree = from_pre_in (bound right rest) right in
    Node(left_tree, root, right_tree)
