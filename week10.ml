
(* task1 *)

let(%) = fun f1 f2 x -> f1(f2 x)



let(@@) = fun f x -> f x

let(|>) = fun x f -> f x

(* task2 *)

module type Set = sig
  type t
  val to_string : t -> string
end

module StringSet : Set = struct
  type t = string
  let to_string s = "\"" ^ s ^ "\"" 
end

module type Map = sig
  type key
  type value
  type t
  val empty : t
  val set : key -> value -> t -> t
  val get : key -> t -> value
  val get_opt : key -> t -> value option
  val to_string : t -> string
end


module type OrderedSet = sig
  include Set
  
  val compare : t -> t -> int
end

(* module BTreeMap (K: OrderedSet) (V: Set):Map with type key = K.t and type value = V.t = struct 
  type key = K.t 
  type value = V.t 
  type t = Empty | Node of key * value * left * right
  let empty = Empty *)


  (* let rec set k v tree = match tree with
    | Empty -> Node(k, v, Empty, Empty)
    | Node(key, value, left, right) -> 
      let c = K.compare k key in 
      if c > 0 then Node(key, value, left, set k v right)
      else if c < 0  Node(key, value, set k v left, right)
      else Node(key, v, left, right) *)


(* 
  let get_opt k tree = failwith ""
  let get k tree = failwith ""
  let to_string tree = failwith ""
end *)


module BTreeMap (K: OrderedSet) (V: Set):Map with type key = K.t and type value = V.t = struct 
  type key = K.t 
  type value = V.t 
  type t = Empty | Node of key * value * t * t
  let empty = Empty
  let rec set k v tree = match tree with
    | Empty -> Node(k, v, Empty, Empty)
    | Node(key, value, left, right) -> 
      let c = K.compare k key in 
      if c > 0 then Node(key, value, left, set k v right)
      else if c < 0  then Node(key, value, set k v left, right)
      else Node(key, v, left, right)
  
   
  let rec get_opt key tree = 
    match tree with 
    | Empty -> None
    | Node(k, v, left, right) -> 
      let c = K.compare k key in 
      if c = 0 then Some v
      else if c < 0 then get_opt k left
      else get_opt k right



  let rec get key tree = 
    match tree with 
    | Empty -> failwith "wrong value nigga"
    | Node(k, v, left, right) -> 
      let c = K.compare k key in 
      if c = 0 then  v
      else if c < 0 then get k left
      else get k right



     let rec to_list tree = 
      match tree with 
      | Empty -> []
      | Node(k, v ,left, right) -> to_list left @ (k,v) :: to_list right


  let to_string tree = List.map (fun (k, v) -> K.to_string k ^ "-> " ^ V.to_string v ) (to_list tree) |> String.concat "," |> Printf.sprintf "{%s}"

end



module IntSet : OrderedSet with type t = int = struct
  type t = int

  let to_string s= string_of_int s
  let compare = Stdlib.compare


end

module IntStringMap = BTreeMap (IntSet) (StringSet)
module IntIntMap = BTreeMap (IntSet) (IntSet)


(* let lst = [1; 5; 3; 7; 8]
let m = List.fold_left (fun acc x -> IntStringMap.set x (string_of_int (x + 2)) acc) IntStringMap.empty lst

let _ = print_endline(IntStringMap.) *)


(* matrix *)

module type Ring = sig
  type t
  val zero : t
  val one : t
  val add : t -> t -> t
  val mul : t -> t -> t
  val compare : t -> t -> int
  val to_string : t -> string
end

module type Matrix = sig
  type elem
  type t
  val create : int -> int -> t
  val identity : int -> t
  val from_rows : elem list list -> t
  val set : int -> int -> elem -> t -> t
  val get : int -> int -> t -> elem
  val transpose : t -> t
  val add : t -> t -> t
  val mul : t -> t -> t
  val to_string : t -> string
end


module IntRing : Ring with type t = int = struct 
  type t = int
  let zero = 0
  let one = 1
  let add = (+)
  let mul = ( * )
  let compare = compare 
  let to_string = string_of_int
  
end

module FloatRing : Ring with type t = float = struct 

  type t = float 
  let zero = 0.0
  let one = 1.0
  let add = (+.)
  let mul = ( *. )
  let compare = compare
  let to_string = string_of_float

end


module type FiniteRing = sig
        include Ring
        val elems : t list 
end

module BoolRing : FiniteRing with type t = bool = struct
  type t = bool
  let zero = false
  let one = true
  let add = (||)
  let mul = (&&)
  let compare = compare
  let to_string = string_of_bool
  let elems = [false; true]
end

module SetRing (D : FiniteRing) : Ring with type t = D.t list = struct
  type t = D.t list
  let zero = []
  let one = D.elems
  let add a b = List.sort_uniq D.compare (a @ b)

  let mul a b = List.sort_uniq D.compare(List.filter(fun e -> List.mem e b) a)
  let compare a b = 
    let a = List.sort D.compare a in
    let b = List.sort D.compare b in 
    let rec helper list1 list2 = match list1, list2 with 
    | [], _ | _, [] -> (List.length list1) - (List.length list2)
    | hd1 :: tl1, hd2 :: tl2 -> 
      let c = D.compare hd1 hd2 in 
      if c <> 0 then  c else helper list1 list2
    in helper a b
    let to_string l = "{" ^ (String.concat "," (List.map D.to_string l)) ^ "}"
    
  
end











 
