(* same as interface *)

module type Fact = sig
  val fact : int -> int
end

module RecursiveFact : Fact = struct
  let rec fact n = 
    if n = 0 then 1
    else n * fact(n - 1)
end

module TailRecursive : Fact = struct
  let rec factAux n acc = 
    if n == 0 then acc else
      factAux (n - 1) (acc * n)

      let fact n = 
        factAux n 1
end

let x = TailRecursive.fact 10

 module type MyInterface = sig
   val add : int -> int -> int
   val sub : int -> int -> int
 end

 module MyMod : MyInterface = struct
   let add = fun x y -> x + y
   let sub = fun x y -> x - y
 end


 module type abstract = sig
  type 'a t 
   (*  aka def of abstarct stuff *)

   val empty : 'a t
   val add : 'a -> 'a t -> 'a t
   val remove : 'a  -> 'a t -> 'a t

 end

 module MyModule : abstract = struct
   type 'a t = 'a list

   let empty = []
   let add x l = x :: l
   let remove x l = List.filter(fun y -> y <> x) l
 end





