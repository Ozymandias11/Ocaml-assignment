

module type ListStackSig = sig

  val empty : 'a list
  val push : 'a -> 'a list -> 'a list
  val peek : 'a list -> 'a
  val pop : 'a list -> 'a list
end


module type stackSig = sig
  type 'a stack

  val empty : 'a stack
  val push : 'a -> 'a stack -> 'a stack
  val peek : 'a stack -> 'a
  val pop : 'a stack -> 'a stack
 

end

module MyStack : stackSig  = struct

  type 'a stack = 
  | Empty
  | Entry of 'a * 'a stack

  let empty = Empty

  let push x s = 
    Entry(x, s)

    let peek = function
    | Empty ->failwith "Empty"
    | Entry(x, _) -> x

    let pop = function
    | Empty -> failwith "Empty"
    | Entry(_, s) -> s
end



module ListStack : stackSig  = struct 
  type 'a stack = 'a list
  let empty = []

  let push x s = 
    x :: s

  let peek = function
  | [] -> failwith "Empty"
  | x :: _ -> x

  let pop = function
  | [] -> failwith " Empty"
  | _ :: s -> s



end

let s = ListStack.empty
let s1 = ListStack.push 1 s
let s2 = ListStack.peek s1



let x = ListStack.(empty |> push 42 |> peek)

let w = 
  let open ListStack in 
  empty |> push 42 |> peek