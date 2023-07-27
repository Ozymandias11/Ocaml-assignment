let a (* : todo *) = (fun a b c -> c (a + b)) 3 

let b (* : todo *) = (fun a b -> (+) b) 

let c (* : todo *) = (fun a b c -> b (c a) :: [a]) "x" 

let d (* : todo *) = (fun a b -> List.fold_left b 1 (List.map ( * ) a))

let e (* : todo *) = (let x = List.map in x (<))  

(* task2 *)

let fac n = 
  let rec fac_helper n acc = 
    if n < 2 then acc
    else fac_helper (n - 1) (acc * n) in 
    fac_helper n 1


let remove a list = 
  let rec remove_helper list acc = 
    match list with 
    | [] -> List.rev acc
    | hd :: tl -> 
      if hd = a then remove_helper tl acc 
      else 
        remove_helper tl (hd :: acc)
      in remove_helper list []


      let partition f l = 
        let rec partition_helper l acc1 acc2 = 
          match l with 
          | [] -> List.rev acc1, List.rev acc2
          | hd :: tl -> 
            if f hd then partition_helper tl (hd :: acc1) acc2  
            else partition_helper tl acc1 (hd :: acc2)
          in partition_helper l [] []

          (* lazy, maby easy evaluation??? *)

          type 'a llist = Cons of 'a * (unit -> 'a llist)

          let rec lnat n = 
            Cons(n, fun () -> lnat (n + 1))

           let rec lfib_helper a b = Cons(a, fun() -> lfib_helper b (a + b))
            let lfib () = lfib_helper 0 1


            let rec ltake n l = 
              if n <= 0 then []
              else match l with 
              | Cons(x, f) -> x :: ltake (n - 1) ( f() )


              let rec filter p (Cons(h,tl)) = 
                if p h  then Cons(h, fun() -> filter p ())
                else filter p (f ())




          
            




