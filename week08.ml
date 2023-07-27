

     let hd = function 
   | [] -> failwith "invalid"
   | h :: _ -> h
   

   let tl = function 
   | [] -> failwith "invalid"
   | _ :: tl -> tl
   
   let rec length = function 
   | [] -> 0
   | h :: tl -> 1 + length tl
   
   let rec append list1 list2 = 
     match list1 with 
     | [] -> list2 
     | h :: t -> 
       h :: append t list2
   
    let rev list = 
     let rec helper acc list = 
        match list with 
        | [] -> acc
        | h :: t -> helper (h :: acc) t
     in helper [] list
     
     let rec nth list k = 
      if k < 0 then raise (Invalid_argument "error")
      else 
       match list with 
       | [] -> failwith "Invalid"
       | h :: t -> if k = 0 then h 
       else nth t (k - 1)

       (* (task2) *)

       let squaresum list = 
        List.fold_left (fun acc x -> acc + x * x) 0 list

        let float_list list = 
          List.map float_of_int list

     

          let to_string lst =
            "[" ^ List.fold_left (fun acc x -> acc ^ string_of_int x ^ "; ") "" lst ^ "]"



            let part_even list = 
              let odds = List.filter(fun x -> x mod 2 = 1) list in
              let evens  = List.filter(fun x -> x mod 2 = 0) list in
               List.fold_left(fun acc x -> x :: acc) odds evens 

              (* subtasks3 *)




              let eval_poly k list = 
                let rec helper i k  acc = 
                if i = List.length list then acc
                else 
                   helper (i + 1) k (acc +. (List.nth list i) *. k ** float i) 
                in helper 0 k 0.0 

                let derive_poly list = 
                    let rec helper i list acc = 
                      if i = List.length list - 1 then List.rev acc 
                      else 
                        helper (i + 1) (List.tl list) ((float (i + 1) *. List.hd list) :: acc) 
                      in helper 0 list []

                      (* subtask4 *)


                 let rec foo x y b = 
                  if x > y then foo y x b
                  else if b then foo (x + 1) y (not b)
                  else foo x (y - 1) (not b)

                  
                


              


   






      