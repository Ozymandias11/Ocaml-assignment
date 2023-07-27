(* subtask1 *)

let f1 acc (a, b) = acc @ [(b, a)]

let f2 acc a =
  let length = List.length acc in
  if length mod 2 = 0 then [a] @ acc else acc @ [a]

  

let f3 acc (k, v) = fun x -> if x = k then v else acc x


let testing_fs () =
  let l =
    [
      __LINE_OF__ ((List.fold_left f1 [] [(1,2); (3,4); (5,6)]) =
                     [(2,1); (4,3); (6,5)]);
      __LINE_OF__ ((List.fold_left f2 [] ['a';'b';'c';'d';'e';'f';'g']) =
                     ['g';'e';'c';'a';'b';'d';'f']);
      __LINE_OF__ (let g = List.fold_left f3 (fun _ -> 0)
                             [('a',3); ('z', -9); ('d', 18)] in
                   (g 'a' = 3) && (g 'd' = 18) && (g 'z' = -9))
   ] in
  let result = List.fold_left (&&) true (List.map snd l) in
  if result then (Printf.printf "The f1, f2, f3 test succeeds.\n"; [])
  else (Printf.printf "The f1, f2, f3 test fails.\n Check the corresponding line numbers in the list below.\n";
        (List.filter (fun (x,y) -> y=false) l) |> List.map fst)


        (* subtask2 *)


        let map_tr f list = 
          let rec helper acc list = 
            match list with 
            | [] -> List.rev acc
            | hd :: tl -> helper(f hd :: acc) tl 
          in helper [] list

          
          let replicate_tr n x = 
            let rec helper acc n  = 
              if n < 1 then List.rev acc else 
                helper (x :: acc) (n - 1) 
              in
              helper [] n 

              
let test_tr_llist () =
  let l =
    [
      __LINE_OF__ (map_tr succ [1;2;3] = [2;3;4]);
      __LINE_OF__ (map_tr (fun x -> x^x) ["a";"b";"c"] = ["aa";"bb";"cc"]);
      __LINE_OF__ (replicate_tr 5 "a" = ["a";"a";"a";"a";"a"]);
      __LINE_OF__ (replicate_tr (-3) "a" = [])
    ] in
  let result = List.fold_left (&&) true (List.map snd l) in
  if result then (Printf.printf "The tests for map and replicate succeed.\n"; [])
  else (Printf.printf "The test for tests for map and replicate fail.\n Check the corresponding line numbers in the list below.\n";
        (List.filter (fun (x,y) -> y=false) l) |> List.map fst)

        (* subtasks3 *)


        type 'a custom_llist = (unit -> 'a custom_cell) and 'a custom_cell = NilC | ConsC of ('a * 'a custom_llist)
        type 'a ocaml_llist = 'a ocaml_cell Lazy.t and 'a ocaml_cell = NilO | ConsO of ('a * 'a ocaml_llist)





let rec map_over_custom_llist f llist = 
  fun () -> match llist () with
    | NilC -> NilC
    | ConsC (x, xs) -> ConsC (f x, map_over_custom_llist f xs)

    let rec map_over_ocaml_llist f llist = 
      lazy (match Lazy.force llist with
        | NilO -> NilO
        | ConsO (x, xs) -> ConsO (f x, map_over_ocaml_llist f xs)) 





        let rec from_to_custom from to_ step =
          if from <= to_
          then fun () -> ConsC (from, from_to_custom (from + step) to_ step)
          else fun () -> NilC
    
    let rec print_custom_llist n c_list =
      if n != 0
      then match c_list () with
           | NilC -> print_string "Nil\n"
           | ConsC (h, t) ->
              Printf.printf "%d, " h;
              print_custom_llist (n-1) t
      else print_string "...\n"
    
    let rec custom_llist_to_string n c_list =
      if n != 0
      then match c_list () with
        | NilC -> "Nil"
        | ConsC (h, t) ->
           string_of_int h ^ ", " ^
             custom_llist_to_string (n-1) t
      else "..."
    
    let rec from_to_ocaml from to_ step =
          if from <= to_
          then lazy (ConsO (from, from_to_ocaml (from + step) to_ step))
          else lazy NilO
    
    let rec print_ocaml_llist n o_list =
      if n != 0
      then match Lazy.force o_list with
        | NilO -> print_string "Nil\n"
        | ConsO (h, t) ->
           Printf.printf "%d, " h;
           print_ocaml_llist (n-1) t
      else print_string "...\n"
    
    let rec ocaml_llist_to_string n o_list =
      if n != 0
      then match Lazy.force o_list with
        | NilO -> "Nil"
        | ConsO (h, t) ->
           string_of_int h ^ ", " ^
             ocaml_llist_to_string (n-1) t
      else "..."


      let test_map_llist () =
        let l =
          [
            __LINE_OF__ (custom_llist_to_string 10
              (map_over_custom_llist (fun x -> x+1) (from_to_custom 0 5 1)) =
                           "1, 2, 3, 4, 5, 6, Nil");
            __LINE_OF__ (custom_llist_to_string 10
              (map_over_custom_llist (fun x -> x+1) (from_to_custom 6 5 1)) =
                           "Nil");
             __LINE_OF__ (ocaml_llist_to_string 10
              (map_over_ocaml_llist (fun x -> x+1) (from_to_ocaml 0 5 1)) =
                            "1, 2, 3, 4, 5, 6, Nil");
              __LINE_OF__ (ocaml_llist_to_string 10
              (map_over_ocaml_llist (fun x -> x+1) (from_to_ocaml 6 5 1)) =
                             "Nil")
          ] in
        let result = List.fold_left (&&) true (List.map snd l) in
        if result then (Printf.printf "The test for mapping over lazy lists succeeds.\n"; [])
        else (Printf.printf "The test for mapping over lazy lists fails.\n Check the corresponding line numbers in the list below.\n";
              (List.filter (fun (x,y) -> y=false) l) |> List.map fst) 


              (* subtask4 *)


              let rec merge_custom_llists llist1 llist2 = 
                fun () -> match llist1 (), llist2 () with
                  | NilC, _ -> llist2 ()
                  | _, NilC -> llist1 ()
                  | ConsC (x, xs), ConsC (y, ys) ->
                      if x <= y then ConsC (x, merge_custom_llists xs llist2)
                      else ConsC (y, merge_custom_llists llist1 ys)
              
              let rec merge_ocaml_llists llist1 llist2 = 
                lazy (match Lazy.force llist1, Lazy.force llist2 with
                  | NilO, _ -> Lazy.force llist2
                  | _, NilO -> Lazy.force llist1
                  | ConsO (x, xs), ConsO (y, ys) ->
                      if x <= y then ConsO (x, merge_ocaml_llists xs llist2)
                      else ConsO (y, merge_ocaml_llists llist1 ys))

                    
let test_merge_llists () =
  let l =
    [
      __LINE_OF__ (custom_llist_to_string 13
        (merge_custom_llists (from_to_custom 0 5 1) (from_to_custom 0 5 1)) =
                     "0, 0, 1, 1, 2, 2, 3, 3, 4, 4, 5, 5, Nil");
      __LINE_OF__ (custom_llist_to_string 13
                     (merge_custom_llists (from_to_custom 0 5 1) (from_to_custom 6 5 1)) =
                     "0, 1, 2, 3, 4, 5, Nil");
      __LINE_OF__ (ocaml_llist_to_string 13
        (merge_ocaml_llists (from_to_ocaml 0 5 1) (from_to_ocaml 0 5 1)) =
                     "0, 0, 1, 1, 2, 2, 3, 3, 4, 4, 5, 5, Nil");
      __LINE_OF__ (ocaml_llist_to_string 13
                     (merge_ocaml_llists (from_to_ocaml 0 5 1) (from_to_ocaml 6 5 1)) =
                     "0, 1, 2, 3, 4, 5, Nil")
    ] in
  let result = List.fold_left (&&) true (List.map snd l) in
  if result then (Printf.printf "The test for merging over lazy lists succeeds.\n"; [])
  else (Printf.printf "The test for merging over lazy lists fails.\n Check the corresponding line numbers in the list below.\n";
        (List.filter (fun (x,y) -> y=false) l) |> List.map fst)
                

        (* subtask5 *)

        let rec drop_dupl_custom_llist llist = 
          fun () -> match llist () with
            | NilC -> NilC
            | ConsC (x1, xs1) ->
                match xs1 () with
                  | NilC -> ConsC (x1, xs1)
                  | ConsC (x2, xs2) ->
                      if x1 = x2 then drop_dupl_custom_llist xs1 ()
                      else ConsC (x1, drop_dupl_custom_llist xs1)
        
        let rec drop_dupl_ocaml_llist llist = 
          lazy (match Lazy.force llist with
            | NilO -> NilO
            | ConsO (x1, xs1) ->
                match Lazy.force xs1 with
                  | NilO -> ConsO (x1, xs1)
                  | ConsO (x2, xs2) ->
                      if x1 = x2 then Lazy.force (drop_dupl_ocaml_llist xs1)
                      else ConsO (x1, drop_dupl_ocaml_llist xs1))

                let test_drop_dupl_llists () =
                  let l =
                    [
                      __LINE_OF__ (custom_llist_to_string 13
                                     (drop_dupl_custom_llist
                                        (merge_custom_llists (from_to_custom 0 5 1)
                                           (from_to_custom 0 5 2))) =
                                     "0, 1, 2, 3, 4, 5, Nil");
                      __LINE_OF__ (custom_llist_to_string 13
                                     (drop_dupl_custom_llist
                                        (merge_custom_llists (from_to_custom 0 5 1)
                                           (from_to_custom 6 5 1))) =
                                     "0, 1, 2, 3, 4, 5, Nil");
                      __LINE_OF__ (ocaml_llist_to_string 13
                                     (drop_dupl_ocaml_llist
                                        (merge_ocaml_llists (from_to_ocaml 0 5 1)
                                           (from_to_ocaml 0 5 1))) =
                                     "0, 1, 2, 3, 4, 5, Nil");
                      __LINE_OF__ (ocaml_llist_to_string 13
                                     (drop_dupl_ocaml_llist
                                        (merge_ocaml_llists (from_to_ocaml 0 5 1)
                                           (from_to_ocaml 6 5 1))) =
                                     "0, 1, 2, 3, 4, 5, Nil")
                    ] in
                  let result = List.fold_left (&&) true (List.map snd l) in
                  if result then (Printf.printf "The test for dropping duplicates from  lazy lists succeeds.\n"; [])
                  else (Printf.printf "The test for dropping duplicates from lazy lists fails.\n Check the corresponding line numbers in the list below.\n";
                        (List.filter (fun (x,y) -> y=false) l) |> List.map fst)

                  
                        

                        let rec hamming_custom_llist = 
                          let rec mult n llist = 
                            map_over_custom_llist (fun x -> n * x) llist
                          in
                          let rec aux () = 
                            ConsC (1, merge_custom_llists (mult 2 aux) (merge_custom_llists (mult 3 aux) (mult 5 aux)))
                          in
                          drop_dupl_custom_llist aux
                        
                        let rec hamming_ocaml_llist = 
                          let rec mult n llist = 
                            map_over_ocaml_llist (fun x -> n * x) llist
                          in
                          let rec aux = 
                            lazy (ConsO (1, merge_ocaml_llists (mult 2 aux) (merge_ocaml_llists (mult 3 aux) (mult 5 aux))))
                          in
                          drop_dupl_ocaml_llist aux




                          let hamming_custom = hamming_custom_llist
                          let hamming_ocaml = hamming_ocaml_llist

                          let test_hamming_llists () =
                            let l =
                              [
                                __LINE_OF__ (custom_llist_to_string 14 hamming_custom =
                                               "1, 2, 3, 4, 5, 6, 8, 9, 10, 12, 15, 16, 18, 20, ...");
                                __LINE_OF__ (custom_llist_to_string 20 hamming_custom = 
                                               "1, 2, 3, 4, 5, 6, 8, 9, 10, 12, 15, 16, 18, 20, 24, 25, 27, 30, 32, 36, ...");
                                __LINE_OF__ (ocaml_llist_to_string 14 hamming_ocaml =
                                               "1, 2, 3, 4, 5, 6, 8, 9, 10, 12, 15, 16, 18, 20, ...");
                                __LINE_OF__ (ocaml_llist_to_string 20 hamming_ocaml = 
                                               "1, 2, 3, 4, 5, 6, 8, 9, 10, 12, 15, 16, 18, 20, 24, 25, 27, 30, 32, 36, ...")
                              ] in
                            let result = List.fold_left (&&) true (List.map snd l) in
                            if result then (Printf.printf "The test for Hamming lists succeeds.\n"; [])
                            else (Printf.printf "The test for hamming lists fails.\n Check the corresponding line numbers in the list below.\n";
                                  (List.filter (fun (x,y) -> y=false) l) |> List.map fst)



                                  


                                  


                        




