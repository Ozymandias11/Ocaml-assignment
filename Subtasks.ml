
(* subtask1 *)

let rec member c t = function
  | [] -> false
  | h::t' -> if c t h = 0 then true else member c t t'

let compare x y = if x = y then 0 else if x < y then -1 else 1

let equal_second_components (_, x) (_, y) = compare x y

let evens_eq_evens_odds_eq_odds n1 n2 = compare (n1 mod 2) (n2 mod 2)

let testing_member () =
  let l =
    [
      __LINE_OF__ ((member compare 3 [1; 2; 3]) = true);
      __LINE_OF__ ((member compare 4 [1; 2; 3]) = false);
      __LINE_OF__ ((member compare 'a' ['a'; 'b'; 'c']) = true);
      __LINE_OF__ ((member equal_second_components ('a',5) [(1,2); (3,4); (5,6)]) = false);
      __LINE_OF__ ((member equal_second_components ('a',6) [(1,2); (3,4); (5,6)]) = true);
      __LINE_OF__ ((member equal_second_components (42, 6) [(1,2); (3,4); (5,6)]) = true);
      __LINE_OF__ ((member evens_eq_evens_odds_eq_odds 4 [1; 2; 3]) = true);
      __LINE_OF__ ((member evens_eq_evens_odds_eq_odds 4 [1; 3; 5]) = false);
    ] in
  let result = List.fold_left (&&) true (List.map snd l) in
  if result then (Printf.printf "The member test succeeds.\n"; [])
  else (Printf.printf "The member test fails.\n Check the corresponding line numbers in the list below.\n";
        (List.filter (fun (x,y) -> y=false) l) |> List.map fst)


        (* subtask2 *)
     
         let count_occurrences lst = 
          let sorted_lst = List.sort compare lst in
          let rec count_helper lst count acc =
            match lst with
            | [] -> acc
            | [x] -> (x, count + 1)::acc
            | x::t -> 
                match t with
                | [] -> (x, count + 1)::acc
                | y::t1 -> if x = y then count_helper t (count + 1) acc
                             else count_helper t 0 ((x, count + 1)::acc)
          in
          count_helper sorted_lst 0 [] |> List.rev |> List.sort (fun (_, count1) (_, count2) -> compare count2 count1) 

        

       
        
    
 


          let testing_count_occurrences () =
            let l =
              [
                __LINE_OF__ ((count_occurrences [("str1", 1); ("str1",2); ("str2",1); ("str2",1); ("str1",2)]) = [(("str1", 2), 2); (("str2", 1), 2); (("str1", 1), 1)]);
                __LINE_OF__ ((count_occurrences ['a'; 'b'; 'a'; 'c'; 'c'; 'a'; 'd']) = [('a', 3); ('c', 2); ('b', 1); ('d', 1)]);
                __LINE_OF__ ((count_occurrences [0; 0; 0; -2; 3; -1; -1; 3; 3; 0]) = [(0, 4); (3, 3); (-1, 2); (-2, 1)]);
                __LINE_OF__ ((count_occurrences [("str1", 1); ("str1",2); ("str2",1); ("str2",1); ("str1",2)]) = [(("str1", 2), 2); (("str2", 1), 2); (("str1", 1), 1)]);
              ] in
            let result = List.fold_left (&&) true (List.map snd l) in
            if result then (Printf.printf "The count_occurrences test succeeds.\n"; [])
            else (Printf.printf "The count_occurrences test fails.\n Check the corresponding line numbers in the list below.\n";
                  (List.filter (fun (x,y) -> y=false) l) |> List.map fst)


         (* subtask 3 *)

         let drop_last list = 
          let rec helper list acc = 
           match list with
            | [] -> failwith "Empty list has no last element"
            | [_] -> List.rev acc
            | h :: t -> helper t (h :: acc)
          in 
          helper list []

          let testing_drop_last () =
            let l =
              [
                __LINE_OF__ ((drop_last [1; 2; 3; 4]) = [1; 2; 3]);
                __LINE_OF__ ((drop_last [1]) = []);
                __LINE_OF__ ((try Some (drop_last []) with (Failure _) -> None) = None) (* If this line is reported during testing, you have an rrror in raising Failure *)
              ] in
            let result = List.fold_left (&&) true (List.map snd l) in
            if result then (Printf.printf "The drop_last test succeeds.\n"; [])
            else (Printf.printf "The drop_last test fails.\n Check the corresponding line numbers in the list below.\n";
                  (List.filter (fun (x,y) -> y=false) l) |> List.map fst)

                (* subtask 4 *)

                let drop_last_opt list = 
                  let rec helper list acc = 
                    match list with 
                    | [] -> None
                    | [_] -> Some (List.rev acc)
                    | h :: t -> helper t (h :: acc)
                  in helper list []

                  let testing_drop_last_opt () =
                    let l =
                      [
                        __LINE_OF__ ((drop_last_opt []) = None);
                        __LINE_OF__ ((drop_last_opt [1]) = Some []);
                        __LINE_OF__ ((drop_last_opt [1;2;3]) = Some [1;2])
                      ] in
                    let result = List.fold_left (&&) true (List.map snd l) in
                    if result then (Printf.printf "The drop_last_opt test succeeds.\n"; [])
                    else (Printf.printf "The drop_last_opt test fails.\n Check the corresponding line numbers in the list below.\n";
                          (List.filter (fun (x,y) -> y=false) l) |> List.map fst)

                        (* (subtask 5) *)

                        let rec zip_with f list1 list2 = 
                          match (list1, list2) with 
                          | ([], _) | (_, []) -> []
                          | h1 :: t1, h2 :: t2 -> 
                            (f h1 h2) :: (zip_with f t1 t2)

                            let testing_zip_with () =
                              let l =
                                [
                                  __LINE_OF__ ((zip_with (fun x y -> [x;y]) [1;2;3] [5;6]) = [[1; 5]; [2; 6]]);
                                  __LINE_OF__ ((zip_with (fun x y -> [x;y]) [1;2;3] [5;6;7;8]) = [[1; 5]; [2; 6]; [3; 7]]);
                                  __LINE_OF__ ((zip_with (fun x y -> (x,y)) [1;2;3] ['a';'b']) = [(1, 'a'); (2, 'b')]);
                                  __LINE_OF__ ((zip_with (+) [1;2;3] [5;6]) =[6; 8]);
                                  __LINE_OF__ ((zip_with (^) ["aa";"bb";"cc"] ["1";"2"]) = ["aa1"; "bb2"]);
                           
                                ] in
                              let result = List.fold_left (&&) true (List.map snd l) in
                              if result then (Printf.printf "The zip_with test succeeds.\n"; [])
                              else (Printf.printf "The zip_with test fails.\n Check the corresponding line numbers in the list below.\n";
                                    (List.filter (fun (x,y) -> y=false) l) |> List.map fst)

                                    (* subtask 6 *)

                                    let unzip lst =
                                      let (lst1, lst2) =
                                        List.fold_left (fun (acc1, acc2) (e1, e2) -> (e1 :: acc1, e2 :: acc2)) ([], []) lst
                                      in
                                      (List.rev lst1, List.rev lst2)


                                      

                                       let testing_unzip () =
                                        let l =
                                          [
                                            __LINE_OF__ ((unzip [('a',1); ('b',2)]) = (['a';'b'], [1;2]));
                                            __LINE_OF__ ((unzip []) = ([], []));
                                            __LINE_OF__ ((unzip [('a',1)]) = (['a'], [1]));
                                     
                                          ] in
                                        let result = List.fold_left (&&) true (List.map snd l) in
                                        if result then (Printf.printf "The unzip test succeeds.\n"; [])
                                        else (Printf.printf "The unzip test fails.\n Check the corresponding line numbers in the list below.\n";
                                              (List.filter (fun (x,y) -> y=false) l) |> List.map fst)

                                              (* subtask7 *)

                                                (* Step 1 *)
                                                (* ([], []) |> (fun (acc1, acc2) -> (['a'], [1])) *)
                                                (* Result: (['a'], [1]) *)

                                                (* Step 2 *)
                                                (* (['a'], [1]) |> (fun (acc1, acc2) -> (['b'; 'a'], [2; 1])) *)
                                                (* Result: (['b'; 'a'], [2; 1]) *)

                                                (* Step 3 *)
                                                (* (['b'; 'a'], [2; 1]) |> (fun (lst1, lst2) -> (List.rev lst1, List.rev lst2)) *)
                                                (* Result: (['a'; 'b'], [1; 2]) *)

                                                (* Final result *)
                                                 (* (['a'; 'b'], [1; 2]) *)

                                               



                                                (* subtask8 *)

   type team = Arg | Sau | Mex | Pol | Por | Kor | Uru | Gha | Cro
  let table_and_scorers games = 
  let teams = List.sort_uniq compare @@ List.concat_map (fun (t1, _, t2, _) -> [t1; t2]) games in
  let team_stats =
    List.map (fun t -> 
        let (games_played, wins, draws, losses, goals_for, goals_against, points) = 
          List.fold_left (fun (g, w, d, l, gf, ga, p) (t1, scorers1, t2, scorers2) -> 
              if t = t1 then
                let (gf', ga') = (List.length scorers1, List.length scorers2) in
                let p' = if gf' > ga' then 3 else if gf' < ga' then 0 else 1 in
                (g+1, w + (if gf' > ga' then 1 else 0), d + (if gf' = ga' then 1 else 0), l + (if gf' < ga' then 1 else 0), gf+gf', ga+ga', p+p')
              else if t = t2 then
                let (gf', ga') = (List.length scorers2, List.length scorers1) in
                let p' = if gf' > ga' then 3 else if gf' < ga' then 0 else 1 in
                (g+1, w + (if gf' > ga' then 1 else 0), d + (if gf' = ga' then 1 else 0), l + (if gf' < ga' then 1 else 0), gf+gf', ga+ga', p+p')
              else (g, w, d, l, gf, ga, p)
            ) (0, 0, 0, 0, 0, 0, 0) games
        in (t, games_played, wins, draws, losses, goals_for, goals_against, points)
      ) teams
  in let team_table = List.sort (fun (t1, _, _, _, _, gf1, ga1, p1) (t2, _, _, _, _, gf2, ga2, p2) ->
    let cmp_points = compare p2 p1 in
    if cmp_points <> 0 then cmp_points
    else let cmp_goals = compare gf2 gf1 in
    if cmp_goals <> 0 then cmp_goals
    else let cmp_goal_diff = compare (gf2 - ga2) (gf1 - ga1) in
    if cmp_goal_diff <> 0 then cmp_goal_diff
    else compare t1 t2) team_stats
  in 

  let goals = List.sort (fun (p1, t1, g1) (p2, t2, g2) ->
    let cmp_goals = compare g2 g1 in
    if cmp_goals <> 0 then cmp_goals
    else let cmp_name = compare (String.lowercase_ascii p1) (String.lowercase_ascii p2) in
    if cmp_name <> 0 then cmp_name
    else compare t1 t2) @@
  List.concat_map (fun (t1, scorers1, t2, scorers2) ->
      let goals1 = List.map (fun p -> (p, t1, 1)) scorers1 in
      let goals2 = List.map (fun p -> (p, t2, 1)) scorers2 in
      goals1 @ goals2) games
  |> List.fold_left (fun acc (p, t, n) ->
         let acc' = List.find_opt (fun (p', t', _) -> p' = p && t' = t) acc in
         match acc' with
         | Some (p', t', n') -> (p', t', n' + n) :: List.filter (fun (p'', t'', _) -> not (p'' = p && t'' = t)) acc
         | None -> (p, t, n) :: acc) []
  |> List.sort (fun (p1, t1, g1) (p2, t2, g2) ->
    let cmp_goals = compare g2 g1 in
    if cmp_goals <> 0 then cmp_goals
    else let cmp_name = compare (String.lowercase_ascii p1) (String.lowercase_ascii p2) in
    if cmp_name <> 0 then cmp_name
    else compare t1 t2)
         
        in
  (team_table, goals)     

 

 

          
          
          








let wc22_C = 
  [(Arg, ["Messi"], Sau, ["Al-Shehri"; "Al-Dawsari"]);
   (Mex, [], Pol, []);
   (Pol, ["Zielinski"; "Lewandowski"], Sau, []);
   (Arg, ["Messi"; "Fernandez"], Mex, []);
   (Pol, [], Arg, ["Mac Allister"; "Alvarez"]);
   (Sau, ["Al-Dawsari"], Mex, ["Martin"; "Chavez"])
  ]

  let wc22_H = 
    [(Uru, [], Kor, []);
     (Por, ["Ronaldo"; "Felix"; "Leao"], Gha, ["Ayew"; "Bukari"]);
     (Kor, ["Cho Gue-sung"; "Cho Gue-sung"], Gha, ["Salisu"; "Kudus"; "Kudus"]);
     (Por, ["Fernandes"; "Fernandes"], Uru, []);
     (Kor, ["Kim Young-gwon"; "Hwang Hee-chan"], Por, ["Horta"]);
     (Gha, [], Uru, ["De Arrascaeta"; "De Arrascaeta"])
    ]


    let testing_table_and_scorers () =
      let l =
        [
          __LINE_OF__ (table_and_scorers wc22_H =
                         ([(Por, 3, 2, 0, 1, 6, 4, 6);
                           (Kor, 3, 1, 1, 1, 4, 4, 4);
                           (Uru, 3, 1, 1, 1, 2, 2, 4);
                           (Gha, 3, 1, 0, 2, 5, 7, 3)],
                          [("Cho Gue-sung", Kor, 2);
                           ("De Arrascaeta", Uru, 2);
                           ("Fernandes", Por, 2);
                           ("Kudus", Gha, 2);
                           ("Ayew", Gha, 1);
                           ("Bukari", Gha, 1);
                           ("Felix", Por, 1);
                           ("Horta", Por, 1);
                           ("Hwang Hee-chan", Kor, 1);
                           ("Kim Young-gwon", Kor, 1);
                           ("Leao", Por, 1);
                           ("Ronaldo", Por, 1);
                           ("Salisu", Gha, 1)]));
          __LINE_OF__ (table_and_scorers wc22_C =
                         ([(Arg, 3, 2, 0, 1, 5, 2, 6);
                           (Pol, 3, 1, 1, 1, 2, 2, 4);
                           (Mex, 3, 1, 1, 1, 2, 3, 4);
                           (Sau, 3, 1, 0, 2, 3, 5, 3)],
                          [("Al-Dawsari", Sau, 2);
                           ("Messi", Arg, 2);
                           ("Al-Shehri", Sau, 1);
                           ("Alvarez", Arg, 1);
                           ("Chavez", Mex, 1);
                           ("Fernandez", Arg, 1);
                           ("Lewandowski", Pol, 1);
                           ("Mac Allister", Arg, 1);
                           ("Martin", Mex, 1);
                           ("Zielinski", Pol, 1)]))
        ] in
      let result = List.fold_left (&&) true (List.map snd l) in
      if result then (Printf.printf "The table_and_scorers test succeeds.\n"; [])
      else (Printf.printf "The table_and_scorers test fails.\n Check the corresponding line numbers in the list below.\n";
            (List.filter (fun (x,y) -> y=false) l) |> List.map fst)




                                              
                                                    



                                                  
