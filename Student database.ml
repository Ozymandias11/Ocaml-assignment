


type student = {
 name : string;
 last_name : string;
 id : int;
 semester : int;
 grades : (int * float) list
 }

type database = student list


let insert student db = 
  student :: db

  let rec find_by_id id db = match db with [] -> []
    | h :: t -> if h.id = id then [h]
    else find_by_id id t 

    let rec find_by_last_name last_name db = match db with [] -> []
    | h :: t -> if h.last_name = last_name then h :: find_by_last_name last_name t
    else h :: find_by_last_name last_name t
  
    let rec remove_by_id id db = match db with [] -> []
    | h :: t -> if h.id <> id then h :: remove_by_id id t 
    else remove_by_id id t

   

      let rec count_in_semster sm db = match db with
      | [] -> 0
      | h :: t -> if h.semester = sm then 1 + count_in_semster sm t
      else count_in_semster sm t

      

            let student_avg_grade id db = 
              let rec sum_grades grades acc = 
                match grades with 
                | [] -> 0
                | (_, grade) :: t -> sum_grades t (acc +. grade)

              in 
              let rec number_of_grades grades acc = 
                match grades with 
                | [] -> 0
                | _ :: t -> number_of_grades t (acc + 1)

              in 
              match find_by_id id db with
              | [] -> 0.0
              | {grades = []} :: _ -> 0.0
              | {grades} :: _ -> 
                let total = sum_grades grades 0.0 in 
                let count = number_of_grades grades 0 in 
               float_of_int total /. float_of_int count

               




                let rec course_avg_grade_helper course_id grades =
                  match grades with
                  | [] -> []
                  | (id, grade)::tl -> if id = course_id then grade :: course_avg_grade_helper course_id tl else course_avg_grade_helper course_id tl
                
                let rec course_avg_grade course_id db =
                  let rec grades_for_course course_id students =
                    match students with
                    | [] -> []
                    | hd::tl -> course_avg_grade_helper course_id hd.grades @ grades_for_course course_id tl
                  in
                  let all_grades = grades_for_course course_id db in
                  let rec sum_grades grades total =
                    match grades with
                    | [] -> total
                    | hd::tl -> sum_grades tl (total +. hd)
                  in
                  let num_grades = List.length all_grades in
                  if num_grades = 0 then 0.0 else sum_grades all_grades 0.0 /. float_of_int num_grades
              

               

                  
                  let rec course_av c db = 
                    let rec avg_grade sum n l = 
                      

                    in

                    let rec iter_students sum n l = 


                    in





                  

               (* let rec merge_sort lst =
                match lst with
                | [] -> []
                | [x] -> [x]
                | _ ->
                    let mid = List.length lst / 2 in
                    let left = sub lst 0 mid in
                    let right = sub lst mid (List.length lst - mid) in
                    merge (merge_sort left) (merge_sort right)
              
              and sub list start len =
                match list with
                | [] -> []
                | h :: t ->
                    if start = 0 then
                      if len = 0 then []
                      else h :: sub t 0 (len - 1)
                    else sub t (start - 1) len
              
              and merge left right =
                match left, right with
                | [], r -> r
                | l, [] -> l
                | hl :: tl, hr :: tr ->
                    if hl < hr then hl :: merge tl right
                    else hr :: merge left tr


                    let rec interleave3 l1 l2 l3 =
                      match (l1, l2, l3) with
                      | ([], [], []) -> []
                      | ([], l2, l3) -> interleave3 l3 l2 []
                      | (l1, [], l3) -> interleave3 l1 l3 []
                      | (l1, l2, []) -> interleave3 l1 [] l2
                      | (x::xs, y::ys, z::zs) -> x::y::z::(interleave3 xs ys zs) *)