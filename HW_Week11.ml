module type Readable = sig
  type t
  type arg = string
  val begin_read : arg -> t
  val end_read : t -> unit
  val at_end : t -> bool
  val read_line : t -> (t * string)
  end


  
  module ReadableString : Readable = struct
    type t = { lines : string list; current_line : int }

    type arg = string
  
    let begin_read arg =
      let lines = String.split_on_char '\n' arg in
      { lines; current_line = 0 }
  
    let end_read _ = ()
  
    let at_end t = t.current_line >= List.length t.lines
  
    let read_line t =
      if at_end t then
        (t, "")
      else
        let line = List.nth t.lines t.current_line in
        let new_t = {t with current_line = t.current_line + 1} in 
        (new_t, line)
  end


  

  (* testcase1 *)

  let test_readable_string () =
    let rs = ReadableString.begin_read "A multiline \ntext" in
    let e = ReadableString.at_end rs in
    assert (e = false);
    let rs, l1 = ReadableString.read_line rs in
    assert (l1 = "A multiline ");
    let rs, l2 = ReadableString.read_line rs in
    assert (l2 = "text");
    let e' = ReadableString.at_end rs in
    assert (e' = true);
    ReadableString.end_read rs;
    print_endline "All tests passed!"




    (* task2 *)
    module ReadableFile : Readable = struct
      type t = in_channel
      type arg = string
    
      let begin_read filename =
        let channel = open_in filename in
        channel
    
      let end_read channel =
        close_in channel
    
      let at_end channel =
        let length = in_channel_length channel in
        let position = pos_in channel in
        position >= length
    
      let read_line channel =
        let line = input_line channel in
        (channel, line)
    end
    

    (* testcase2 *)
      (* let rs = ReadableFile.begin_read "test1.txt"
    let a,line = ReadableFile.read_line rs
    let b,line1 = ReadableFile.read_line rs
    let c,line2 = ReadableFile.read_line rs   *)


      (* task3 *)
      module Reader (R : Readable) = struct
        include R
      
        let read_all r =
          let rec loop r acc =
            if at_end r then (r, String.concat "\n" (List.rev acc))
            else
              let r, line = read_line r in
              loop r (line :: acc)
          in
          loop r []
      end  




   module R = Reader (ReadableFile)
    let r = R.begin_read "A multiline\ntext"
    let r, t = R.read_all r 
    let _ = R.end_read r
 




   










   