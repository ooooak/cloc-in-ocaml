module Fs = struct

    let is_dir name = try match Sys.is_directory name with
        | true -> `True
        | false -> `False
        with 
        |  Sys_error _ -> `Not_Exists

    let is_file path = match is_dir path with
        | `False -> `True
        | `True -> `False
        | `Not_Exists -> `Not_Exists


    (* wall fail on utf8 *)
    let to_str = Char.escaped
    
    let rec ext path = 
        let rec ext' path output len = 
            if len = 0 then ""
            else 
                match String.get path len with
                | '.' -> output
                | c   -> ext' path ((to_str c) ^ output) (len - 1)
        in 
        ext' path "" @@ String.length path - 1


    (* should i just return list then append it ? 
       TDO: can be improved, can we use rec node *)
    let rec walk start storage = match start with
        | "" -> !storage
        | _ -> start |> Sys.readdir |> Array.iter (fun f -> 
            let current = Filename.concat start f in 
                match is_dir current  with
                | `True -> let _ = walk current storage in ()
                | `False -> storage := current :: !storage; ()
                | `Not_Exists -> ()
        );
        !storage


    let count_lines path = match is_file path with
        | `True ->
            let count = ref 0 in
            let in_channel = open_in path in begin 
                try 
                    while true do
                        let _ = input_line in_channel in
                        count := !count + 1
                    done 
                with 
                | _ -> close_in in_channel
            end; 
            !count
        | _ -> 0
end

(* Helper Methods *)
let print = Printf.printf
let loga = Array.iter (print "%s\n") 
let logl = List.iter (print "%s\n")

let init start_dir  =
    Fs.walk start_dir (ref []) 
    |> List.iter (fun f -> 
        let ext = Fs.ext f in
        print "count: %s\n" ext
    )


(* TODO: Parse dir name *)
let main =
    match Array.length Sys.argv with
    | 0 | 1 -> print "Error: Dir name is missing.\n"
    | _ -> 
        let dir_name = Sys.argv.(1) in 
        match Fs.is_dir dir_name with
        | `True -> init dir_name
        | _ -> print "Error: Invalid directory name.\n"
