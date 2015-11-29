(* Helper Methods *)
let print = Printf.printf
let loga = Array.iter (print "%s\n") 
let logl = List.iter (print "%s\n")

let print = Printf.printf

let is_dir name = try match Sys.is_directory name with
    | true -> `True
    | false -> `False
    with 
    |  Sys_error _ -> `Not_Exists

let is_file path = match is_dir path with
    | `False -> `True
    | `True -> `False
    | `Not_Exists -> `Not_Exists


(* should i just return list then append it ? *)
(* this function can be improved              *)
let rec scan_files start storage = match start with
    | "" -> !storage
    | _ -> start |> Sys.readdir |> Array.iter (fun f -> 
        let current = Filename.concat start f in 
            match is_dir current  with
            | `True -> let _ = scan_files current storage in ()
            | `False -> storage := current :: !storage; ()
            | `Not_Exists -> ()
    );
    !storage

(* open_in throw Sys_error Exception when path is invalid *)
(* Make sure file is valid before reading *)
let unsafe_read path = 
    let count = ref 0 in
    let in_channel = open_in path in begin 
        try 
            while true do
                let _ = input_line in_channel in
                count := !count + 1
            done 
        with 
            End_of_file -> close_in in_channel
    end; 
    !count


let count_nl path = match is_file path with
    | `True -> unsafe_read path
    | _ -> 0
    

let main = 
    scan_files "test" (ref []) |> List.iter (fun f -> 
        let c = count_nl f in
        print "count: %d\n" c
    )