let print = Printf.printf
let loga = Array.iter (print "%s\n") 
let logl = List.iter (print "%s\n")

let print = Printf.printf
let is_dir name = try match Sys.is_directory name with
    | true -> `True
    | false -> `False
    with Sys_error _ -> `Error

let files dir = dir |> Sys.readdir |> Array.to_list

(* should i just return list then append it ? *)
(* this function can be improved *)
let (/) = Filename.concat
let rec scan start storage = match start with
    | "" -> !storage
    | _ -> files start |> List.iter (fun f -> 
        let current = (start / f) in 
        match is_dir current  with
        | `True -> let _ = scan current storage in ()
        | `False -> storage := current :: !storage; ()
        | `Error -> ()
    );
    !storage

let main = 
  scan "test" (ref []) |> logl