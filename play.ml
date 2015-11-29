let is_dir name = try match Sys.is_directory name with
    | true -> `True
    | false -> `False
    with 
    |  Sys_error _ -> `Not_Exists

let is_file path = match is_dir path with
    | `False -> `True
    | ex -> ex


let main =
    match is_file "test" with
    | `True -> print_string "yes"
    | `False -> print_string "No"
    | `Not_Exists -> print_string "Invalid path"