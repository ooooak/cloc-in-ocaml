open Printf

let is_dir name = 
    try 
        Sys.is_directory name
    with 
        Sys_error _ -> false

let absp path_name = Filename.current_dir_name ^ ""

let rec collect lists files dirs = match lists with 
    | [] -> (files, dirs)
    | h :: t -> (match is_dir h with
       | true   -> collect t files (h :: dirs)
       | false  -> collect t (h :: files) dirs)
;;

let make_path dir name = dir ^ "\\" ^ name

(*  file1 
    file2
    dir1
        file1
        dir1
            dir1
                dir1
                    file
    file3
    dir2
*)

let scan dir = 
    let current_list = dir |> Sys.readdir 
                           |> Array.map (make_path dir)
                           |> Array.to_list in 
    match collect current_list [] [] with
    | files, [] -> files
    | _, dirs ->  []
;;



let main =
    match Array.length Sys.argv with
    | 0 | 1 -> printf("Error: Dir name is missing.")
    | _ -> 
        let dir_name = Sys.argv.(1) in 
        if is_dir dir_name then
            scan dir_name
        else
            printf("Error: Invalid directory name.")


