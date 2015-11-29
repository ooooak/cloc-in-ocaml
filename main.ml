(* Helper Methods *)
let print = Printf.printf
let loga = Array.iter (print "%s\n") 
let logl = List.iter (print "%s\n")

module Fs = struct

    let is_dir name = try match Sys.is_directory name with
        | true -> `True
        | false -> `False
        with 
        |  Sys_error _ -> `Not_Exists

    let is_file path = match is_dir path with
        | `False -> `True
        | `True  -> `False
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


    let lang_ext = function 
        | "hs"  | "ml"   | "rs" | "go"  | "d"   | "c" 
        | "cpp" | "java" | "h"  | "php" | "js"  | "py" 
        | "rb"  | "lua"  | "r"  | "rkt" | "clj" | "coffee"   
            -> true
        | _ -> false

    (* should i just return list then append it ? 
       TDO: can we use tree or list ? *)
    let rec walk start storage = match start with
        | "" -> !storage
        | _ -> start |> Sys.readdir |> Array.iter (fun f -> 
            let current = Filename.concat start f in 
                match is_dir current  with
                | `True -> let _ = walk current storage in ()
                | `False -> 
                    if lang_ext (ext current) then
                        storage := current :: !storage
                    else ()
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

(* TODO: store values in unique map *)
module Stats = struct
    let has search_for from = 
        try 
            List.find (fun v -> v = search_for) from;
           true
        with
        | Not_found  -> false

    let tbl_keys tbl = 
        let keys = ref [] in
        Hashtbl.iter (fun index _ -> 
            if has index !keys then ()
            else keys := index :: !keys
        ) tbl;
        !keys

    let store ls = 
        let storage = Hashtbl.create 10 in 
        ls |> List.iter (fun f -> 
            let ext = Fs.ext f in
            Hashtbl.add storage ext (Fs.count_lines f)
        );
        storage

    let log tbl =
        let rec count c v = match c with 
        | [] -> v
        | h :: t -> count t (v+h) in 
        tbl_keys tbl |> List.iter (fun k -> 
            print "%s: %d\n" k @@ count (Hashtbl.find_all tbl k) 0
        ) 
end


let init start_dir  = 
    Fs.walk start_dir (ref []) 
    |> Stats.store
    |> Stats.log


(* TODO: Parse dir name *)
let main =
    match Array.length Sys.argv with
    | 0 | 1 -> print "Error: Dir name is missing.\n"
    | _ -> 
        let dir_name = Sys.argv.(1) in 
        match Fs.is_dir dir_name with
        | `True -> init dir_name
        | _ -> print "Error: Invalid directory name.\n"
