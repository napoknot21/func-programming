List.sort;;

let rec sort cmp = function
        | [] -> []
        | x::xs -> let high,low = List.partition (cmp x) xs in sort cmp low @ [x] @ sort cmp high;;

let rec sort_o cmp = function
        | [] -> []
        | x::xs -> sort cmp (List.filter (comporse not (cmp x)) xs) @ [x] @ sort cmp 
