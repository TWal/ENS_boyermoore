type compiled_pattern = { bc : int array; gs : int array; pat : string; }

let compile pat =
    let m = String.length pat in
    let bc = Array.make 256 (-1) in
    for i = 0 to String.length pat - 1 do
        bc.(Char.code pat.[i]) <- i;
    done;
    let kmp = Array.make (m+1) (-1) in
    let gs = Array.make (m+1) 0 in
    let i = ref m in
    let j = ref (m+1) in
    kmp.(!i) <- !j;
    while !i > 0 do
        while !j <= m && pat.[!i-1] != pat.[!j-1] do
            if gs.(!j) == 0 then begin
                gs.(!j) <- !j - !i;
            end;
            j := kmp.(!j);
        done;
        i := !i - 1;
        j := !j - 1;
        kmp.(!i) <- !j;
    done;
    j := kmp.(0);
    for i = 0 to m do
        if gs.(i) == 0 then begin
            gs.(i) <- !j;
        end;
        if i == !j then begin
            j := kmp.(!j);
        end;
    done;
    { bc = bc; gs = gs; pat = pat }
;;



let boyer_moore_search cpat haystack offset =
    let n = String.length haystack in
    let m = String.length cpat.pat in
    let i = ref offset in
    let result = ref (-1) in
    while !result < 0 && !i <= n-m do
        Printf.printf "%s\n%s%s\n" haystack (String.make !i ' ') cpat.pat;
        let j = ref (m-1) in
        while !j >= 0 && cpat.pat.[!j] == haystack.[!i + !j] do
            j := !j - 1;
        done;
        if !j < 0 then begin
            result := !i
        end;
        i := !i + max (!j - cpat.bc.(Char.code haystack.[!i + !j])) cpat.gs.(!j + 1);
    done;
    !result
;;

let testSingle (p, t, r) = (boyer_moore_search (compile p) t 0) == r ;;

let tests = [("EXAMPLE", "HERE IS A SIMPLE EXAMPLE", 17);
             ("EAAMPLEAMPLE", "HERE IS A SIMPLE EAAMPLEAMPLE", 17);
             ("EXAMPLE", "HERE IS A SIMPLE EXAMPLA", -1);
             ("ABCDABD", "ABC ABCDAB ABCDABCDABDE", 15);
             ("EXAMPLS", "HERE IEXAMPLS", 6);
             ("AMPNAM", "AAANAMPNAM", 4)
            ] in
Printf.printf "%b\n" (List.fold_right (fun x y -> x && y) (List.map testSingle tests) true)

