
type compiled_pattern = { delta1 : int array; delta2 : int array; pat : string; patlen : int; }

(* TODO compile *)
let compile pat = { delta1 = Array.create 5 0; delta2 = Array.create 5 0; pat = pat; patlen = String.length pat - 1; };;

let boyer_moore_search cpat haystack offset =
    let n = String.length haystack in
    let i = ref (offset + cpat.patlen) in
    let ed = ref false in
    if cpat.patlen + 1 >= n then (-1) else begin
        while not !ed && !i < n do
            let charhay = haystack.[!i] and charpat = cpat.pat.[cpat.patlen] in
            if charhay <> charpat then i := !i + cpat.delta1.(Char.code charhay) else begin
                let m = ref 1 in
                while !m <= cpat.patlen && haystack.[!i - !m] = cpat.pat.[cpat.patlen - !m] do
                    m := !m - 1;
                done;
                if !m > cpat.patlen then begin
                    i := !i - cpat.patlen;
                    ed := true;
                end else begin
                    let nchar = haystack.[!i - !m] in
                    let delta = max cpat.delta1.(Char.code nchar) cpat.delta2.(!m) in
                    i := !i + delta;
                end;
            end;
        done;
        if !ed then !i else (-1);
    end;;

