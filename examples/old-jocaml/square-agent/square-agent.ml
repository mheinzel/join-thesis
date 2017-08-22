print_string "looking up location and function...\n"
let there = Ns.lookup "squareloc" vartype
let square  = Ns.lookup "square" vartype

let def done1! () | done2! () = print_string "done!\n"; exit 0;
;;

let def log s =
    print_string ("log: "^s^""); flush stdout;
      reply
;;

let loc mobile
    (* quadric is also moved with the location *)
    def quadric x =
        Printf.printf "quadric call: %d\n" x; flush stdout;
        reply x * x * x * x
    and sum (s,n,f) = reply (if n = 0 then s else sum(s + f n, n-1, f))
    do {
        go there;
        let result_str = "sum (i^2 , i=1..10) = "^string_of_int(sum (0,10,square))^"\n" in
        print_string result_str; flush stdout;
        log result_str;
        done1 ()
    }
;;

spawn {
    let result_str = "sum (i^4 , i=1..10) = "^string_of_int(sum (0,10,quadric))^"\n" in
    print_string result_str;
    log result_str;
    done2 ()
}
