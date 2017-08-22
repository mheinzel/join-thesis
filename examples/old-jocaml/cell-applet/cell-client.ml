let cell = Ns.lookup "cell" vartype

let loc user
  do {
    let get, (put : string -> unit) = cell user in
    put "world";
    put ("hello, "^get ()); 
    print_string (get ()^"\n");
    exit 0;
  }
