let def cell there =
  let def log s = print_string ("cell "^s^"\n"); flush stdout; reply in

  let loc applet
    def get () | some! x = log ("is empty"); none () | reply x
    and  put x | none! () = log ("contains "^x); some x | reply
    do { go there; none () } in

  reply get, put
;;

Ns.register "cell" cell vartype;
Join.server ()
