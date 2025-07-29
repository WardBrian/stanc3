(** stanc console application *)

let main () =
  let model_file = "foo" in
  match Driver.Entry.stan2cpp model_file with () -> exit 0

let () = main ()
