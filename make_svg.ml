let header n =
  Printf.printf "<svg viewBox=\"-%d -%d %d %d\" xmlns=\"http://www.w3.org/2000/svg\">" n n (2* n) (2 * n)

let add_point n =
  Printf.printf "<circle cx=\"%f\" cy=\"0\" r=\"1\" stroke-width=\"0.1\" stroke=\"black\" fill=\"red\">\n  <animateTransform attributeName=\"transform\" attributeType=\"XML\" type=\"rotate\" from=\"0\" to=\"%d\" begin=\"0s\" dur=\"1200s\" repeatCount=\"indefinite\"/>\n</circle>" (2. *. sqrt (float n)) (n * 360)

let footer () =
  Printf.printf "</svg>"

let main n =
  header (int_of_float (2. *. sqrt (float n)) + 2);
  for i = 1 to n do
    add_point i
  done;
  footer ()

let () = main 500
