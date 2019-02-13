let distance n = 2. *. sqrt (float n)

let x n = distance n *. cos (float n *. 0.763932 *. Float.pi)

let y n = distance n *. sin (float n *. 0.763932 *. Float.pi)

let header n =
  Printf.printf "<svg viewBox=\"-%d -%d %d %d\" xmlns=\"http://www.w3.org/2000/svg\">" n n (2* n) (2 * n)

let is_prime n =
  n >= 2 &&
  let rec p i =
    i * i > n || (n mod i <> 0 && p (i + 1))
  in
  p 2

let color n =
  if is_prime n then "red"
  else "#1f76e0"

let add_point n =
  Printf.printf "<circle cx=\"%f\" cy=\"%f\" r=\"1\" stroke-width=\"0.1\" stroke=\"black\" fill=\"%s\">\n  <animateTransform attributeName=\"transform\" attributeType=\"XML\" type=\"rotate\" from=\"0\" to=\"%d\" begin=\"0s\" dur=\"3600s\" repeatCount=\"indefinite\"/>\n</circle>" (x n) (y n) (color n) (n * 360)

let footer () =
  Printf.printf "</svg>"

let main n =
  header (int_of_float (distance n) + 2);
  for i = 1 to n do
    add_point i
  done;
  footer ()

let () = main 500
