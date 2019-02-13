let distance n = 2. *. sqrt (float n)

let x n = distance n *. cos (float n *. 0.763932 *. Float.pi)

let y n = distance n *. sin (float n *. 0.763932 *. Float.pi)

let header n =
  Printf.printf "<svg viewBox=\"-%d -%d %d %d\" xmlns=\"http://www.w3.org/2000/svg\">" n n (2* n) (2 * n)

let factors n =
  let rec f n i =
    if n mod i = 0 then
      let a, b, c = f (n / i) i in
      1 + a, b, c
    else if n = 1 then
      0, 0, 0
    else if i * i > n then
      0, 1, 1
    else
      let a, b, c = f n (i + 1) in
      0, b + a, c + if a = 0 then 0 else 1
  in
  let a, b, c = f n 2 in
  b + a, c + if a = 0 then 0 else 1

let norm n =
  if n < 2 then 255
  else 255 lsr (n - 1)

let color n =
  let a, b = factors n in
  Printf.sprintf "#%02x00%02x" (norm a) (255 - norm b)

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
