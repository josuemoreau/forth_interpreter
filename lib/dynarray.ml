type 'a t = {
  mutable t: 'a array;
  default: 'a;
  mutable size: int
}

let create n d = { t = Array.make n d; default = d; size = 0 }

let get t n = t.t.(n)

let[@inline] resize t =
  if t.size = Array.length t.t then
    let t' = Array.make (Array.length t.t lsl 1) t.default in
    Array.blit t.t 0 t' 0 t.size;
    t.t <- t'
  else if float_of_int t.size <= 3. *. float_of_int (Array.length t.t) /. 4. then
    let t' = Array.make (Array.length t.t lsr 1) t.default in
    Array.blit t.t 0 t' 0 t.size;
    t.t <- t'

let set t n x = t.t.(n) <- x

let push t x = resize t; t.t.(t.size) <- x; t.size <- t.size + 1

let pop t =
  t.size <- t.size - 1;
  let x = t.t.(t.size) in
  t.t.(t.size) <- t.default;
  x

let length t = t.size

let iter f t = Array.iter f t.t