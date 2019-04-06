let add x y = x + y
let times x y = x * y
let add1Times5v1 = (add 1) >> (times 5)
let add1Times5v2 n = ((add 1) >> (times 5)) n
// let res = add 1 10
// times 5 res

// [ 1  ; 2 ; 3 ]
//   |    |   |
//   v    v   v
// [ "A";"B";"C" ]

type Hero =
  | SuperMan
  | BatMan
  | SpiderMan

let heroes = [ SuperMan; BatMan; SpiderMan ]

let rec map fn = function
  | [] -> []
  | hd :: tl -> (fn hd) :: map fn tl

let map2 fn =
  let rec loop acc = function
    | [] -> List.rev acc
    | hd :: tl -> loop ((fn hd) :: acc) tl
  loop []

let isWearingMask = function
  | SuperMan -> false
  | _ -> true

let res0 = map isWearingMask heroes

let rec fold fn acc = function
  | [] -> acc
  | hd :: tl -> fold fn (fn acc hd) tl

let sumOfCapes sum = function
  | SpiderMan -> sum
  | _ -> sum + 1

let res1 = fold sumOfCapes 0 heroes

// 1 ; 2 ; 3 ; 4 ; 5
// 1 - (2 - (3 - (4 - (5 - 15)))) = -12

// a + b = b + a
// a - b = b - a
// commutative

[<EntryPoint>]
let main argv =
    printfn "Hello World from F#!"
    0 // return an integer exit code
