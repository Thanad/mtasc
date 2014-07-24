(* ODLL sample (c)2004 Nicolas Cannasse *)

let print_one () =
	print_endline "1";
	flush stdout

let rec fib ~number =
	let x = number in
	if x <= 1 then 1 else fib ~number:(x-1) + fib ~number:(x-2)

let ftos = string_of_float

let acreate ~size ~init =
	let s = String.make size init in
	Array.create size s

let f2d a = a

let sdecompose =
	Array.map (fun s -> Array.init (String.length s) (fun n -> s.[n]))
