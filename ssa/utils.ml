let ( >> ) f g x = g (f x)

let last lst = List.(rev >> hd) lst
