type TupleHelper = 
    static member map(f: 'a -> 'b, (a: 'a, b: 'a)) : 'b * 'b = f a, f b
    static member map(f: 'a -> 'b, (a, b, c)) : 'b * 'b * 'b = f a, f b, f c

let inline TupleMap f v : ^d =
    let inline map (_: ^t) (f : ^a -> ^b) (value: ^c) : ^d = 
        ((^t or ^a) : (static member map : (^a -> ^b) * ^c -> ^d)(f, value))
    map (Unchecked.defaultof<TupleHelper>) f v

let inline test value = TupleMap (fun a -> a + 1) value

let res : _ * _ = TupleMap (fun a -> a + 1) (1, 2)
let (a, b) = res    