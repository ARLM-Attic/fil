[<ReflectedDefinition>]
module Examples

let ``unit`` () = ()
let ``1`` () = 1
let ``1 + 1`` () = 1 + 1
let ``1 + 1 = 2`` () = 1 + 1 = 2
let ``1 < 2`` () = 1 < 2
let ``1 > 2`` () = 1 > 2
let ``2 <= 2`` () = 2 <= 2
let ``mod`` () = 5 % 4
let ``**`` () = 2. ** 4.
let ``max`` () = System.Math.Max(0,1)
let ``new`` () = System.DateTime(1L)
let ``Array`` () = [|1;2;3|]
let ``[|x..y|]`` () = [|1..3|]
let ``Array get`` () = [|1..3|].[1]
let ``Array set`` () = let xs = [|1..3|] in xs.[1] <- 0; xs
let ``tuple/2`` () = 1,2
let ``tuple/3`` () = 1,2,3
let ``tuple get`` () = let _,two = 1,2 in two
let ``let`` () = let x = 1 in x + 1
let ``let2`` () = let a = 2 in let b = 3 in a * b
let ``sequential`` () = (); (); (); 3
let ``then`` () = if true then 1 else 0
let ``else`` () = if false then 1 else 0
let ``mutable`` () = let mutable x = 1 in x <- x + 1; x
let ``for loop`` () = 
    let mutable sum = 0 
    for n = 1 to 3 do sum <- sum + n
    sum
let ``while loop`` () = 
    let mutable x = 3 
    while x > 0 do x <- x - 1
    x