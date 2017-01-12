//let lagrange (x:float list) (y:float list) (v:float) =
//    [for i = 0 to x.Length - 1 do
//        yield [for j = 0 to x.Length - 1 do
//                if j <> i then
//                    yield (v - x.[j]) / (x.[i]-x.[j])]
//              |> List.fold (*) y.[i]]
//    |> List.sum*)
//let lagrange (x:float list) (y:float list) (v:float) =
//    [for i in 0..x.Length - 1 ->
//         printfn ""
//         printf "+ %.01f*" y.[i]
//         [for j in [0..i-1]@[i+1..x.Length - 1] ->
//            printf "((%.01f - %.01f)/(%.01f - %.01f)) * " v x.[j] x.[i] x.[j]
//            (v - x.[j]) / (x.[i] - x.[j])]
//         |> List.fold (*) y.[i]]
//    |> List.sum
//valores de x
//let x = [0.0;15.0;30.0;45.0;60.0;90.0;120.0]
//valores de fx
//let y = [0.5;0.65;0.73;0.88;1.03;1.14;1.30]
//valor a interpolar
//let v = 110.0

//lagrange x y v
open System

let lagrange (x:float list) (y:float list) (v:float) =
    [for i in 0..x.Length - 1 ->
         [for j in [0..i-1]@[i+1..x.Length - 1] ->
            (v - x.[j]) / (x.[i] - x.[j])]
         |> List.fold (*) y.[i]]
    |> List.sum

//valores de x
let x = [111.0;112.0;113.0;114.0;115.0]
//valores de fx
let y = [13.0;24.0;35.0;46.0;57.0]
//valor a interpolar
let v = 117.0

lagrange x y v
