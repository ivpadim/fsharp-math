//No me gusta la funcion pown o la notacion **
let (^) a b = a ** (float) b

let trapecio f a b n =
    let h = (b - a) / (float) n
    ([a..h..b]
     |> List.map(fun x -> f(x) * if x = a || x = b then 1.0 else 2.0)
     |> List.sum) * (h / 2.0)

let simpson f a b n =
    let h = (b - a) / (float) n
    if n % 3 = 0 then
        //Cuando n es multiplo de 3
        ([a..h..b]
         |> List.mapi(fun i x -> f(x) * if x = a || x = b then 1.0 elif i % 3 = 0 then 2.0 else 3.0)
         |> List.sum) * (3.0 / 8.0 * h)
    elif n % 2 = 0 then
        //Cuando n es par
        ([a..h..b]
         |> List.mapi(fun i x -> f(x) * if x = a || x = b then 1.0 elif i % 2 = 0 then 2.0 else 4.0)
         |> List.sum) * (1.0 / 3.0 * h)
    else
        //Fuck this shit, por trapecio
        trapecio f a b n


let romberg f a b n =
    //Desarrolla la matriz triangular de romberg
    let rec matrix (ri:float list) j =
        match ri with
        | rn::[] -> printfn "%.06f" rn
                    rn //Valor de la ultima iteración
        | _ ->  //función de la iteracion n
                ri |> List.iter(fun x -> printfn "%.06f" x)
                let iter r0 r1 n = ((4.0^n) * r1 - r0)  / ((4.0^n) - 1.0)
                matrix [for i in [1..n-j] -> iter ri.[i-1] ri.[i] j] (j+1)
    //Con trapecios generamos la primer iteración de la matriz
    matrix [for i in [0..n-1] -> trapecio f a b ((int) (2.0^i))] 1


//let f(x) = exp (-1.0 * (x * x))
//let f(x) = sqrt (1.0 + x * x)
let f(x) = (x**0.1)*(1.2-x)*(1.0-exp(20.0*(x-1.0)))
let a = 0.0
let b = 1.0
let n = 6

trapecio f a b n
simpson f a b n
simpson f a b (n-1)
romberg f a b n


open System.Diagnostics
let sw = new Stopwatch()
sw.Start()
sw.Stop()

printfn "%A" sw.Elapsed
//let f x = 0.2 + 25.0 * x - 200.0 * (x^2) + 675.0 * (x^3) - 900.0 * (x^4) + 400.0 * (x^5)
//let f x = x + 1.0

//let f x = exp (-1.0 * (x^2))
//let f x = (-5400.0 * x)/(-8.27 * (x*x) - 2000.0)
(*
let f x = log ((x**2.0) + 1.0)
let a = 2.0
let b = 6.0
let n = 4
*)


//let f(x) = sqrt(324.0 - x**2.0)
//let a = -18.0
//let b =  -4.76877752
//let n = 23

//trapecio f a b 8317
//simpson f a b 4096000
//romberg f a b n


// (x*Sqrt[324 - x^2])/2 + 162*ArcSin[x/18]  = 84.823001674
//let F x = x * sqrt(324.0-x**2.0)/2.0 + 162.0 * asin(x/18.0)
//(F 18.0) - (F 1.0) = 169.6460033 -
//    (x*Sqrt[324 - x^2])/2 + 162*ArcSin[x/18]
//Metodo de romberg
(*
  R 1,1 trapecio n = 1
  R 2,1 trapecio n = 2
  R 3,1 trapecio n = 4
  R 4,1 trapecio n = 8

  //(B+b)h/2 = (f(2)+f(6)(6-2)) / 2
  ((f 2.0 + f 6.0) * 4.0) / 2.0
*)
//((4.0^1) * r.[1] - r.[0])  / ((4.0^1) - 1.0)


(*
let mutable r =  [for i in [0..n-1] -> trapecio f a b (pown 2 i)]
for j in [1..n-1] do
    r <- [for i in [1..n-j] -> ((4.0^j) * r.[i] - r.[i-1])  / ((4.0^j) - 1.0)]
    printfn "%A" r
r.[0]*)


(*
#load "FSharp.Charting.Gtk.fsx"
open System
open FSharp.Charting


let h = (b - a) / (float) n

[a..h..b]
|> List.map (fun x -> x, f x)
|> Chart.Line

let simpson f a b =
    let h = (b - a) / (float) 2
    ([a..h..b]
     |> List.map(fun x -> f x * if x = a || x = b then 1.0 else 4.0)
     |> List.sum) * (1.0/3.0 * h)*)

(* pares    impar
0     1     1
1     4     3
2     2     3
3     4     2
4     2     3
5     4     3
6     1     1
*)
