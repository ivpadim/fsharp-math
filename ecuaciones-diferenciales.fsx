let euler1 f a b n y0 =
    let h = (b - a) / (float) n
    let rec euler f x y0 h =
        match x with
        | x0::x1::t -> let y1 = y0 + f(x0,y0) * h
                       printfn "%.05f %.05f" x1 y1
                       euler f ([x1]@t) y1 h
        | _ -> y0
    euler f [a..h..b] y0 h

let euler f a b n y0 =
    let h = (b - a) / (float) n
    let rec euler f x y0 h =
        match x with
        | x0::x1::xt -> let y1 = y0 + f(x0,y0) * h
                        let yx = y0 + (f(x0,y0) + f(x1,y1)) / 2.0 * h
                        printfn "%.06f %.06f %.06f" x1 y1 yx
                        euler f ([x1]@xt) yx h
        | _ -> y0
    euler f [a..h..b] y0 h


let rungekutta f a b n y0 =
    let h = (b - a) / (float) n
    let rec rungekutta f x y0 h =
        match x with
        | x0::xt -> let k1 = f(x0,y0)
                    let k2 = f(x0 + h/2.0, y0 + k1/2.0 * h)
                    let k3 = f(x0 + h, y0 - k1 * h + 2.0 * k2 * h)
                    let y1 = y0 + (h /6.0 * (k1 + 4.0*k2 + k3))
                    printfn "%.02f %.06f %.06f %.06f %.06f" x0 y0 k1 k2 k3
                    rungekutta f xt y1 h
        | _ -> ()
    rungekutta f [a..h..b] y0 h

let rungekutta4 f a b n y0 =
    let h = (b - a) / (float) n
    let rec rungekutta4 f x y0 h =
        match x with
        | x0::xt -> let k1 = f(x0,y0)
                    let k2 = f(x0 + h/2.0, y0 + k1/2.0 * h)
                    let k3 = f(x0 + h/2.0, y0 + k2/2.0 * h)
                    let k4 = f(x0 + h, y0 + k3 * h)
                    let y1 = y0 + h /6.0 * (k1 + 2.0*k2 + 2.0*k3 + k4)
                    printfn "%.02f %.06f %.06f %.06f %.06f %.06f" x0 y0 k1 k2 k3 k4
                    rungekutta4 f xt y1 h
        | _ -> ()
    rungekutta4 f [a..h..b] y0 h

//let f(x,y) = 0.000002*(100000.00-y)*y
let k1 = 0.00002
let k2 = 0.0001
let m = 100000.0
let x0 = 99000.0
let f(x,y) = k2*(m-y-x0*exp(-k1/k2*y))

let a = 0.0
let b = 10.0
let n = 5 // (b - a) / 2.0
let y = 5213.95256

euler1 f a b n y
rungekutta4 f a b n y
rungekutta  f a b n y



(*
rectan f [0.0..0.1..1.0] 1.0

let rec rectan f (l:float list) (y:float) =
    let m f (x0:float) (x1:float) (y:float) = y + (f x1 y)*(x1-x0)
    match l with
    | h::[] -> y
    | h::t -> let y1 = m f h t.Head y

let f(x,y) = -y + (x**2.0)

let a = 0.0
let b = 1.0
let n = 5
let y = 1.0

*)
(*let f(x,y) = 0.7*y - (x**2.0) + 1.0

let a = 1.0
let b = 2.0
let n = 10
let y = 1.0

euler1 f a b n y
euler f a b n y
*)
(*
let f(x,y) = -2.0*(y**2.0) + exp(-3.0*x)

let a = 0.0
let b = 2.0
let n = 10
let y = 1.0
*)
