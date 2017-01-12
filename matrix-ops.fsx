#load @"matrix.fs"

open matrix

let a = matrix [[ 5.0; 3.0; 2.0]
                [ 1.0; 4.0; 9.0]
                [13.0; 0.0; 1.0]]
                        
let b = matrix [[ 3.0; 4.0]
                [-2.0; 0.0]
                [ 1.0;-2.0]]

let c = matrix[[-3.0; 1.0]
               [ 0.0; 2.0]]

let d = matrix [[ 7.0;-2.0; 2.0]
                [ 3.0; 1.0; 6.0]
                [ 5.0; 4.0; 5.0]]

let e = matrix [[ 1.0; 4.0]
                [ 5.0; 1.0]]

//Suma
let s = a + d
//Resta
let r = a - d
//Multiplicacion
let m = a * a

let u = (2 * a * b) + (3 * b * c.t())


(*
a.d()
a.t().d()

let a = matrix [[ 0.0; 5.0;-3.0;-2.0]
                [-2.0;-3.0; 2.0; 0.0]
                [ 1.0; 3.0; 0.0; 2.0]
                [ 4.0; 0.0;-5.0; 3.0]]

a.d()
c - e

4 4 (fun x -> x + 1)*)
