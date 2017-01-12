namespace matrix
    type matrix(m:float list list) =
        ///Numero de filas de la matriz
        member this.rows = m.Length
        ///Numero de columnas de la matriz
        member this.cols = m.Head.Length
        //Elementos de la matriz
        member this.items = m        
        //Obtiene el valor de un elemento de la matriz
        member this.item i j = m.[i].[j]        
        override this.ToString() = 
            "\n" + (this.items 
                    |> List.fold (fun r s -> r + "[ " + 
                                             (s |> List.fold (fun x y -> x + (sprintf "%*.02f "5 y)) "") + 
                                             "]\n") "")
        ///Obtiene la matriz traspuesta   
        member this.t() = 
            matrix ([for i in 0..this.cols - 1 do
                        yield [for j in 0..this.rows - 1 do
                                yield this.item j i]])
        ///Calcula la determinante de una matriz cuadrada
        ///Utilizando el metodo de cofactores
        member this.d() =        
           if this.rows = this.cols then
                let b = ref (this)
                while b.Value.item 0 0 = 0.0 do
                    b := matrix (List.append b.Value.items.Tail  [b.Value.items.Head])
                for k = 0 to b.Value.rows - 1 do
                    //Se hacen 0 los elementos que estan abajo de la diagonal
                    b := matrix (b.Value.items 
                                 |> List.mapi (fun i x -> x 
                                                         |> List.mapi (fun j y -> if i > k then 
                                                                                    y - (b.Value.item i k / b.Value.item k k) * b.Value.item k j 
                                                                                  else y )))
                //Se multiplican los elementos que estan en diagonal para obtener la determinante
                b.Value.items 
                |> List.mapi (fun i _ -> b.Value.item i i) 
                |> List.fold ( * ) 1.0
            else
                failwith "no existe determinante la matriz no es cuadrada" 
        ///Obtiene la inversa de una matriz cuadrada
        member this.i() =        
            //Generamos la matriz identidad
            let id = matrix [for i = 0 to this.rows - 1 do
                                yield [for j = 0 to this.cols - 1 do
                                        yield if j = i then 1.0 else 0.0]]  
            //Agregamos la matriz identidad a la derecha de nuestra matriz                              
            let mi = ref (matrix (List.map2 (fun x y -> List.append x y) this.items id.items))
            if (mi.Value.items |> List.exists( fun x -> x.Head <> 0.0)) = false then
                failwith "no existe inversa"
            while mi.Value.item 0 0 = 0.0 do
                mi := matrix (List.append mi.Value.items.Tail  [mi.Value.items.Head])
            //Hacemos los calculos necesarios para pasar la matriz identidad a la izquierda
            for k = 0 to mi.Value.rows - 1 do
                //Se hace 1 los elementos que estan en diagonal
                mi := matrix (mi.Value.items 
                              |> List.mapi (fun i items -> items 
                                                           |> List.map (fun y -> if i = k then y / mi.Value.item k k else y)))            
                //Hacer 0 las columna i de los otros renglones
                mi := matrix (mi.Value.items 
                              |> List.mapi (fun i items -> items 
                                                           |> List.mapi (fun j y -> if i <> k then 
                                                                                        (mi.Value.item k j * - mi.Value.item i k) + y  
                                                                                    else y )))                    
            mi.Value
        ///Resuelve un sistema de ecuaciones lineales representados en una matriz
        member this.g() =
            let mi = ref (this)
            for k = 0 to mi.Value.rows - 1 do
                //Se hace 1 los elementos que estan en diagonal
                mi := matrix (mi.Value.items 
                              |> List.mapi (fun i items -> items 
                                                           |> List.map (fun y -> if i = k then y / mi.Value.item k k else y)))            
                //Hacer 0 las columna i de los otros renglones
                mi := matrix (mi.Value.items 
                              |> List.mapi (fun i items -> items 
                                                           |> List.mapi (fun j y -> if i <> k then 
                                                                                        (mi.Value.item k j * - mi.Value.item i k) + y  
                                                                                    else y )))
            mi.Value.items 
            |> List.map (fun x -> x.[this.cols - 1])
        ///Operador para suma de matrices donde x y son del mismo tamaño 
        static member ( + ) (x:matrix, y:matrix) =
            matrix ((x.items, y.items) ||> List.map2 (fun x y -> (x, y) ||> List.map2(fun x y -> x + y)))
        ///Operador para resta de matrices donde x y son del mismo tamaño
        static member ( - ) (x:matrix, y:matrix) =
            matrix ((x.items, y.items) ||> List.map2 (fun x y -> (x, y) ||> List.map2(fun x y -> x - y)))
        ///Operador para multiplicacion de una matriz por un numero n
        static member ( * ) (n:int, x:matrix) = 
            matrix ( x.items |> List.map (fun x-> x |> List.map(fun x -> x * (float)n)))
         ///Operador para multiplicacion de una matriz por un numero n
        static member ( * ) (n:float, x:matrix) = 
            matrix ( x.items |> List.map (fun x-> x |> List.map(fun x -> x * n)))
        ///Operador para multiplicacion de matrices
        static member ( * ) (x:matrix, y:matrix) =
            matrix([for k in 0..x.rows - 1 do 
                    yield [for i in 0..y.cols - 1 do
                           yield List.sum [for j in 0..x.cols - 1 do yield (x.item k j) * (y.item j i)]]])







