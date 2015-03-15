namespace MoveMent
open MathNet.Numerics
open MathNet.Numerics.LinearAlgebra

//Movement of matrices
module public MoveMent =
    type System.Random with
        /// Generates an infinite sequence of random numbers within the given range.
        member this.GetValues(minValue, maxValue) =
            Seq.initInfinite (fun _ -> this.Next(minValue, maxValue))
    let rnd = System.Random()

    type direction =
    | Up
    | Down
    | Left
    | Right
    
    type move = Move of direction * int

    let CycleLeft (vector : Vector<double>) =
        vector 
        |>  (fun xs ->  Seq.append (Seq.skip 1 xs) [Seq.head xs]) 
        |> Vector.Build.DenseOfEnumerable

    let CycleRight (vector : Vector<double>) =
        vector 
        |>  (fun xs ->  Seq.append [vector.[vector.Count-1]] (vector.SubVector(0,vector.Count-1) ) )
        |> Vector.Build.DenseOfEnumerable
   
    let MakeMove (M : Matrix<double>) (m : move) =
        let mutable M' = M.Clone()
        let vector =
            match m with
            | Move(Left,row) -> M.Row(row) |> CycleLeft
            | Move(Right,row) -> M.Row(row) |> CycleRight
            | Move(Up,col) -> M.Column(col) |> CycleLeft
            | Move(Down,col) -> M.Column(col) |> CycleRight
    
        match m with
        | Move(Left,row) -> M'.SetRow(row,vector)
        | Move(Right,row) -> M'.SetRow(row,vector)
        | Move(Up,col) -> M'.SetColumn(col,vector)
        | Move(Down,col) -> M'.SetColumn(col,vector)

        M'

    let ScrambleMap (S : Matrix<double>) N k =
        let directions = 
            rnd.GetValues(0,3)
            |> Seq.take k
            |> Seq.map (fun n ->
                match n with
                | 0 -> direction.Up
                | 1 -> direction.Down
                | 2 -> direction.Right
                | _ -> direction.Left)
        let rowcols = rnd.GetValues(0,N-1) |> Seq.take k
        let moves = Seq.map2 (fun direction rowcol -> Move(direction,rowcol)) directions rowcols
        Seq.fold (fun M move -> (MakeMove M move)) S moves