namespace MoveMent
open MathNet.Numerics
open MathNet.Numerics.LinearAlgebra

//Movement of matrices
module public MoveMent =
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