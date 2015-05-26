namespace Model
open MathNet.Numerics
open MathNet.Numerics.LinearAlgebra
open Types
//Movement of matrices
module public MoveMent =
    type System.Random with
        /// Generates an infinite sequence of random numbers within the given range.
        member this.GetValues(minValue, maxValue) =
            Seq.initInfinite (fun _ -> this.Next(minValue, maxValue))
    let rnd = System.Random()

    let CycleLeft (vector : RowCol) =
        //Insert all except first into sequence, then insert first at the end
        vector 
        |> (fun xs ->  Seq.append (Seq.skip 1 xs) [Seq.head xs]) 
        |> Vector.Build.DenseOfEnumerable

    let CycleRight (vector : RowCol) =
        //Insert last into sequence, then insert everythin except last
        vector 
        |>  (fun xs ->  Seq.append [vector.[vector.Count-1]] (vector.SubVector(0,vector.Count-1) ) )
        |> Vector.Build.DenseOfEnumerable
   
    let MakeMove (board : Board) (m : move) =
        let mutable board' = board.Clone()
        let vector =
            match m with
            | Move(Left,row) -> board.Row(row) |> CycleLeft
            | Move(Right,row) -> board.Row(row) |> CycleRight
            | Move(Up,col) -> board.Column(col) |> CycleLeft
            | Move(Down,col) -> board.Column(col) |> CycleRight
    
        match m with
        | Move(Left,row) -> board'.SetRow(row,vector)
        | Move(Right,row) -> board'.SetRow(row,vector)
        | Move(Up,col) -> board'.SetColumn(col,vector)
        | Move(Down,col) -> board'.SetColumn(col,vector)

        board'

    let ScrambleMap (board : Board) N shuffles =
        let directions = 
            rnd.GetValues(0,3)
            |> Seq.take shuffles
            |> Seq.map (fun n ->
                match n with
                | 0 -> direction.Up
                | 1 -> direction.Down
                | 2 -> direction.Right
                | _ -> direction.Left)
        let rowcols = rnd.GetValues(0,N-1) |> Seq.take shuffles
        let moves = 
            List.ofSeq (
                (directions,rowcols)
                ||> Seq.map2 (fun direction rowcol -> Move(direction,rowcol)))
        let board' : Board = (Seq.fold (fun board' move -> (MakeMove board' move)) board moves)
        board' , moves