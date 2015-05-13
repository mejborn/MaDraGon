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
        List.append vector.Tail [vector.Head] : RowCol

    let CycleRight (vector : RowCol) =
        //Insert last into sequence, then insert everythin except last
        List.append [vector.[vector.Length-1]] (List.ofSeq (Seq.take (vector.Length-2) vector)) : RowCol
   
    let MakeMove (board : Board) (m : move) =
        let selectCol col =
            List.ofSeq (
                seq {0..board.Length-1}
                |> Seq.map (fun i ->
                    board.[i].[col]))

        let mutateCol col (col' : RowCol) =
            List.ofSeq (
                board
                |> Seq.mapi (fun i row ->
                    List.ofSeq(
                        row
                        |> Seq.mapi (fun i' value ->
                            match i with
                            |col -> col'.[i]
                            |_ -> value))))

        let mutateRow (row : int) (row' : RowCol) =
            List.ofSeq (
                board
                |> Seq.mapi (fun i row ->
                    match i with
                    |row -> row'
                    |_ -> board.[i]))

        let board' =
            match m with
            //If a row, we can select a whole list.
            | Move(Left,row) -> 
                board.[row] |> CycleLeft |> mutateRow row
            | Move(Right,row) -> 
                board.[row] |> CycleRight |> mutateRow row
            //If a collumn, then we need to select the column numbered element from each list
            | Move(Up,col) -> 
                selectCol col |> CycleLeft |> mutateCol col
            | Move(Down,col) -> 
                selectCol col |> CycleRight |> mutateCol col
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
        board' , []