open System.Windows.Forms
open FSharp.Charting
open FSharp.Charting.ChartTypes
open MathNet.Numerics
open MathNet.Numerics.LinearAlgebra
open FSharp.Collections.ParallelSeq
open System.Drawing
open ToolBox
open MoveMent
open EvolutionaryAlgoritms

type System.Random with
    /// Generates an infinite sequence of random numbers within the given range.
    member this.GetValues(minValue, maxValue) =
        Seq.initInfinite (fun _ -> this.Next(minValue, maxValue))


let N = 10
let k = 30

let rnd = System.Random()

let ScrambleMap (S : Matrix<double>) N k =
    let directions = 
        rnd.GetValues(0,3)
        |> Seq.take k
        |> Seq.map (fun n ->
            match n with
            | 0 -> MoveMent.direction.Up
            | 1 -> MoveMent.direction.Down
            | 2 -> MoveMent.direction.Right
            | _ -> MoveMent.direction.Left)

    let rowcols = rnd.GetValues(0,N-1) |> Seq.take k

    let moves = Seq.map2 (fun direction rowcol -> MoveMent.Move(direction,rowcol)) directions rowcols
    
    Seq.fold (fun M move -> (MoveMent.MakeMove M move)) S moves

[<EntryPoint>]
let main argv = 
    let N = 10
    let k = 30
    let cooling = 0.0001
    let maxIterations = 10000
    let numRunsForMean = 100
    let mutable charts = []

    let M : Matrix<double> = DenseMatrix.init N N (fun i j -> double ((i+j) % 2))

    let S = ScrambleMap M N k
    let mutable temperature = 100.0

    let mutable results : double array = Array.zeroCreate maxIterations

    for t in 0..10 do
        printfn "%A" t
        match t with
            |0 -> temperature <- 0.0
            |1 -> temperature <- 0.1
            |2 -> temperature <- 0.2
            |3 -> temperature <- 0.3
            |4 -> temperature <- 0.4
            |5 -> temperature <- 0.5
            |6 -> temperature <- 0.6
            |7 -> temperature <- 0.7
            |8 -> temperature <- 0.8
            |9 -> temperature <- 0.9
            |10 -> temperature <- 1.0
        //Try to solve the map with j temperature
        let mutable resultArray : List<double> array = Array.zeroCreate numRunsForMean
        for i in 0..numRunsForMean-1 do
            printfn "%A" i
            let simulatedAnnealingResult = (EvolutionaryAlgoritms.SimulatedAnnealing.runWithArguments M S temperature cooling maxIterations)
            resultArray.[i] <- simulatedAnnealingResult
        
        for i in 0..resultArray.Length-1 do
            for j in 0..resultArray.[i].Length-1 do
                results.[j] <- results.[j] + resultArray.[i].[j]
        for i in 0..results.Length-1 do
            results.[i] <- float results.[i] / float numRunsForMean

        
        let file = System.IO.File.AppendText("N_" + N.ToString() + "k_" + k.ToString() + "Temperature_" + temperature.ToString() + ".txt")
        for i in 0..results.Length-1 do
           file.WriteLine(i.ToString() + " " + results.[i].ToString())
    0  