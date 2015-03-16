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

[<EntryPoint>]
let main argv = 
    let N = 10
    let k = 30
    let cooling = 0.0001
    let mu = 3
    let lambda = 3
    let maxIterations = 10000
    let numRunsForMean = 100
    let mutable charts = []

    let M : Matrix<double> = DenseMatrix.init N N (fun i j -> double ((i+j) % 2))
    let S = MoveMent.ScrambleMap M N k
    
    let mutable temperature = 1.0
    let mutable results : double array = Array.zeroCreate maxIterations

    //Run Simulated Annealing
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

        let file = System.IO.File.AppendText("SimulatedAnnealing_" + "N_" + N.ToString() + "k_" + k.ToString() + "Temperature_" + temperature.ToString() + ".txt")
        for i in 0..results.Length-1 do
           file.WriteLine(i.ToString() + " " + results.[i].ToString())
    //Run Mu + Lambda
    let mutable resultArray : List<double> array = Array.zeroCreate numRunsForMean
    for i in 0..numRunsForMean-1 do
        printfn "%A" i
        let muPlusLambdaResult = (EvolutionaryAlgoritms.MuPlusLambda.runWithArguments M [S] maxIterations mu lambda)
        resultArray.[i] <- muPlusLambdaResult
        
    for i in 0..resultArray.Length-1 do
        for j in 0..resultArray.[i].Length-1 do
            results.[j] <- results.[j] + resultArray.[i].[j]
    for i in 0..results.Length-1 do
        results.[i] <- float results.[i] / float numRunsForMean

    let file = System.IO.File.AppendText("MuPlusLambda_" + "N_" + N.ToString() + "k_" + k.ToString() + "Mu_" + mu.ToString() + "Lambda_" + lambda.ToString() + ".txt")
    for i in 0..results.Length-1 do
        file.WriteLine(i.ToString() + " " + results.[i].ToString())
    0  