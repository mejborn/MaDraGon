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
    let k = 50
    let cooling = 0.0001

    let maxIterations = k
    let numRunsForMean = 1

    let M : Matrix<double> = DenseMatrix.init N N (fun i j -> double ((i+j) % 2))
    let S = MoveMent.ScrambleMap M N k
    printfn "%A" M
    printfn "%A" S

    let mutable results : double array = Array.zeroCreate (maxIterations * 2)

    //Run Local Search
    let mutable resultArray : List<double> array = Array.zeroCreate numRunsForMean
    for i in 0..numRunsForMean-1 do
        printfn "Local Search Run no: %A" i
        let localSearchResult = (EvolutionaryAlgoritms.LocalSearch.runWithArguments M S maxIterations)
        resultArray.[i] <- localSearchResult
        
    for i in 0..resultArray.Length-1 do
        for j in 0..resultArray.[i].Length-1 do
            results.[j] <- results.[j] + resultArray.[i].[j]
    for i in 0..results.Length-1 do
        results.[i] <- float results.[i] / float numRunsForMean

    let file = System.IO.File.AppendText("LocalSearch" + "_N_" + N.ToString() + "_k_" + k.ToString() + ".txt")
    for i in 0..results.Length-1 do
        file.WriteLine(i.ToString() + " " + results.[i].ToString())
    file.Flush()
    file.Close()

    //Run Simulated Annealing with Temperature = 0.0
//    let temperature = 0.0
//    let mutable resultArray : List<double> array = Array.zeroCreate numRunsForMean
//    for i in 0..numRunsForMean-1 do
//        printfn "Simulated Annealing Run no: %A" i
//        let simulatedAnnealingResult = (EvolutionaryAlgoritms.SimulatedAnnealing.runWithArguments M S temperature cooling maxIterations)
//        resultArray.[i] <- simulatedAnnealingResult
//        
//    for i in 0..resultArray.Length-1 do
//        for j in 0..resultArray.[i].Length-1 do
//            results.[j] <- results.[j] + resultArray.[i].[j]
//    for i in 0..results.Length-1 do
//        results.[i] <- float results.[i] / float numRunsForMean
//
//    let file = System.IO.File.AppendText("SimulatedAnnealing_" + "_N_" + N.ToString() + "_k_" + k.ToString() + "_Temp_" + t.ToString() + ".txt")
//    for i in 0..results.Length-1 do
//        file.WriteLine(i.ToString() + " " + results.[i].ToString())
//    file.Flush()
//    file.Close()
//
//    //Run Mu + Lambda
//    let mu = 4
//    let lambda = 1
//    let mutable results : double array = Array.zeroCreate (maxIterations * (int mu))
//    let mutable resultArray : List<double> array = Array.zeroCreate numRunsForMean
//    for i in 0..numRunsForMean-1 do
//        printfn "Mu Plus Lambda Run no: %A" i
//        let muPlusLambdaResult = (EvolutionaryAlgoritms.MuPlusLambda.runWithArguments M [S] maxIterations mu lambda)
//        resultArray.[i] <- muPlusLambdaResult
//
//    for i in 0..resultArray.Length-1 do
//        for j in 0..resultArray.[i].Length-1 do
//            results.[j] <- results.[j] + resultArray.[i].[j]
//    for i in 0..results.Length-1 do
//        results.[i] <- float results.[i] / float numRunsForMean
//
//    let file = System.IO.File.AppendText("LocalSearch_" + "N_" + N.ToString() + "k_" + k.ToString() + "Temperature_" + temperature.ToString() + ".txt")
//    for i in 0..results.Length-1 do
//        file.WriteLine(i.ToString() + " " + results.[i].ToString())
//    file.Flush()
//    file.Close()

    0  