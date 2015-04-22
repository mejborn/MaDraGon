open MathNet.Numerics
open MathNet.Numerics.LinearAlgebra
open FSharp.Collections.ParallelSeq
open ToolBox
open MoveMent
open Madragon.RunSimulation
open EvolutionaryAlgoritms
open Madragon.World
type System.Random with
    /// Generates an infinite sequence of random numbers within the given range.
    member this.GetValues(minValue, maxValue) =
        Seq.initInfinite (fun _ -> this.Next(minValue, maxValue))

let rnd = System.Random()

[<EntryPoint>]
let main argv = 
    let N = 10
    let k = 30
    let cooling = 0.01

    let maxIterations = 500
    let numRunsForMean = 50

    let M : Matrix<double> = DenseMatrix.init N N (fun i j -> if (i = N/2 || i = N/2-1 || i= N/2+1 || j = N/2) then 1.0 else 0.0)
    let (S,realSolution) = MoveMent.ScrambleMap M N k
    printfn "%A" M
    printfn "%A" S


//    Try with Local Search
//    localSearch M S k
    
//    //Run Simulated Annealing with t temperature
//    for t in 100..100 do
//        SimulatedAnnealing M S k numRunsForMean maxIterations (float t) cooling
    
    //Run Mu + Lambda
//    for mu in 5..5 do
//        for lambda in 5..5 do
//            MuPlusLambda M S k numRunsForMean maxIterations mu lambda

    //Run Mu , Lambda
    //Lambda must be equal or greater than mu
//    for mu in 1..10 do
//        for lambda in mu..10 do
//            MuCommaLambda M S k numRunsForMean maxIterations mu lambda
    //Run Variable Neighbourhood Search
    let lambda = 4
    let mu = 4
    let numGenerations = 5
    VariableNeighbourhoodSeach M S k numRunsForMean maxIterations mu lambda numGenerations


    System.Console.ReadLine()
    0