﻿open Model
open Model.Types
open Model.MoveMent
open Algorithms
open FSharp.Collections.ParallelSeq
open MathNet.Numerics.LinearAlgebra
open MathNet.Numerics

[<EntryPoint>]
let main argv = 
    // General Setup
    let numRunsForMean = 50
    let maxMerges = 5
    let N = 5 //Board size
    let k = 25 //Number of shuffles
    let numIslands = 2
    let board : Board = DenseMatrix.init N N (fun i j ->  (double) ((i+j) % 2))

    let board',moves = ScrambleMap board N k
    let maxIterations = 10000 // Maximum iterations an algorithm can work on an Island
    printfn "Starting..."
    // Simulation configuration
    let simulation = Simulation.Single
    let algorithm = Algorithm.VariableNeighbourhoodSearch
    let fitTest = FitTest.Hamming

    // Simulation specific configuration
    // Simulated Annealing
    let temperature , cooling , lambda = 100.0 , 0.01 , 1.0
    let saConfig = temperature , cooling , lambda
    // Mu + Lambdas
    let mu , lambda' = 5 , 5
    let mplConfig = mu , lambda'
    let configuration : Configuration = (maxIterations,fitTest,algorithm,saConfig,mplConfig)

    // Create world from configuration
    let world : World = World.CreateWorld board board' simulation algorithm configuration numIslands

    // Mutate the world(s)
    let worlds : List<World> = 
        List.ofSeq (seq {0..numRunsForMean-1}
                                |> Seq.map (fun i -> 
                                    printfn "Creating world no. %A" i
                                    DoMutation.run world board 0 maxMerges))
  
    // Go trough worlds, and get means from the fitnesses
    printfn "Outputting data before reduction"
    for i in 0..worlds.Length-1 do
        printfn "Printing data from World no. %A" i
        for island in worlds.[i] do
            let (population,configuration) = island
            let (_,fitTest,algorithm,_,_) = configuration
            let (_,fitnesses) = population
            let algorithmText = 
                match algorithm with
                    |SimulatedAnnealing -> "SimulatedAnnealing"
                    |MuPlusLambda -> "MuPlusLambda"
                    |MuCommaLambda -> "MuCommaLamda"
                    |LocalSearch -> "LocalSearch"
                    |VariableNeighbourhoodSearch -> "VariableNeighbourhoodSearch"

            ignore (System.IO.Directory.CreateDirectory("output/before_reduction/" + algorithmText))
            let file = System.IO.File.AppendText("output/before_reduction/" + algorithmText + "/runno" + i.ToString() + ".txt")
            for i in 0..fitnesses.Length-1 do
                file.WriteLine(i.ToString() + " " + fitnesses.[i].ToString())
            file.Flush()
            file.Close()
    // Try to reduce the path length
    // Deconstructing the first world, to get the run configuration
    let island = worlds.[0].[0]
    // Get the the best solution available
    let bestSolution =
        let mutable bestFitness = 99999.9
        let mutable solution = Unchecked.defaultof<Individual>
        for world in worlds do
            for island in world do
                let population , _ = island
                let individuals , fitnesses = population
                if (fitnesses.[0] < bestFitness) then
                    bestFitness <- fitnesses.[0]
                    solution <- individuals.[0]
                else if (fitnesses.[0] = bestFitness) then
                    let _ , _ , path = solution
                    let _ , _ , path' = individuals.[0]
                    if (path'.Length < path.Length) then
                        solution <- individuals.[0]
        solution
    let _ , _ , path = bestSolution
    printfn "Trying to reduce the length of the path"
    ignore (System.IO.Directory.CreateDirectory("output/path_reduction"))
    let file = System.IO.File.AppendText("output/path_reduction/" + "result.txt")
    seq {0..maxIterations}
    |> Seq.fold (fun path' i ->
        printfn "%A" i
        let path'' = PathReduction.run board path simulation algorithm configuration numIslands maxMerges
        file.WriteLine(path''.Length)
        path'') path
    |> file.WriteLine
        
    file.Flush()
    file.Close()
    0