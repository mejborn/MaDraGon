﻿open Model
open Model.Types
open Model.MoveMent
open Algorithms
open FSharp.Collections.ParallelSeq
open MathNet.Numerics.LinearAlgebra
open MathNet.Numerics

let DoMutation (world : World) goal =
    List.ofSeq(
        world
        |> Seq.map (fun (island : Island) ->
            // Deconstruct the island and the configuration
            let population , (configuration : Configuration) = island
            let (_ , _ , algorithm , _ , _) = configuration
            match algorithm with
            // The algorithms expect an Island and a goal, and will return an Island
            |Algorithm.LocalSearch -> LocalSearch.run island goal
            |Algorithm.MuPlusLambda -> MuPlusLambda.run island goal
            |Algorithm.MuCommaLambda -> MuCommaLambda.run island goal
            |Algorithm.SimulatedAnnealing -> SimulatedAnnealing.run island goal
            |Algorithm.VariableNeighbourhoodSearch -> island//EvolutionaryAlgorithms.RunSimulation.localSearch island
        ))

[<EntryPoint>]
let main argv = 
    // General Setup
    let numRunsForMean = 1
    let N = 10 //Board size
    let k = 2 //Number of shuffles
    let board : Board = 
        DenseMatrix.ofColumnList (
            [[0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0];
            [0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0;0.0];
            [0.0;0.0;1.0;1.0;1.0;1.0;1.0;1.0;0.0;0.0];
            [0.0;0.0;1.0;1.0;1.0;1.0;1.0;1.0;0.0;0.0];
            [0.0;0.0;1.0;1.0;1.0;1.0;1.0;1.0;0.0;0.0];
            [0.0;0.0;1.0;1.0;1.0;1.0;1.0;1.0;0.0;0.0];
            [0.0;0.0;1.0;1.0;1.0;1.0;1.0;1.0;0.0;0.0];
            [0.0;0.0;1.0;1.0;1.0;1.0;1.0;1.0;0.0;0.0];
            [0.0;0.0;1.0;1.0;1.0;1.0;1.0;1.0;0.0;0.0];
            [0.0;0.0;1.0;1.0;1.0;1.0;1.0;1.0;0.0;0.0]])

    let board',moves = ScrambleMap board N k
    let maxIterations = 10000 // Maximum iterations an algorithm can work on an Island
    printfn "Starting..."
    // Simulation configuration
    let simulation = Simulation.Single
    let algorithm = Algorithm.LocalSearch
    let fitTest = FitTest.Hamming2

    // Simulation specific configuration
    // Simulated Annealing
    let temperature = 100.0
    let cooling = 0.01
    let lambda = Constants.E
    let saConfig = temperature , cooling , lambda
    // Mu + Lambdas
    let mu = 5
    let lambda' = 5
    let mplConfig = mu , lambda'
    let configuration : Configuration = (maxIterations,fitTest,algorithm,saConfig,mplConfig)

    // Create world from configuration
    let world : World = World.CreateWorld board board' simulation algorithm configuration 1
    let worlds : List<World> = List.ofSeq (seq {0..numRunsForMean-1}
                                |> Seq.map (fun _ -> DoMutation world board))
    // Go trough worlds, and get means from the fitnesses
    for i in 0..worlds.Length-1 do
        printfn "Running world no. %A" i
        for island in worlds.[i] do
            let (population,configuration) = island
            let (_,fitTest,algorithm,_,_) = configuration
            let (_,fitnesses) = population
            //@TODO: Change output to depend on the island type instead of sorting by worlds
            let algorithmText = match algorithm with
                |SimulatedAnnealing -> "SimulatedAnnealing"
                |MuPlusLambda -> "MuPlusLambda"
                |MuCommaLambda -> "MuCommaLamda"
                |LocalSearch -> "LocalSearch"
                |VariableNeighbourhoodSearch -> "VariableNeighbourhoodSearch"

            ignore (System.IO.Directory.CreateDirectory("output/" + algorithmText))
            let file = System.IO.File.AppendText("output/" + algorithmText + "/runno" + i.ToString() + ".txt")
            for i in 0..fitnesses.Length-1 do
                file.WriteLine(i.ToString() + " " + fitnesses.[i].ToString())
            file.Flush()
            file.Close()
    0