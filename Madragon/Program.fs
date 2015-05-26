open Model
open Model.Types
open Model.MoveMent
open Algorithms
open FSharp.Collections.ParallelSeq
open MathNet.Numerics.LinearAlgebra
open MathNet.Numerics

let DoMutation (world : World) goal =
    List.ofSeq(
        world
        |> PSeq.map (fun (island : Island) ->
            // Deconstruct the island and the configuration
            let population , (configuration : Configuration) = island
            let (_ , _ , algorithm , _ , _) = configuration
            match algorithm with
            // The algorithms expect an Island and a goal, and will return an Island
            |Algorithm.LocalSearch -> 
                printfn "Running Local search" 
                LocalSearch.run island goal
            |Algorithm.MuPlusLambda -> 
                printfn "Running MuPlusLamda" 
                MuPlusLambda.run island goal
            |Algorithm.MuCommaLambda -> 
                printfn "Running MuCommaLambda" 
                MuCommaLambda.run island goal
            |Algorithm.SimulatedAnnealing -> 
                printfn "Running SimulatedAnnealing"
                SimulatedAnnealing.run island goal
            |Algorithm.VariableNeighbourhoodSearch -> 
                printfn "Running VariableNeighborhoodSearch"
                island//EvolutionaryAlgorithms.RunSimulation.localSearch island
        ))

[<EntryPoint>]
let main argv = 
    // General Setup
    let numRunsForMean = 1
    let N = 5 //Board size
    let k = 10 //Number of shuffles
    let board : Board = DenseMatrix.init N N (fun i j ->  (double) ((i+j) % 2))

    let board',moves = ScrambleMap board N k
    let maxIterations = 100000 // Maximum iterations an algorithm can work on an Island
    printfn "Starting..."
    // Simulation configuration
    let simulation = Simulation.All
    let algorithm = Algorithm.LocalSearch
    let fitTest = FitTest.Hamming2

    // Simulation specific configuration
    // Simulated Annealing
    let temperature = 100.0
    let cooling = 0.01
    let lambda = 1.0
    let saConfig = temperature , cooling , lambda
    // Mu + Lambdas
    let mu = 5
    let lambda' = 5
    let mplConfig = mu , lambda'
    let configuration : Configuration = (maxIterations,fitTest,algorithm,saConfig,mplConfig)

    // Create world from configuration
    let world : World = World.CreateWorld board board' simulation algorithm configuration 4

    // Mutate the world(s)
    let worlds : List<World> = List.ofSeq (seq {0..numRunsForMean-1}
                                |> Seq.map (fun i -> 
                                    printfn "Creating world no. %A" i
                                    DoMutation world board))
    // Go trough worlds, and get means from the fitnesses
    for i in 0..worlds.Length-1 do
        printfn "Printing data from World no. %A" i
        for island in worlds.[i] do
            let (population,configuration) = island
            let (_,fitTest,algorithm,_,_) = configuration
            let (_,fitnesses) = population
            //@TODO: Change output to depend on the island type instead of sorting by worlds
            let algorithmText = 
                match algorithm with
                    |SimulatedAnnealing -> "SimulatedAnnealing"
                    |MuPlusLambda -> "MuPlusLambda"
                    |MuCommaLambda -> "MuCommaLamda"
                    |LocalSearch -> "LocalSearch"
                    |VariableNeighbourhoodSearch -> "VariableNeighbourhoodSearch"

            System.IO.Directory.CreateDirectory("output/" + algorithmText)
            let file = System.IO.File.AppendText("output/" + algorithmText + "/runno" + i.ToString() + ".txt")
            for i in 0..fitnesses.Length-1 do
                file.WriteLine(i.ToString() + " " + fitnesses.[i].ToString())
            file.Flush()
            file.Close()
    0