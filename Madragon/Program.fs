open Model
open Model.Types
open Model.MoveMent
open MathNet.Numerics.LinearAlgebra

let DoMutation (world : World) goal =
    List.ofSeq(
        world
        |> Seq.map (fun island ->
            // Deconstruct the island and the configuration
            let population , configuration = island
            let (_ , _ , _ , _ , _ , _ , _ , mutation) = configuration
            match mutation with
            // The algorithms expect an Island and a goal, and will return an Island
            |Mutation.LocalSearch -> island
            |Mutation.MuPlusLambda -> island//EvolutionaryAlgorithms.RunSimulation.localSearch island
            |Mutation.SimulatedAnnealing -> island//EvolutionaryAlgorithms.RunSimulation.localSearch island
            |Mutation.VariableNeighbourhoodSearch -> island//EvolutionaryAlgorithms.RunSimulation.localSearch island
        ))

[<EntryPoint>]
let main argv = 
    // General Setup
    let numRunsForMean = 50
    let N = 10 //Board size
    let k = 30 //Number of shuffles
    let board : Board = DenseMatrix.init N N (fun i j ->  (double) ((i+j) % 2))
    let board',moves = ScrambleMap board N k
    let maxIterations = 1000 // Maximum iterations an algorithm can work on an Island

    // Simulation configuration
    let simulation = Simulation.Single
    let mutation = Mutation.LocalSearch
    let fitTest = FitTest.Hamming

    // Simulation specific configuration
    // Simulated Annealing
    let temperature = 100.0
    let cooling = 0.01
    let lambda = 1.0
    // Mu + Lambdas
    let mu = 5
    let lambda' = 5
    let configuration : RunConfiguration = (temperature,cooling,lambda,lambda',mu,maxIterations,fitTest,mutation)

    // Create world from configuration
    let world : World = World.CreateWorld board board' simulation mutation configuration 1
    let worlds : List<World> = List.ofSeq (seq {0..numRunsForMean}
                                |> Seq.map (fun _ -> DoMutation world board))
    // Go trough worlds, and get means from the fitnesses
    worlds
    |> Seq.iteri (fun i -> 
        islands
        |> Seq.iteri (fun j -> 
        let (population,_) = worlds.[i].[j]
        let (_,fitnesses) = population
        )
    System.Console.ReadLine()
    0