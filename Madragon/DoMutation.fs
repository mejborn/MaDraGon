module DoMutation
    open Model
    open Model.Types
    open Model.MoveMent
    open Algorithms
    open FSharp.Collections.ParallelSeq
    open MathNet.Numerics
    open MathNet.Numerics.LinearAlgebra

    let rec run (world : World) goal numMerges maxMerges =
        let world' = 
            List.ofSeq(
                world
                // Mutate the islands, they will all run for maxIterations
                // Islands can be mutated individually, so they can run in parallel
                |> PSeq.map (fun (island : Island) ->
                    // Deconstruct the island and the configuration
                    let population , (configuration : Configuration) = island
                    let (_ , _ , algorithm , _ , _) = configuration
                    match algorithm with
                    // The algorithms expect an Island and a goal, and will return an Island
                    |Algorithm.LocalSearch -> 
                        //printfn "Running Local search" 
                        LocalSearch.run island goal
                    |Algorithm.MuPlusLambda -> 
                        //printfn "Running MuPlusLamda" 
                        MuPlusLambda.run island goal
                    |Algorithm.MuCommaLambda -> 
                        //printfn "Running MuCommaLambda" 
                        MuCommaLambda.run island goal
                    |Algorithm.SimulatedAnnealing -> 
                        //printfn "Running SimulatedAnnealing"
                        SimulatedAnnealing.run island goal
                    |Algorithm.OptimisticLocalSearch -> 
                        //printfn "Running VariableNeighborhoodSearch"
                        OptimisticLocalSearch.run island goal
                ))
        
        //printfn ""
        //printfn "Finished running algorithms"
        if (numMerges >= maxMerges) then
            world'
        else
            printfn "Mergin run no: %A"  numMerges
            // Migrate the islands, and DoMutation again
            let bestIndividuals =
                List.ofSeq(
                    world'
                    |> Seq.map (fun island ->
                        let population , configuration = island
                        let (individuals , _) = population
                        let individuals' = 
                            individuals
                            |> List.sortBy (fun (fitness , board , path) -> fitness, path.Length)
                        let (fitness , _ , _) = individuals'.[0]
                        printfn "Best Individual fitness: %A" fitness
                        individuals'.[0]
                        ))
            // We got all the best individuals, now clone the best one into all the islands
            let world'' =
                List.ofSeq(
                    world'
                    |> Seq.map (fun island ->
                        let population , configuration = island
                        let individuals , fitnesses = population
                        let individuals' = List.append [bestIndividuals.[0]] individuals // Put best individual in front of the sequence
                        let population' = individuals' , fitnesses
                        let island' = population' , configuration
                        island')
                        |> Seq.take (world.Length)) //We dont want the worlds to grow
            run world'' goal (numMerges+1) maxMerges