namespace Algorithms
open MathNet.Numerics
open MathNet.Numerics.Distributions
open Model
open Model.MoveMent
open Model.Types
open FSharp.Collections.ParallelSeq

// ##################################################
// # Performs Mu Plus Lambda on a given Island      #
// # Returns new Island with new individuals        #
// # And a fitness history for the best individual. #
// # Requires that mu,lambda > 0                    #
// ##################################################

module public MuPlusLambda = 
    let mutable originalIndividuals = Unchecked.defaultof<List<Individual>>
    type System.Random with
        /// Generates an infinite sequence of random numbers within the given range.
        member this.GetValues(minValue, maxValue) =
            Seq.initInfinite (fun _ -> this.Next(minValue, maxValue))
    let rnd = System.Random()

    let rec loop (population : Population) goal configuration iterations =
        // De-construct the configuration
        let (maxIterations , _ , _ , _ , mplConfig) = configuration
        // Print progress
        let iterations' = (double) iterations
        let percentage = (iterations' / (double) maxIterations) * 100.0
        //printfn "%A" percentage
        
        let (mu,lambda) = mplConfig
        let (parents,fitnesses) = population
        // Generate lambda new children from parents in the population
        let parents' : List<Individual> = 
            List.ofSeq(
                seq {0..lambda}
                |> PSeq.map (fun _ -> 
                              let k = Poisson.Sample(1.0)
                              let parent = parents.[rnd.Next(0,parents.Length-1)]
                              // The new parent will carry new fitness, new board and new path
                              let (fitness,board,path) = parent
                              let (board',tmp) = ScrambleMap board board.RowCount k
                              let path' = List.append path tmp
                              let fitness' = FitnessTest.run goal board' configuration
                              let parent' = (fitness',board',path')
                              parent')
                // Add the new parents to the population buffer, sort by fitness, and keep the mu best.
                |> Seq.append parents
                |> Seq.sortBy (fun (fitness,_,_) -> fitness)
                |> Seq.take mu)
        // Generate a new Population with the best available fitness.
        let (fitness',_,_) = parents'.Head
        
        let fitnesses' = List.append fitnesses [fitness']
        let population' : Population = 
            if ((iterations % 4000) = 0) then // Reset if has run for 1000 iterations without solution
                originalIndividuals , fitnesses'
            else
                (parents',fitnesses')
        
        if (fitness' = 0.0 || iterations > maxIterations) then
            if (fitness' = 0.0) then printfn "MPL Found a solution"
            population' , configuration
        else
            loop population' goal configuration (iterations + 1)

    let run (island : Island) (goal : Board) =
        // Deconstruct the island
        let (population , configuration) = island
        let individuals , _ = population
        originalIndividuals <- individuals
        // Since Mu plus Lambda is family tree dependant, the whole Population can be sent to the algorithm.
        loop population goal configuration 0