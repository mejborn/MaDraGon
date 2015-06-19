namespace Algorithms
open Model
open Model.MoveMent
open Model.Types
open MathNet.Numerics.Distributions

//// ##################################################
//// # Performs OLS on a given Island                 #
//// # Returns new Island with new individuals        #
//// # And a fitness history for the best individual. #
//// ##################################################
//
    module public OppertunisticLocalSearch =
        let rec loop (individual : Individual) fitnesses goal configuration i operator numIterationsWithoutMove =
            //Deconstruct the individual
            let mutable (fitness,board,path) = individual
            let (maxIterations,_,_,_,_) = configuration
            let N = board.RowCount

            // Choose a random solution
            let k = Poisson.Sample(1.0)
            let (board',tmp) = ScrambleMap board N k
            let path' = List.append path tmp
            let fitness' = FitnessTest.run goal board' configuration
            let fitnesses' = List.append fitnesses [fitness']
            let individual' = (fitness',board',path')

            
            //If solution has been found or maxiteration has been reached, return
            if (fitness' = 0.0 || i > maxIterations) then
                if (fitness' = 0.0) then printfn "OLS Found a solution"
                //printfn "VariableNeighbourhoodSearch Finished"
                (individual',fitnesses')
            //If a better sollution has been found, continue with that.
            
            else if(
                    match operator with
                    |true -> fitness' < fitness
                    |false -> fitness' > fitness) then
                        loop individual' fitnesses' goal configuration (i+1) operator 0

            else if numIterationsWithoutMove < 1000 then //If we have tried less than 500 moves, keep trying.
                loop individual (List.append fitnesses [fitness]) goal configuration (i+1) (not operator) (numIterationsWithoutMove+1)
            else
            //Else try going the other way
                printfn "Turning around"
                loop individual (List.append fitnesses [fitness]) goal configuration (i+1) (not operator) 0

        let run (island : Island) (goal : Board) =
            // Deconstruct the island
            let (population , configuration) = island
            // Perform local search on each individual on the Island.
            // Since the local search only runs on a single individual, we need to handle the populations general fitness differently
            // Pass the two sequences together. Return af sequence of touples, remake this as two new Lists.
            let (individuals , fitnesses) = population
            let population' = List.ofSeq (
                                individuals
                                |> Seq.map (fun individual-> loop individual fitnesses goal configuration 0 true 0))
            // The function returns a sequence of individuals and a sequence of lists of fitnesses
            // Since a Population is defined as a sequence of individuals, and a sequence containing the best individuals fitness
            // We will need to convert them back
            let (_,fitness') = population'.Head
            let (individuals') = List.ofSeq (population' |> Seq.map (fun (individual , _) -> individual))
            // Return the new Island.
            let island' : Island = (individuals',fitness') , configuration
            island'