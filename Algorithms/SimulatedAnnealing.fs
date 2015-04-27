namespace Algorithms
open MathNet.Numerics
open MathNet.Numerics.Distributions
open Model
open Model.MoveMent
open Model.Types

// ##################################################
// # Performs Simulated Annealing on a given Island #
// # Returns new Island with new individuals        #
// # And a fitness history for the best individual. #
// ##################################################

module SimulatedAnnealing =
    type System.Random with
        /// Generates an infinite sequence of random numbers within the given range.
        member this.GetValues(minValue, maxValue) =
            Seq.initInfinite (fun _ -> this.Next(minValue, maxValue))
    let rnd = System.Random()

    let rec loop (individual : Individual) fitnesses goal (configuration : RunConfiguration) iterations temperature =
        //The posibility that a solution has been found, and the algorithm is called again may be there.
        let (fitness,board,path) = individual
        let N = board.RowCount
        let (_ , cooling , lambda , _ , _ , maxIterations , _ , _) = configuration
        
        //Generate random solution:
        let k = Poisson.Sample(lambda)
        let (board',tmp) = ScrambleMap board N k
        let path' = List.append path tmp
        
        //Generate new values from solution
        let fitness' = FitnessTest.run board' goal configuration
        let fitnesses' = List.append fitnesses [fitness']
        let temperature' = temperature-temperature*cooling
        let individual' = (fitness',board',path')
        let iterations' = iterations + 1

        let AcceptanceProbability =
            if (fitness > fitness') then 1.0
            else if (fitness-fitness' = 0.0) then 0.25
            else Constants.E ** ((fitness-fitness')/temperature)

        if (fitness' = 0.0 || iterations > maxIterations) then //Stop the algorithm
            individual' , fitnesses'
        else if(rnd.NextDouble() <= AcceptanceProbability) then //Keep the new individual
            loop individual' fitnesses' goal configuration iterations' temperature'
        else
            loop individual fitnesses goal configuration iterations' temperature' //Throw out the new individual

    let run (island : Island) (goal : Board) =
        let (population : Population , configuration) = island
        // Perform Simulated Annealing on each individual on the Island.
        // Since the Simulated Annealing only runs on a single individual, we need to handle the populations general fitness differently
        // Pass the two sequences together. Return af sequence of touples, remake this as two new Lists.
        let (individuals , fitnesses) = population
        let (temperature , _ , _ , _ , _ , _ , _ , _) = configuration
        let population' = List.ofSeq (
                            population
                            ||> Seq.map2 (fun individual fitness -> loop individual [fitness] goal configuration 0 temperature))
        // The function returns a sequence of individuals and a sequence of lists of fitnesses
        // Since a Population is defined as a sequence of individuals, and a sequence containing the best individuals fitness
        // We will need to convert them back
        let (_,fitness') = population'.Head
        let (individuals') = List.ofSeq (population' |> Seq.map (fun (individual , _) -> individual))
        // Return the new Island.
        let island' : Island = (individuals',fitness') , configuration
        island'