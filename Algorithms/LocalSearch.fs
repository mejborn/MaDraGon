namespace Algorithms
open Model
open Model.MoveMent
open Model.Types

// ##################################################
// # Performs local search on a given Island        #
// # Returns new Island with new individuals        #
// # And a fitness history for the best individual. #
// ##################################################

module public LocalSearch =
    let rec loop (individual : Individual) fitnesses goal configuration =
        //Deconstruct the individual
        let mutable (fitness,board,path) = individual
        let N = board.RowCount
        printfn "%A" fitness

        //Optimisation needed!
        let mutable tmp = board
        let mutable board' = board
        let mutable move = Move(Left,0)
        for i in 0..N-1 do
            for j in 0..3 do
                let move' =
                    match j with
                    |0 -> Move(Left,i)
                    |1 -> Move(Right,i)
                    |2 -> Move(Up,i)
                    |3 -> Move(Down,i)
                tmp <- MakeMove board move
                let fitness' = FitnessTest.run tmp goal configuration
                if(fitness' < fitness) then
                    fitness <- fitness'
                    board' <- tmp
                    move <- move'
        
        //If taking a step reduced the fitness, try to take another.
        let (fitness' , _ , _) = individual
        if fitness <> fitness' then
            let individual' = fitness,board',List.append path [move]
            let fitnesses' = List.append fitnesses [fitness]
            loop individual' fitnesses' goal configuration
        else
            (individual,fitnesses)

    let run (island : Island) (goal : Board) =
        // Deconstruct the island
        let (population , configuration) = island
        // Perform local search on each individual on the Island.
        // Since the local search only runs on a single individual, we need to handle the populations general fitness differently
        // Pass the two sequences together. Return af sequence of touples, remake this as two new Lists.
        let (individuals , fitnesses) = population
        let population' = List.ofSeq (
                            individuals
                            |> Seq.map (fun individual -> loop individual fitnesses goal configuration))
        // The function returns a sequence of individuals and a sequence of lists of fitnesses
        // Since a Population is defined as a sequence of individuals, and a sequence containing the best individuals fitness
        // We will need to convert them back
        let (_,fitness') = population'.Head
        let (individuals') = List.ofSeq (population' |> Seq.map (fun (individual , _) -> individual))
        // Return the new Island.
        let island' : Island = (individuals',fitness') , configuration
        island'