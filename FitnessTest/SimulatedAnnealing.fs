namespace EvolutionaryAlgoritms
open MathNet.Numerics
open MathNet.Numerics.Distributions
open MathNet.Numerics.LinearAlgebra
open ToolBox
open MoveMent

module public SimulatedAnnealing =
    type System.Random with
        /// Generates an infinite sequence of random numbers within the given range.
        member this.GetValues(minValue, maxValue) =
            Seq.initInfinite (fun _ -> this.Next(minValue, maxValue))

    let rnd = System.Random()
<<<<<<< HEAD
    let Temperature = 100.0
    let MinTemp = 0.00001
    let Cooling = 0.01
=======
    let mutable numIterations = 0
>>>>>>> origin/master
    let lambda = 1.0
    
    let rec loop (original : Matrix<double>) (solution : Matrix<double>) (fitnessList : List<double>) temperature cooling maxIterations =
        let N = original.RowCount
        numIterations <- numIterations + 1
        //Calculate current solutions fitness:
        let Fitness = FitTest.doFitTest original solution
        
        //Display progress in console
        //printfn "Current temperature: %A \nIteration count    : %A \nCurrent fitness    : %A \n" temperature numIterations Fitness

        //Generate random solution:
        let k = Poisson.Sample(lambda)
        
        let NewSolution =
            let directions = 
                rnd.GetValues(0,3)
                |> Seq.take k
                |> Seq.map (fun n ->
                    match n with
                    | 0 -> MoveMent.direction.Up
                    | 1 -> MoveMent.direction.Down
                    | 2 -> MoveMent.direction.Right
                    | _ -> MoveMent.direction.Left)
            let rowcols = rnd.GetValues(0,N-1) |> Seq.take k
            let moves = Seq.map2 (fun direction rowcol -> MoveMent.Move(direction,rowcol)) directions rowcols
            Seq.fold (fun M move -> (MoveMent.MakeMove M move)) solution moves
        
        //Generate new fitness from data      
        let NewFitness =
            FitTest.doFitTest original NewSolution
        let NewTemperature =
            temperature-temperature*cooling
        let NewFitnessList =
            List.append fitnessList [NewFitness]

        let AcceptanceProbability =
            if (Fitness > NewFitness) then
                1.0
            else if (Fitness-NewFitness = 0.0) then
                0.25
            else
                Constants.E ** ((Fitness-NewFitness)/temperature)

        if NewFitness = 0.0 then
            //printfn "%A %A" original NewSolution
            NewFitnessList
        else if(rnd.NextDouble() <= AcceptanceProbability && numIterations < maxIterations) then
            loop original NewSolution NewFitnessList NewTemperature cooling maxIterations
        //Use old fitnesslist since new fitness isnt usefull
<<<<<<< HEAD
        else 
            loop original solution NewTemperature fitnessList
=======
        else if (numIterations < maxIterations) then
            loop original solution fitnessList NewTemperature cooling maxIterations
        else
            //printfn "%A %A" original solution
            fitnessList
>>>>>>> origin/master

    let run original solution =
        let temperature = 100.0
        let cooling = 0.01
        let maxIterations = 100000

        loop original solution [] temperature  cooling maxIterations

    let runWithArguments original solution temperature cooling maxIterations =
        numIterations <- 0
        loop original solution [] temperature cooling maxIterations
