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
    let Temperature = 100.0
    let MinTemp = 0.0001
    let Cooling = 0.001
    let lambda = Constants.E
    
    let rec loop (original : Matrix<double>) solution temperature (fitnessList : List<double>) =
        let N = original.RowCount
        
        //Calculate current solutions fitness:
        let Fitness = FitTest.doFitTest original solution
        
        //Generate random sollution:
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
            temperature-temperature*Cooling
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
            NewFitnessList
        else if(rnd.NextDouble() <= AcceptanceProbability && temperature > MinTemp) then
            loop original NewSolution NewTemperature NewFitnessList
        //Use old fitnesslist since new fitness isnt usefull
        else if (temperature > MinTemp) then
            loop original solution NewTemperature fitnessList
        else
            fitnessList

    let run original solution =
        loop original solution Temperature []