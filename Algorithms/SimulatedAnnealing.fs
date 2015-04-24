namespace EvolutionaryAlgoritms
open MathNet.Numerics
open MathNet.Numerics.Distributions
open MathNet.Numerics.LinearAlgebra
open ToolBox
open MoveMent

module SimulatedAnnealing =
    type System.Random with
        /// Generates an infinite sequence of random numbers within the given range.
        member this.GetValues(minValue, maxValue) =
            Seq.initInfinite (fun _ -> this.Next(minValue, maxValue))

    let rnd = System.Random()
    let lambda = Constants.E
    let mutable numIterations = 0
    let mutable numIterationsWithoutMove = 0
    let maxIterationsWithoutMove = 1000
    let mutable originalSolution : Matrix<double> = DenseMatrix.init 1 1 (fun i j -> 0.0)
    let mutable originalTemperature = 0.0

    let rec loop (original : Matrix<double>) (solution : Matrix<double>) (fitnessList : List<double>) temperature cooling maxIterations =
        let N = original.RowCount
        numIterations <- numIterations + 1
        //Calculate current solutions fitness:
        let Fitness = FitTest.doFitTest original solution

        //Generate random solution:
        let k = Poisson.Sample(lambda)
        
        let (NewSolution,moves) =
            MoveMent.ScrambleMap solution N k
        
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

        if (fitnessList.Length <> 0) && (Fitness >= fitnessList.[fitnessList.Length-1]) then
                numIterationsWithoutMove <- numIterationsWithoutMove + 1

        if NewFitness = 0.0 then
            printfn "%A %A" original NewSolution
            NewFitnessList
        else if numIterationsWithoutMove > maxIterationsWithoutMove then
            //restart
            printf "Restarting at iteration no: %A\n" numIterations
            numIterationsWithoutMove <- 0
            loop original originalSolution [] originalTemperature cooling maxIterations
        else if(rnd.NextDouble() <= AcceptanceProbability && numIterations < maxIterations) then
            loop original NewSolution NewFitnessList NewTemperature cooling maxIterations
        else
            loop original solution NewFitnessList NewTemperature cooling maxIterations
       

    let runWithArguments original solution temperature cooling maxIterations =
        originalTemperature <- temperature
        originalSolution <- solution
        numIterations <- 0
        loop original solution [] temperature cooling maxIterations