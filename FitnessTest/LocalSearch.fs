namespace EvolutionaryAlgoritms
open MathNet.Numerics
open MathNet.Numerics.Distributions
open MathNet.Numerics.LinearAlgebra
open ToolBox
open MoveMent
open FSharp.Collections.ParallelSeq

module public LocalSearch =
    let mutable numIterations = 0
    let mutable numSteps = 0
    let rnd = System.Random()

    let rec simulatedAnnealing (original : Matrix<double>) (solution : Matrix<double>) (fitnessList : List<double>) maxIterations =
        let N = original.RowCount
        numIterations <- numIterations + 1
        //Calculate current solutions fitness:
        let Fitness = FitTest.doFitTest original solution

        //Generate random solution:
        let k = Poisson.Sample(1.0)
        let NewSolution =
            MoveMent.ScrambleMap solution N k
        
        //Generate new fitness from data      
        let NewFitness =
            FitTest.doFitTest original NewSolution

        let AcceptanceProbability =
            if (Fitness > NewFitness) then
                1.0
            else
                0.0

        if NewFitness = 0.0 then
            (List.append fitnessList [NewFitness],NewSolution)
        else if(rnd.NextDouble() <= AcceptanceProbability && numIterations < maxIterations) then
            simulatedAnnealing original NewSolution (List.append fitnessList [NewFitness]) maxIterations
        else if (numIterations < maxIterations) then
            simulatedAnnealing original solution fitnessList maxIterations
        else
            (List.append fitnessList [NewFitness],NewSolution)

    let rec LocalSearch (original : Matrix<double>) (solution : Matrix<double>) (fitnessList : List<double>) (maxSteps : int) =
        if (numSteps >= maxSteps || (not fitnessList.IsEmpty && fitnessList.Head = 0.0)) then
            printfn "%A" original
            printfn "%A" solution
            System.Console.ReadLine()
            fitnessList
        else
            let N = original.RowCount
            let fitness = FitTest.doFitTest original solution
            let mutable newSolution = solution
            let mutable tempSolution  = solution
            for i in 0..N-1 do
                for j in 0..3 do
                    match j with
                    |0 -> tempSolution <- MoveMent.MakeMove solution (MoveMent.Move(MoveMent.Left,i))
                    |1 -> tempSolution <- MoveMent.MakeMove solution (MoveMent.Move(MoveMent.Right,i))
                    |2 -> tempSolution <- MoveMent.MakeMove solution (MoveMent.Move(MoveMent.Up,i))
                    |3 -> tempSolution <- MoveMent.MakeMove solution (MoveMent.Move(MoveMent.Down,i))
                    if(FitTest.doFitTest original tempSolution < FitTest.doFitTest original newSolution) then
                        newSolution <- tempSolution
            //Try with SimulatedAnnealing
            if (newSolution = solution) then
                numIterations <- 0
                let (simulatedAnnealingList,newSolution2) = simulatedAnnealing original solution [] 10000
                numSteps <- numSteps + simulatedAnnealingList.Length-1
                newSolution <- newSolution2
         
            printf "Local Search Step No: %A " numSteps 
            printfn "Current fitness: %A" (FitTest.doFitTest original newSolution)
            numSteps <- numSteps + 1
            LocalSearch original newSolution (List.append fitnessList [FitTest.doFitTest original newSolution]) maxSteps
    
    let runWithArguments original solution maxIterations =
        numIterations <- 0
        LocalSearch original solution [] maxIterations