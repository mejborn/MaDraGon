namespace EvolutionaryAlgoritms
open MathNet.Numerics
open MathNet.Numerics.Distributions
open MathNet.Numerics.LinearAlgebra
open ToolBox
open MoveMent

module MuPlusLambda = 
    type System.Random with
        /// Generates an infinite sequence of random numbers within the given range.
        member this.GetValues(minValue, maxValue) =
            Seq.initInfinite (fun _ -> this.Next(minValue, maxValue))
    let rnd = System.Random()
    
    let mutable mu = 3
    let mutable lambda = 3
    let mutable numIterations = 0
    let mutable numIterationsWithoutMove = 0
    let maxIterationsWithoutMove = 1000*lambda
    let mutable originalSolution : List<Matrix<double>> = [DenseMatrix.init 1 1 (fun i j -> 0.0)]

    let rec loop (original : Matrix<double>) (parents : List<Matrix<double>>) (fitnessList : List<double>) (maxIterations : int) =
        let N = original.RowCount
        //Generate lambda new children from random mu parents
        let mutable newGeneration = []
        //Add the parents to the population
        for parent in parents do
            newGeneration <- List.append newGeneration [(FitTest.doFitTest original parent,parent)]
        for i in 0..lambda do
            //Generate random solution:
            let k = Poisson.Sample(1.0)
            let map = parents.[rnd.Next(0,parents.Length-1)]
            let (mutation,step) = MoveMent.ScrambleMap map map.RowCount (Poisson.Sample(1.0))
            let fitness = FitTest.doFitTest original mutation
            newGeneration <- List.append newGeneration [(fitness,mutation)]
        //Keep mu best generations
        let newParents =
                        newGeneration
                        |> Seq.sortBy (fun (fitness,_) -> fitness)
                        |> if (newGeneration.Length > mu-1) then 
                            Seq.take mu else 
                            Seq.take newGeneration.Length
                        |> Seq.map (fun (_,subject) -> subject)
                        |> List.ofSeq
            
        //Generate new fitness list for the new parents
        let fitness = FitTest.doFitTest original newParents.Head
        let newFitnessList = List.append fitnessList [fitness]
        
        if (fitnessList.Length <> 0) && (fitness >= fitnessList.[fitnessList.Length-1]) then
                numIterationsWithoutMove <- numIterationsWithoutMove + 1
        //Run the loop again
        numIterations <- numIterations + 1
        if (fitness = 0.0) then
            (newFitnessList,parents.[0])
        else if numIterationsWithoutMove > maxIterationsWithoutMove then
            //restart
            printf "Restarting at iteration no: %A\n" numIterations
            numIterationsWithoutMove <- 0
            loop original originalSolution [] maxIterations
        else
            loop original newParents newFitnessList maxIterations

    let runWithArguments original solution maxIterations mu' lambda' =
        numIterations <- 0
        numIterationsWithoutMove <- 0
        mu <- mu'
        lambda <- lambda'
        originalSolution <- solution
        loop original solution [] maxIterations