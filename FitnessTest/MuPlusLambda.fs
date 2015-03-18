namespace EvolutionaryAlgoritms
open MathNet.Numerics
open MathNet.Numerics.Distributions
open MathNet.Numerics.LinearAlgebra
open ToolBox
open MoveMent

module public MuPlusLambda = 
    type System.Random with
        /// Generates an infinite sequence of random numbers within the given range.
        member this.GetValues(minValue, maxValue) =
            Seq.initInfinite (fun _ -> this.Next(minValue, maxValue))
    let rnd = System.Random()
    
    let mutable mu = 3
    let mutable lambda = 3
    let mutable numIterations = 0

    let rec loop (original : Matrix<double>) (parents : List<Matrix<double>>) (fitnessList : List<double>) (maxIterations : int) =
        let N = original.RowCount
        //Generate lambda new children from random mu parents
        let mutable newGeneration = []
        for parent in parents do
            newGeneration <- List.append newGeneration [(FitTest.doFitTest original parent,parent)]
        for i in 0..lambda do
            //Generate random solution:
            let k = Poisson.Sample(1.0)
            let map = parents.[rnd.Next(0,parents.Length-1)]
            let mutation = MoveMent.ScrambleMap map map.RowCount (Poisson.Sample(1.0))
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
        let mutable newFitnessList = fitnessList
        for parent in newParents do
            newFitnessList <- List.append newFitnessList [FitTest.doFitTest original parent]
        //Run the loop again
        numIterations <- numIterations + 1
        if (numIterations = maxIterations) then
            newFitnessList
        else
            loop original newParents newFitnessList maxIterations

    let runWithArguments original solution maxIterations mu' lambda' =
        numIterations <- 0
        mu <- mu'
        lambda <- lambda'
        loop original solution [] maxIterations