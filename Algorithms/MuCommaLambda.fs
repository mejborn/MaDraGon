namespace EvolutionaryAlgoritms
open MathNet.Numerics
open MathNet.Numerics.Distributions
open MathNet.Numerics.LinearAlgebra
open ToolBox
open MoveMent

module MuCommaLambda = 
    type System.Random with
        /// Generates an infinite sequence of random numbers within the given range.
        member this.GetValues(minValue, maxValue) =
            Seq.initInfinite (fun _ -> this.Next(minValue, maxValue))
    let rnd = System.Random()
    
    let mutable mu = 3
    let mutable lambda = 3
    let mutable numIterations = 0

    let rec loop (original : Matrix<double>) (parents : List<(float * Matrix<double> * seq<MoveMent.move>)>) (fitnessList : List<double>) (maxIterations : int) =
        let N = original.RowCount
        //Generate lambda new children from random mu parents
        let mutable newGeneration = []

        //Generate lambda random mutation from random parent. Return as list of fitness,mutation,Sequence of moves.
        for i in 0..lambda do
            //Generate random solution:
            let k = Poisson.Sample(1.0)
            let (_,parent,moves) = parents.[rnd.Next(0,parents.Length-1)]
            let (mutation,newMoves) = MoveMent.ScrambleMap parent parent.RowCount (Poisson.Sample(1.0))
            let fitness = FitTest.doFitTest original mutation
            newGeneration <- List.append newGeneration [(fitness,mutation,(Seq.append moves newMoves))]
        //Keep mu best generations
        let newParents =
                        newGeneration
                        |> Seq.sortBy (fun (fitness,_,_) -> fitness)
                        |> if (newGeneration.Length > mu-1) then 
                            Seq.take mu else 
                            Seq.take newGeneration.Length
                        //|> Seq.map (fun (_,subject,_) -> subject)
                        |> List.ofSeq
            
        //Generate new fitness list from the best available parent
        let mutable newFitnessList = fitnessList
        let (_,bestParent,_) = newParents.Head
        newFitnessList <- List.append newFitnessList [FitTest.doFitTest original bestParent]
        //Run the loop again
        numIterations <- numIterations + 1
        if (numIterations = maxIterations) then
            (newParents,newFitnessList)
        else
            loop original newParents newFitnessList maxIterations

    let runWithArguments original solution maxIterations mu' lambda' =
        numIterations <- 0
        mu <- mu'
        lambda <- lambda'
        loop original [0.0 ,solution , Seq.empty] [] maxIterations

    let runWithParents original parents maxIterations mu' lambda' = 
        numIterations <- 0
        mu <- mu'
        lambda <- lambda'
        loop original parents [] maxIterations