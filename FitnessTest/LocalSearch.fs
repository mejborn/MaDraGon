namespace EvolutionaryAlgoritms
open MathNet.Numerics
open MathNet.Numerics.Distributions
open MathNet.Numerics.LinearAlgebra
open ToolBox
open MoveMent

module public LocalSearch =
    let mutable numIterations = 0

    let rec loop (original : Matrix<double>) (solution : Matrix<double>) (fitnessList : List<double>) (maxIterations : int) =
        if (numIterations = maxIterations) then
            fitnessList
        else
            let N = original.RowCount
            let fitness = FitTest.doFitTest original solution
            let mutable newSolution  = solution
            for i in 0..N-1 do
                for j in 0..3 do
                    let mutable tempSolution  = solution
                    match j with
                    |0 -> tempSolution <- MoveMent.MakeMove solution (MoveMent.Move(MoveMent.Left,i))
                    |1 -> tempSolution <- MoveMent.MakeMove solution (MoveMent.Move(MoveMent.Right,i))
                    |2 -> tempSolution <- MoveMent.MakeMove solution (MoveMent.Move(MoveMent.Up,i))
                    |3 -> tempSolution <- MoveMent.MakeMove solution (MoveMent.Move(MoveMent.Down,i))
                    if(FitTest.doFitTest original tempSolution < FitTest.doFitTest original newSolution) then
                        newSolution <- tempSolution
            numIterations <- numIterations + 1
            loop original newSolution (List.append fitnessList [FitTest.doFitTest original newSolution]) maxIterations
    
    let runWithArguments original solution maxIterations =
        loop original solution [] maxIterations