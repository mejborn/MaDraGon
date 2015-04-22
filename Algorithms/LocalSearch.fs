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
    
    let rec LocalSearch (original : Matrix<double>) (solution : Matrix<double>) (fitnessList : List<double>) (maxSteps : int) previousMoves backStep =
        printf "Local Search Step No: %A " numSteps 
        printfn "Current fitness: %A" (FitTest.doFitTest original solution)

        if (numSteps >= maxSteps || (not fitnessList.IsEmpty && fitnessList.Head = 0.0)) then
            printfn "%A" original
            printfn "%A" solution
            fitnessList
        else
            numSteps <- numSteps + 1
            let N = original.RowCount
            let fitness = FitTest.doFitTest original solution
            let mutable newSolution = solution
            let mutable newSolution2 = solution
            let mutable tempSolution  = solution
            let mutable tempSolution2 = solution
            for i in 0..N-1 do
                for j in 0..3 do
                    match j with
                    |0 -> tempSolution <- MoveMent.MakeMove solution (MoveMent.Move(MoveMent.Left,i))
                    |1 -> tempSolution <- MoveMent.MakeMove solution (MoveMent.Move(MoveMent.Right,i))
                    |2 -> tempSolution <- MoveMent.MakeMove solution (MoveMent.Move(MoveMent.Up,i))
                    |_ -> tempSolution <- MoveMent.MakeMove solution (MoveMent.Move(MoveMent.Down,i))
                    if(FitTest.doFitTest original tempSolution < FitTest.doFitTest original newSolution) then
                        newSolution2 <- newSolution
                        newSolution <- tempSolution
            if backStep then
                printfn "Going Back..."
                newSolution <- newSolution2

            if newSolution <> solution then
                printfn "Solution differs. Walking forward.."
                let moves = List.append [newSolution] previousMoves
                LocalSearch original newSolution (List.append fitnessList [FitTest.doFitTest original newSolution]) maxSteps moves false
            else
                //Go 1 back -> Should now take the next best solution
                let moves = List.ofSeq (Seq.skip 1 previousMoves)
                printfn "%A" moves.Head
                let oldSolution = previousMoves.Head
                LocalSearch original oldSolution (List.append fitnessList [FitTest.doFitTest original newSolution]) maxSteps moves true
    
    let runWithArguments original solution maxSteps =
        LocalSearch original solution [] maxSteps [] false