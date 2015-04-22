namespace EvolutionaryAlgoritms
open MathNet.Numerics
open MathNet.Numerics.Distributions
open MathNet.Numerics.LinearAlgebra
open ToolBox
open MoveMent

module public VariableNeighbourhoodSearch =
    let mutable k = 0
    let mutable numRunsForMean = 0
    let mutable maxIterations = 0
    let mutable mu = 0
    let mutable lambda = 0
    let mutable numGenerationsBeforeMerge = 0
    let mutable bestParents = []
    let rand = new System.Random()
    
    let rec loop original generation islands i =  
        let newIslands =
            islands
            //Run Mu+Lambda on each individual for numGenerationsBeforeMerge times
            |> Seq.map (fun island -> 
                let mutable newIsland = []
                for generation in 0..numGenerationsBeforeMerge do
                    let (tmpIsland,_) = (MuCommaLambda.runWithParents original island maxIterations mu lambda)
                    newIsland <- tmpIsland
                newIsland)
            //Merge the best individual from each Island to the next in the List.
            |> Seq.map (fun island -> 
                bestParents <- List.append bestParents [island.Head]
                island)
            |> Seq.map (fun island -> List.append island [bestParents.[rand.Next(0,List.length bestParents)]])
        //Reloop or Stop
        if i < maxIterations then
            printf "%A\n" i
            loop original (generation+1) newIslands (i+1)
        else
        //Returns all the Islands
            newIslands


    let runWithArguments (M : Matrix<double>) S k' numRunsForMean' maxIterations' mu' lambda' numGenerationsBeforeMerge' = 
        k <- k'
        numRunsForMean <- numRunsForMean'
        maxIterations <- maxIterations'
        mu <- mu'
        lambda <- lambda'
        numGenerationsBeforeMerge <- numGenerationsBeforeMerge'
        //Generate n islands with a random individual
        let n = 4
        let islands = 
            seq {0..n}
            |> Seq.map (fun _ -> 
                let (parent,_) = (MuCommaLambda.runWithArguments M S maxIterations mu lambda)
                parent)
            |> List.ofSeq
        //loop them
        loop M 0 islands 0