namespace Algorithms
//open MathNet.Numerics.LinearAlgebra
//
module public RunSimulation =
    let run =
        printf "HEj."
//    
//    let SimulatedAnnealing (M : Matrix<double>) S k numRunsForMean maxIterations temperature cooling =
//        let mutable results : double array = Array.zeroCreate maxIterations
//        let mutable resultArray : List<double> array = Array.zeroCreate numRunsForMean
//        let N = M.RowCount
//
//        for i in 0..numRunsForMean-1 do
//            printfn "Simulated Annealing Run no: %A" i
//            let simulatedAnnealingResult = (EvolutionaryAlgoritms.SimulatedAnnealing.runWithArguments M S temperature cooling maxIterations)
//            resultArray.[i] <- simulatedAnnealingResult
//        
//        for i in 0..resultArray.Length-1 do
//            for j in 0..resultArray.[i].Length-1 do
//                results.[j] <- results.[j] + resultArray.[i].[j]
//        for i in 0..results.Length-1 do
//            results.[i] <- float results.[i] / float numRunsForMean
//
//        let file = System.IO.File.AppendText("Output\SimulatedAnnealing_" + "N_" + N.ToString() + "_k_" + k.ToString() + "_Temperature_" + temperature.ToString() + ".txt")
//        for i in 0..results.Length-1 do
//            file.WriteLine(i.ToString() + " " + results.[i].ToString())
//        file.Flush()
//        file.Close()
//
//    let MuPlusLambda (M : Matrix<double>) S k numRunsForMean maxIterations mu lambda =
//        let mutable results : double array = Array.zeroCreate maxIterations
//        let mutable resultArray : List<double> array = Array.zeroCreate numRunsForMean
//        let N = M.RowCount
//        
//        for i in 0..numRunsForMean-1 do
//            printfn "Mu + Lambda Run no: %A" i
//            let (muPlusLambdaResult,_) = (EvolutionaryAlgoritms.MuPlusLambda.runWithArguments M [S] maxIterations mu lambda)
//            resultArray.[i] <- muPlusLambdaResult
//
//        for i in 0..resultArray.Length-1 do
//            for j in 0..resultArray.[i].Length-1 do
//                results.[j] <- results.[j] + resultArray.[i].[j]
//        for i in 0..results.Length-1 do
//            results.[i] <- float results.[i] / float numRunsForMean
//
//        let file = System.IO.File.AppendText("Output\MuPlusLambda_" + "N_" + N.ToString() + "_k_" + k.ToString() + "_Mu_" + mu.ToString() + "_Lambda_" + lambda.ToString() + ".txt")
//        for i in 0..results.Length-1 do
//            file.WriteLine(i.ToString() + " " + results.[i].ToString())
//        file.Flush()
//        file.Close()
//
//    let MuCommaLambda (M : Matrix<double>) S k numRunsForMean maxIterations mu lambda =
//        let mutable results : double array = Array.zeroCreate maxIterations
//        let mutable resultArray : List<double> array = Array.zeroCreate numRunsForMean
//        let N = M.RowCount
//
//        for i in 0..numRunsForMean-1 do
//            printfn "Mu , Lambda Run no: %A" i
//            let muPlusLambdaResult = (EvolutionaryAlgoritms.MuPlusLambda.runWithArguments M [S] maxIterations mu lambda)
//            resultArray.[i] <- muPlusLambdaResult
//
//        for i in 0..resultArray.Length-1 do
//            for j in 0..resultArray.[i].Length-1 do
//                results.[j] <- results.[j] + resultArray.[i].[j]
//        for i in 0..results.Length-1 do
//            results.[i] <- float results.[i] / float numRunsForMean
//
//        let file = System.IO.File.AppendText("Output\MuCommaLambda_" + "N_" + N.ToString() + "_k_" + k.ToString() + "_Mu_" + mu.ToString() + "_Lambda_" + lambda.ToString() + ".txt")
//        for i in 0..results.Length-1 do
//            file.WriteLine(i.ToString() + " " + results.[i].ToString())
//        file.Flush()
//        file.Close()
//
//    let VariableNeighbourhoodSeach (M : Matrix<double>) S k numRunsForMean maxIterations mu lambda numGenerations =
//        let mutable results = Array.zeroCreate 0
//        let mutable resultArray : List<double> array = Array.zeroCreate numRunsForMean
//        let N = M.RowCount
//
//        for i in 0..numRunsForMean-1 do
//            printfn "Variable Neighbourhood Search Run no: %A" i
//            //Gets a List of islands, containing a fitness list and the individuals on the island.
//            let islands = List.ofSeq (EvolutionaryAlgoritms.VariableNeighbourhoodSearch.runWithArguments M S k numRunsForMean maxIterations mu lambda numGenerations)
//            results <- Array.zeroCreate islands.Length
//            for j in 0..islands.Length do
//                results.[j] <- 
//                    islands
//                    |> List.map (fun island ->
//                        let results' : double array = Array.zeroCreate island.Length
//                        for k in 0..island.Length-1 do
//                            let (fitness,_,_) = island.[k]
//                            results'.[k] <- fitness
//                        results')
//
//        let mutable tmpresult = Array.zeroCreate results.[0].Length
//        for island in results do
//            for i in 0..island.Length-1 do
//                for j in 0..island.[i].Length-1 do
//                    tmpresult.[j] <- tmpresult.[j] + island.[i].[j]
//            for i in 0..tmpresult.Length-1 do
//                tmpresult.[i] <- float tmpresult.[i] / float numRunsForMean
//            let file = System.IO.File.AppendText("Output\VariableNeighbourhoodSearch_" + "r_" + island.ToString() + "N_" + N.ToString() + "_k_" + k.ToString() + "_Mu_" + mu.ToString() + "_Lambda_" + lambda.ToString() + ".txt")
//            for i in 0..results.Length-1 do
//                file.WriteLine(i.ToString() + " " + results.[i].ToString())
//            file.Flush()
//            file.Close()
//
//    let localSearch island : World.Island =
//        EvolutionaryAlgoritms.LocalSearch M S k