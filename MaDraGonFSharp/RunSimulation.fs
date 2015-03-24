namespace Madragon
open MathNet.Numerics
open MathNet.Numerics.LinearAlgebra

module RunSimulation =
    let SimulatedAnnealing (M : Matrix<double>) S k numRunsForMean maxIterations temperature cooling =
        let mutable results : double array = Array.zeroCreate maxIterations
        let mutable resultArray : List<double> array = Array.zeroCreate numRunsForMean
        let N = M.RowCount

        for i in 0..numRunsForMean-1 do
            printfn "Simulated Annealing Run no: %A" i
            let simulatedAnnealingResult = (EvolutionaryAlgoritms.SimulatedAnnealing.runWithArguments M S temperature cooling maxIterations)
            resultArray.[i] <- simulatedAnnealingResult
        
        for i in 0..resultArray.Length-1 do
            for j in 0..resultArray.[i].Length-1 do
                results.[j] <- results.[j] + resultArray.[i].[j]
        for i in 0..results.Length-1 do
            results.[i] <- float results.[i] / float numRunsForMean

        let file = System.IO.File.AppendText("Output\SimulatedAnnealing_" + "N_" + N.ToString() + "_k_" + k.ToString() + "_Temperature_" + temperature.ToString() + ".txt")
        for i in 0..results.Length-1 do
            file.WriteLine(i.ToString() + " " + results.[i].ToString())
        file.Flush()
        file.Close()

    let MuPlusLambda (M : Matrix<double>) S k numRunsForMean maxIterations mu lambda =
        let mutable results : double array = Array.zeroCreate maxIterations
        let mutable resultArray : List<double> array = Array.zeroCreate numRunsForMean
        let N = M.RowCount
        
        for i in 0..numRunsForMean-1 do
            printfn "Mu Plus Lambda Run no: %A" i
            let muPlusLambdaResult = (EvolutionaryAlgoritms.MuPlusLambda.runWithArguments M [S] maxIterations mu lambda)
            resultArray.[i] <- muPlusLambdaResult

        for i in 0..resultArray.Length-1 do
            for j in 0..resultArray.[i].Length-1 do
                results.[j] <- results.[j] + resultArray.[i].[j]
        for i in 0..results.Length-1 do
            results.[i] <- float results.[i] / float numRunsForMean

        let file = System.IO.File.AppendText("Output\MuPlusLambda_" + "N_" + N.ToString() + "_k_" + k.ToString() + "_Mu_" + mu.ToString() + "_Lambda_" + mu.ToString() + ".txt")
        for i in 0..results.Length-1 do
            file.WriteLine(i.ToString() + " " + results.[i].ToString())
        file.Flush()
        file.Close()

    let MuCommaLambda (M : Matrix<double>) S k numRunsForMean maxIterations mu lambda =
        let mutable results : double array = Array.zeroCreate maxIterations
        let mutable resultArray : List<double> array = Array.zeroCreate numRunsForMean
        let N = M.RowCount

        for i in 0..numRunsForMean-1 do
            printfn "Mu Plus Lambda Run no: %A" i
            let muPlusLambdaResult = (EvolutionaryAlgoritms.MuPlusLambda.runWithArguments M [S] maxIterations mu lambda)
            resultArray.[i] <- muPlusLambdaResult

        for i in 0..resultArray.Length-1 do
            for j in 0..resultArray.[i].Length-1 do
                results.[j] <- results.[j] + resultArray.[i].[j]
        for i in 0..results.Length-1 do
            results.[i] <- float results.[i] / float numRunsForMean

        let file = System.IO.File.AppendText("Output\MuPlusLambda_" + "N_" + N.ToString() + "_k_" + k.ToString() + "_Mu_" + mu.ToString() + "_Lambda_" + mu.ToString() + ".txt")
        for i in 0..results.Length-1 do
            file.WriteLine(i.ToString() + " " + results.[i].ToString())
        file.Flush()
        file.Close()

    let localSearch (M : Matrix<double>) S k = 
        let N = M.RowCount
        let localSearchResult = (EvolutionaryAlgoritms.LocalSearch.runWithArguments M S k)

        let file = System.IO.File.AppendText("Output\LocalSearch" + "_N_" + N.ToString() + "_k_" + k.ToString() + ".txt")
        for i in 0..localSearchResult.Length-1 do
            file.WriteLine(i.ToString() + " " + localSearchResult.[i].ToString())
        file.Flush()
        file.Close()