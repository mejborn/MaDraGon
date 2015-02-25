namespace ToolBox
open MathNet.Numerics
open MathNet.Numerics.Distributions
open MathNet.Numerics.LinearAlgebra
open FSharp.Collections.ParallelSeq
open FSharp.Charting
open System.Drawing

module public FitTest =
    let Hamming (original : double[][]) solution =
        Distance.Hamming(Array.concat original , Array.concat solution)
    
    let Manhattan (original : double[][]) solution =
        PSeq.zip original solution
        |> PSeq.sumBy (fun (row1, row2) -> Distance.Manhattan(row1,row2))

    let NQubed (original : double[][]) (solution : double[][]) =
        let N = original.Length

        let CalculateDistanceBetweenPoints (x1,y1) (x2,y2) =
            let distance p1 p2 =
                if abs p1-p2 <= N/2 then
                    abs (p1-p2)
                else 
                    if N-p1 < N-p2 then
                        N-p1+p2
                    else
                        N-p2+p1
            (distance x1 x2) + (distance y1 y2)

        let FindDistancesForOnePoint (x,y) =
            let mutable distances = []
            for row in 0..N-1 do
                for col in 0..N-1 do
                    if original.[row].[col] = 1.0 then
                        let distance = CalculateDistanceBetweenPoints (row,col) (x,y)
                        distances <- List.append distances [distance,(row,col),(x,y)]
            distances

        let FindDistancesForAllPoints =
            let mutable distanceList = []
            for row in 0..N-1 do
                for col in 0..N-1 do
                    if solution.[row].[col] = 1.0 then
                        let distances = FindDistancesForOnePoint (row,col)
                        distanceList <- List.append distanceList distances
            List.sort distanceList
        
        let rec CalculateTotalDistance (distanceList : List<int * (int32 * int32) * (int32 * int32)>) currentDistance =
            let distance , pointInOriginal , pointInScrambled = distanceList.Head
            let totalDistance = currentDistance + distance

            let mutable newDistanceList = []
            for item in distanceList do
                let _ , point1, point2  = item
                if (point1 <> pointInOriginal && point2 <> pointInScrambled) then
                    newDistanceList <- List.append newDistanceList [item]
            match newDistanceList with
            |[] -> double totalDistance
            |_ -> CalculateTotalDistance newDistanceList totalDistance

        CalculateTotalDistance FindDistancesForAllPoints 0
            

    let doFitTest (original : Matrix<double>) (solution : Matrix<double>) =
        let originalArray = original.ToColumnArrays()
        let solutionArray = solution.ToColumnArrays()
        
        Hamming originalArray solutionArray
