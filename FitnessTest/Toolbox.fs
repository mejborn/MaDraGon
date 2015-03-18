namespace ToolBox
open MathNet.Numerics
open MathNet.Numerics.Distributions
open MathNet.Numerics.LinearAlgebra
open FSharp.Collections.ParallelSeq
open FSharp.Charting
open System.Drawing

module public FitTest =
    let rec Density (original : double[][]) (solution : double[][]) =
        let N = int original.Length
        
        let fitness =
            match N with
            |0 -> 0.0
            |1 -> if (original.[0].[0] = solution.[0].[0]) then 0.0 else 1.0  
            |_ -> 
                (Density original.[..(N/2)-1].[..(N/2-1)] solution.[..(N/2)-1].[..(N/2)-1]) +
                (Density original.[..(N/2)-1].[(N/2)..] solution.[..(N/2)-1].[(N/2)..]) +
                (Density original.[(N/2)..].[..(N/2)-1] solution.[(N/2)..].[..(N/2)-1]) +
                (Density original.[(N/2)..].[(N/2)..] solution.[(N/2)..].[(N/2)..])

        let sumOriginal = Seq.concat original |> Seq.sumBy (fun value -> if (value =  1.0) then 1.0 else 0.0) 
        let sumSolution = Seq.concat solution |> Seq.sumBy (fun value -> if (value =  1.0) then 1.0 else 0.0)

        abs (sumOriginal - sumSolution) + fitness
    
    let Hamming (original : double[][]) solution =
        Distance.Hamming(Array.concat original , Array.concat solution)/2.0
    
    let Manhattan (original : double[][]) solution =
        PSeq.zip original solution
        |> PSeq.sumBy (fun (row1, row2) -> Distance.Manhattan(row1,row2))

    let NQubed (original : double[][]) (solution : double[][]) =
        let N = original.Length

        let CalculateDistanceBetweenPoints (x1,y1) (x2,y2) =
            let distance p1 p2 =
                if abs (p1-p2) <= N/2 then
                    abs (p1-p2)
                else 
                    if N-p1 < N-p2 then
                        N-p1+p2
                    else
                        N-p2+p1
            (distance x1 x2) + (distance y1 y2)
       
        let FindDistancesForOnePoint (x,y) = 
            [for i in 0..N-1 do
             for j in 0..N-1 do
             yield (i,j)]
            |> PSeq.map (fun (row,col) -> 
                if original.[row].[col] = 1.0 then
                    let distance = CalculateDistanceBetweenPoints (row,col) (x,y) 
                    distance,(row,col),(x,y)
                else
                    0,(0,0),(0,0))
            |> PSeq.toList
   
        let FindDistancesForAllPoints = 
            [for i in 0..N-1 do
             for j in 0..N-1 do
             yield (i,j)]
            |> PSeq.map (fun (row,col) -> 
                if solution.[row].[col] = 1.0 then
                    FindDistancesForOnePoint (row,col)
                else
                    [0,(0,0),(0,0)])
            |> PSeq.concat
            |> PSeq.sort
            |> PSeq.toList
        
        let newDistanceList (distanceList : List<int * (int * int) * (int * int)>) currentDistance =
            let distance , pointInOriginal , pointInScrambled = distanceList.Head
            let totalDistance = currentDistance + distance

            distanceList
            |> PSeq.filter (fun item ->
                let _ , point1 , point2 = item
                (point1 <> pointInOriginal && point2 <> pointInScrambled))

        let rec CalculateTotalDistance (distanceList : List<int * (int * int) * (int * int)>) currentDistance =
            let distance , pointInOriginal , pointInScrambled = distanceList.Head
            let totalDistance = currentDistance + distance

            let newDistanceList = 
                distanceList
                |> PSeq.filter (fun item ->
                    let _ , point1 , point2 = item
                    ((point1 <> pointInOriginal) && (point2 <> pointInScrambled)))
                |> PSeq.sort
                |> PSeq.toList
            
            match newDistanceList with
            |[] -> double totalDistance
            |_ -> CalculateTotalDistance newDistanceList totalDistance

        CalculateTotalDistance FindDistancesForAllPoints 0
            

    let doFitTest (original : Matrix<double>) (solution : Matrix<double>) =
        let originalArray = original.ToColumnArrays()
        let solutionArray = solution.ToColumnArrays()
        
        Hamming originalArray solutionArray