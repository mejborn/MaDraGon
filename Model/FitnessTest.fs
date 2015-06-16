namespace Model
open MathNet.Numerics
open Types
open MathNet.Numerics.LinearAlgebra
open FSharp.Collections.ParallelSeq

module public FitnessTest =
    let Hamming (original : double[][]) solution =
        Distance.Hamming(Array.concat original , Array.concat solution)/2.0
    
    let rec Density (original : double[][]) (solution : double[][]) =
        let N = int original.Length
        
        let fitness =
            match N with
            |0 -> 0.0
            |1 -> Hamming original solution
            |2 -> Hamming original solution
            |_ -> 
                (Density original.[..(N/2)-1].[..(N/2)-1] solution.[..(N/2)-1].[..(N/2)-1]) +
                (Density original.[(N/2)..].[(N/2)..] solution.[(N/2)..].[(N/2)..]) +
                (Density original.[..(N/2)-1].[(N/2)..] solution.[..(N/2)-1].[(N/2)..]) +
                (Density original.[(N/2)..].[..(N/2)-1] solution.[(N/2)..].[..(N/2)-1])

        let sumOriginal = Seq.concat original |> Seq.sumBy (fun value -> if (value =  1.0) then 1.0 else 0.0) 
        let sumSolution = Seq.concat solution |> Seq.sumBy (fun value -> if (value =  1.0) then 1.0 else 0.0)

        (abs (sumOriginal - sumSolution)) + fitness
    
    let Manhattan (original : double[][]) solution =
        Seq.zip original solution
        |> Seq.sumBy (fun (row1, row2) -> Distance.Manhattan(row1,row2))

    let Custom (original : double[][]) (solution : double[][]) =
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
            
    let Hamming2 (original : Matrix<double>) (solution : Board) =
        let test1 = 
            [for i in 0..(original.ColumnCount-1) do
                for j in 0..(original.RowCount-1) do
                    yield original.At(i,j)]
        let test2 = 
            [for i in 0..(original.ColumnCount-1) do
                for j in 0..(original.RowCount-1) do
                    yield original.At(i,j)]
        (test1,test2) ||> Seq.map2 (fun x y -> if(x=y) then 0.0 else 1.0) |> Seq.sum
        

    let run (original : Board) (solution : Board) (configuration : Configuration) =
        let (_,fitTest,_,_,_) = configuration
        let originalArray = original.ToRowArrays()
        let solutionArray = solution.ToRowArrays()
        
        match fitTest with
        |Hamming -> Hamming originalArray solutionArray
        |Manhattan -> Manhattan originalArray solutionArray
        |Density -> Density originalArray solutionArray
        |Custom -> Custom originalArray solutionArray
        |Hamming2 -> Hamming2 original solution