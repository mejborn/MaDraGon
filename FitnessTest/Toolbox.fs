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

    let CoSineRelation (original : double[][]) solution =
        PSeq.zip original solution
        |> PSeq.sumBy (fun (row1, row2 ) -> Distance.Cosine( row1, row2))
    
    let Manhattan (original : double[][]) solution =
        PSeq.zip original solution
        |> PSeq.sumBy (fun (row1, row2) -> Distance.Manhattan(row1,row2))

    let doFitTest (original : Matrix<double>) (solution : Matrix<double>) =
        let originalArray = original.ToColumnArrays()
        let solutionArray = solution.ToColumnArrays()
        
        Hamming originalArray solutionArray
