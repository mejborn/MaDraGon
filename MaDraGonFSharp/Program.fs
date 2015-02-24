open System.Windows.Forms
open FSharp.Charting
open FSharp.Charting.ChartTypes
open MathNet.Numerics
open MathNet.Numerics.LinearAlgebra
open System.Drawing
open ToolBox
open MoveMent
open EvolutionaryAlgoritms

type System.Random with
    /// Generates an infinite sequence of random numbers within the given range.
    member this.GetValues(minValue, maxValue) =
        Seq.initInfinite (fun _ -> this.Next(minValue, maxValue))

let N = 5
let k = 10
let rnd = System.Random()

let ScrambleMap (S : Matrix<double>) =
    let directions = 
        rnd.GetValues(0,3)
        |> Seq.take k
        |> Seq.map (fun n ->
            match n with
            | 0 -> MoveMent.direction.Up
            | 1 -> MoveMent.direction.Down
            | 2 -> MoveMent.direction.Right
            | _ -> MoveMent.direction.Left)

    let rowcols = rnd.GetValues(0,N-1) |> Seq.take k

    let moves = Seq.map2 (fun direction rowcol -> MoveMent.Move(direction,rowcol)) directions rowcols
    
    Seq.fold (fun M move -> (MoveMent.MakeMove M move)) S moves

//Main program loop
let rec loop (M : Matrix<double>) (S : Matrix<double>) =
    printfn "%A" M
    printfn "%A" S

    printfn "Calculating using SA. Please Wait."
    
    let SimulatedAnnealingResult = EvolutionaryAlgoritms.SimulatedAnnealing.run M S
  
    let SimulatedAnnealingChart = 
        [ for x in 0..SimulatedAnnealingResult.Length-1 -> (x,SimulatedAnnealingResult.Item(x))]
        |> Chart.Line |> Chart.WithYAxis(Title="Fitness as a function of iterations - Simulated Annealing")
    let SimulatedAnnealingChartControl = new ChartControl(SimulatedAnnealingChart, Dock=DockStyle.Fill)

    let form = new Form(Visible = true, TopMost = true, Width = 700, Height = 500)
    form.Controls.Add(SimulatedAnnealingChartControl)
    do Application.Run(form) |> ignore

let InitializeMap =
    let M : Matrix<double> = DenseMatrix.init N N (fun i j -> double ((i+j) % 2))
    let S = ScrambleMap M
    loop M S

[<EntryPoint>]
let main argv = 
    InitializeMap
    0 // return an doubleeger exit code   