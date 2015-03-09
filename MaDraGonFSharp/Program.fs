﻿open System.Windows.Forms
open FSharp.Charting
open FSharp.Charting.ChartTypes
open MathNet.Numerics
open MathNet.Numerics.LinearAlgebra
open FSharp.Collections.ParallelSeq
open System.Drawing
open ToolBox
open MoveMent
open EvolutionaryAlgoritms

type System.Random with
    /// Generates an infinite sequence of random numbers within the given range.
    member this.GetValues(minValue, maxValue) =
        Seq.initInfinite (fun _ -> this.Next(minValue, maxValue))

let rnd = System.Random()

let ScrambleMap (S : Matrix<double>) N k =
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

[<EntryPoint>]
let main argv = 
    let N = 10
    let k = 30
    let cooling = 0.0001
    let maxIterations = 20000
    let mutable charts = []
    let form = new Form(Visible = true, TopMost = true, Width = 700, Height = 500)

    let M : Matrix<double> = DenseMatrix.init N N (fun i j -> double ((i+j) % 2))

    let S = ScrambleMap M N k
    let mutable temperature = 100.0
    for j in 0..5 do
        match j with
            |0 -> temperature <- 0.1
            |1 -> temperature <- 0.2
            |2 -> temperature <- 0.4
            |3 -> temperature <- 0.5
            |4 -> temperature <- 0.6
            |5 -> temperature <- 0.7
            |6 -> temperature <- 0.8
            |7 -> temperature <- 0.9
            |5 -> temperature <- 1.0
        //Try to solve the map with j temperature
        let simulatedAnnealingResult = (EvolutionaryAlgoritms.SimulatedAnnealing.runWithArguments M S temperature cooling maxIterations)
        let simulatedAnnealingChart = 
            printfn "%A" simulatedAnnealingResult
            let line = [ for x in 0..simulatedAnnealingResult.Length-1 -> (x,simulatedAnnealingResult.Item(x))]
            Chart.Line(line,Name=temperature.ToString()) 
            |> Chart.WithLegend(Title="Temperature")
            |> Chart.WithYAxis(Title="Fitness as a function of iterations - Simulated Annealing")
        charts <- List.append charts [simulatedAnnealingChart]
    
    let SimulatedAnnealingChartControl = new ChartControl(Chart.Combine charts, Dock=DockStyle.Fill)
    form.Controls.Add(SimulatedAnnealingChartControl)
    
    do Application.Run(form) |> ignore
    0  