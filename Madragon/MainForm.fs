module MainForm

open System
open System.Windows.Forms
open System.Drawing
open Model.Types
open Model.MoveMent
open Model.World
open MathNet.Numerics.LinearAlgebra
open MathNet.Numerics

type MainForm() as form =
    inherit Form()
    // Variables setup
    let mutable numRunsForMean = 1
    let mutable maxMerges = 2
    let mutable N = 5
    let mutable k = 2
    let mutable board : Board = DenseMatrix.init N N (fun i j ->  (double) ((i+j) % 2))
    let mutable (board',moves) = ScrambleMap board N k
    let mutable maxIterations = 100000
// Configuraition of simulation
    let mutable simulation = Simulation.Single
    let mutable algorithm = Algorithm.LocalSearch
    let mutable fitTest = FitTest.Hamming2
// Simulated Annealing
    let mutable temperature = 100.0
    let mutable cooling = 0.01
    let mutable lambda = 1.0
    let mutable saConfig = temperature , cooling , lambda
// Mu + Lambdas
    let mutable mu = 5
    let mutable lambda' = 5
    let mutable mplConfig = mu , lambda'
    let mutable configuration : Configuration = (maxIterations,fitTest,algorithm,saConfig,mplConfig)
    // Controls
    let MainMenu = new MainMenu()
    let origDisp = new ListBox()
    let solDisp = new ListBox()
    let confDisp = new MenuItem()
    let consDisp = new MenuItem()
    let buttonDisp = new MenuItem()

    do form.InitializeForm

    member this.InitializeForm =
        this.FormBorderStyle <- FormBorderStyle.Fixed3D
        this.Text <- "MaDraGon Solver"
        this.Width <- 500
        this.Height <- 500

