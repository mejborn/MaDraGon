namespace Model
open MathNet.Numerics.LinearAlgebra

module public Types =
    // ##################
    // # Board Movement #
    // ##################
    type direction =
    | Up
    | Down
    | Left
    | Right
    
    type move = Move of direction * int

    // #####################
    // # Run configuration #
    // #####################
    type FitTest =
    |Hamming
    |Manhattan
    |Density
    |Custom

    type Temperature = double
    type Cooling = double
    type Mu = int
    type Lambda = float
    type Lambda' = int
    type MaxIterations = int
    
    // ###############
    // # World setup #
    // ###############
    type RowCol = List<double>
    type Board = List<RowCol>

    type SAConfig = Temperature * Cooling * Lambda
    type MPLConfig = Mu * Lambda'

    type Algorithm =
    | SimulatedAnnealing
    | MuPlusLambda
    | MuCommaLambda
    | LocalSearch
    | VariableNeighbourhoodSearch

    type Configuration = MaxIterations * FitTest * Algorithm * SAConfig * MPLConfig

    type Simulation =
    |Single
    |Multiple
    |All

    type Fitness = float
    type Path = List<move>
    type Individual = Fitness * Board * Path

    type Population = List<Individual> * List<Fitness>

    type Island = Population * Configuration
    type World = List<Island>