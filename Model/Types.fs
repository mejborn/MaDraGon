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
    type Board = Matrix<double>
    type RowCol = Vector<double>

    type Mutation =
    | SimulatedAnnealing
    | MuPlusLambda
    | LocalSearch
    | VariableNeighbourhoodSearch

    type RunConfiguration = Temperature * Cooling * Lambda * Lambda' * Mu * MaxIterations * FitTest * Mutation

    type Simulation =
    |Single
    |Multiple
    |All

    type Fitness = float
    type Path = List<move>
    type Individual = Fitness * Board * Path

    type Population = List<Individual> * List<Fitness>

    type Island = Population * RunConfiguration
    type World = List<Island>