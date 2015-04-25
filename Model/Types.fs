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
    type Temperature = double
    type Cooling = double
    type Mu = int
    type Lambda = float
    type MaxIterations = int
    type RunConfiguration = Temperature * Cooling * Mu * Lambda * MaxIterations

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

    type Simulation =
    |Single
    |Multiple
    |All

    type Fitness = float
    type Path = List<move>
    type Individual = Fitness * Board * Path

    type Population = List<Individual> * List<Fitness>

    type Island = Population * Mutation
    type World = List<Island>