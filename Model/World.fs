namespace Model
open MathNet.Numerics.LinearAlgebra
open MoveMent.MoveMent
open Madragon.RunSimulation


module public World =
    type Mutation =
    | SimulatedAnnealing
    | MuPlusLambda
    | LocalSearch
    | VariableNeighbourhoodSearch

    type Simulation =
    |Single
    |Multiple
    |All

    type Fitness = int
    type Solution = Array2D
    type Path = List<move>
    type Individual = Fitness * Solution * Path

    type Population = List<Individual>

    type Island = List<Population> * Mutation
    type World = List<Island>

    let DoMutation (island : Island) mutation =
        match mutation with
        |Mutation.LocalSearch -> RunSimulation.localSearch island
        |Mutation.MuPlusLambda -> RunSimulation.localSearch
        |Mutation.SimulatedAnnealing -> RunSimulation.localSearch
        |Mutation.VariableNeighbourhoodSearch -> RunSimulation.localSearch

    //Creates a new World to run simulations on, n = number of islands to be created.
    let CreateWorld ancestor simulation mutation n =
        //Create the simulation world
        let islands : World =
            //As the world is created there is only one an ancestor
            let population = [ancestor]
            //Determine which kind of simulation is run
            match simulation with
            //Single simulation needed. Create a world with only 1 island
            |Simulation.Single -> [[population],mutation]
            //Multiple simulation with same mutation type. Create Several islands, with the same mutation type
            |Simulation.Multiple -> [for i in 0..n -> [population],mutation]
            //Multiple simulations with different mutation types. Create several islands, with different mutation types
            //TODO: Need to rethink how to manage Sinlge population algorithms with mergin
            |Simulation.All -> [for i in 0..n -> 
                                                [population],
                                                match (i%4) with
                                                |0 -> Mutation.LocalSearch
                                                |1 -> Mutation.MuPlusLambda
                                                |2 -> Mutation.SimulatedAnnealing
                                                |_ -> Mutation.VariableNeighbourhoodSearch]
        islands