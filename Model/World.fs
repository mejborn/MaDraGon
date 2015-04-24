namespace Model
open Model.MoveMent
open MathNet.Numerics.LinearAlgebra
open Types
open ToolBox.FitTest

module public World =
    let DoMutation (world : World) goal mutation =
        //Do the thing with each island in the world. As a sequence, so we can go parallel ;)
        match mutation with
        |Mutation.LocalSearch -> 0//EvolutionaryAlgorithms.RunSimulation.localSearch island
        |Mutation.MuPlusLambda -> 0//EvolutionaryAlgorithms.RunSimulation.localSearch island
        |Mutation.SimulatedAnnealing -> 0//EvolutionaryAlgorithms.RunSimulation.localSearch island
        |Mutation.VariableNeighbourhoodSearch -> 0//EvolutionaryAlgorithms.RunSimulation.localSearch island

    //Creates a new World to run simulations on, n = number of islands to be created.
    let CreateWorld board (board' : Board) simulation mutation n =
        let ancestor : Individual = ((doFitTest board board') ,board,[])
        //Create the simulation world
        let islands : World =
            //As the world is created there is only one an ancestor, Which 
            let (population : Population) = [ancestor] , []
            //Determine which kind of simulation is run
            match simulation with
            //Single simulation needed. Create a world with only 1 island
            |Simulation.Single -> [population,mutation]
            //Multiple simulation with same mutation type. Create Several islands, with the same mutation type
            |Simulation.Multiple -> [for i in 0..n -> population,mutation]
            //Multiple simulations with different mutation types. Create several islands, with different mutation types
            //TODO: Need to rethink how to manage Sinlge population algorithms with mergin
            //TODO: The initial population should include a real fitness!!!!
            |Simulation.All -> [for i in 0..n -> 
                                                population,
                                                match (i%4) with
                                                |0 -> Mutation.LocalSearch
                                                |1 -> Mutation.MuPlusLambda
                                                |2 -> Mutation.SimulatedAnnealing
                                                |_ -> Mutation.VariableNeighbourhoodSearch]
        (islands,board')