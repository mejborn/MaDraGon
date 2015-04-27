namespace Model
open Model.MoveMent
open MathNet.Numerics.LinearAlgebra
open Types

module public World =
    //Creates a new World to run simulations on, n = number of islands to be created.
    //World is missing runConfiguration
    let CreateWorld board (board' : Board) simulation mutation (configuration : RunConfiguration) n =
        let ancestor : Individual = ((FitnessTest.run board board' configuration) ,board,[])
        // Unpack the configuration
        let temperature , cooling , mu , lambda , lambda' , maxIterations , fitTest , mutation = configuration
        //Create the simulation world
        let islands : World =
            //As the world is created there is only one an ancestor, Which 
            let (population : Population) = [ancestor] , []
            //Determine which kind of simulation is run
            match simulation with
            //Single simulation needed. Create a world with only 1 island
            |Simulation.Single -> [population,configuration]
            //Multiple simulation with same mutation type. Create Several islands, with the original configuration
            |Simulation.Multiple -> [for i in 0..n -> population,configuration]
            //Multiple simulations with different mutation types. Create several islands, with different mutation types
            //Perhaps implement different fitness functions for each of the Mutation types
            |Simulation.All -> [for i in 0..n -> 
                                                population,
                                                let configuration' = 
                                                    (temperature , cooling, mu, lambda , lambda' , maxIterations , 
                                                        (let fitTest = fitTest
                                                        fitTest),
                                                        (let mutation' =
                                                            match (i%4) with
                                                            |0 -> Mutation.LocalSearch
                                                            |1 -> Mutation.MuPlusLambda
                                                            |2 -> Mutation.SimulatedAnnealing
                                                            |_ -> Mutation.VariableNeighbourhoodSearch
                                                        mutation'))
                                                configuration']
        islands