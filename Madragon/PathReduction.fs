module PathReduction
    open Model
    open Model.Types
    open Model.MoveMent
    open Algorithms

    // ##############################################
    // #           REDUCION OF PATH LENGTH          #
    // #               Returns a path               #
    // ##############################################

    let rec run original (path : Path) simulation algorithm configuration numIslands maxMerges =
        
        // Find two random points in the solution paths and use them as new starting points
        let val1 , val2 = System.Random().Next(0,path.Length-1) , System.Random().Next(0,path.Length-1)
        let length = (System.Math.Abs (val1 - val2))

        // Walks val1 and val2 steps from the original
        let solboard , solboard' =
            if (val1 < val2) then
                Seq.fold (fun board move -> MakeMove board move) original (Seq.take val1 path)
                ,
                Seq.fold (fun board move -> MakeMove board move) original (Seq.take val2 path)
            else
                Seq.fold (fun board move -> MakeMove board move) original (Seq.take val2 path)
                ,
                Seq.fold (fun board move -> MakeMove board move) original (Seq.take val1 path)

        let world : World = World.CreateWorld solboard solboard' simulation algorithm configuration numIslands
        // Create a new world
        let world' : World = DoMutation.run world original 0 maxMerges
        //Sort the world by path length
        let sortedIslands =
            List.ofSeq(
                world |> Seq.sortBy
                    (fun (island : Island)->
                    // Must return the length of the best possible island's
                    let population , _ = island
                    let (individuals : List<Individual>) , _ = population
                    let _ , _ , (path' : Path) = individuals.[0]
                    path'.Length))
                    
        // Check if the best path possible is smaller than the current path
        let bestIsland , _ = sortedIslands.[0]
        let bestIndividuals , _ = bestIsland
        let bestIndividual = bestIndividuals.[0]
        let _ , _ , bestPath = bestIndividual
        // Replace the new path in the solution path if the new path is smaller
        if bestPath.Length < length then
            let newPath =
                let path1 = List.ofSeq (path |> Seq.take val1)
                let path2 = List.ofSeq (path |> Seq.skip val2)
                if (val1 < val2) then
                    let path' = List.append path1 bestPath
                    List.append path' path2
                else
                    let path' = List.append path2 bestPath
                    List.append path' path1
            newPath
        else
            path
