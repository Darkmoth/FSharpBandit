module Resolver

open OptionMath
open Observations

type TreeNode = { N: float; Reward: float option }

type Tree =
    | Node of data: GenericObservation
    | Branch of data: TreeNode * left: Tree * right: Tree
    member this.N =
        match this with
        | Node x -> 1.0
        | Branch (d, l, r) -> d.N

    member this.Reward() =
        match this with
        | Node x -> x.test_value
        | Branch (d, l, r) -> d.Reward

    member this.AvgReward =
        match this with
        | Node x -> x.test_value
        | Branch (d, l, r) ->
            let reward_sum = optionAdd (l.Reward()) (r.Reward())
            let cnt = l.N + r.N
            optionDiv reward_sum (Some cnt)

type Resolver =
    | Tree of Tree
    | ClassForest of ClassTree list

and ClassTree = { class_type: string; data: Tree }

let rec TreeBuilder (obs_list: GenericObservation list) =
    let splitObservations observations =
        let sorted = List.sortBy (fun (o: GenericObservation) -> o.test_level) observations
        let count = List.length sorted
        let midpoint = count / 2
        let firstHalf = List.take midpoint sorted
        let secondHalf = List.skip midpoint sorted
        firstHalf, secondHalf

    let retval =
        if List.length obs_list = 1 then
            let obs = Node(List.head obs_list)
            obs
        else
            let left, right = splitObservations obs_list

            let left_tree: Tree = TreeBuilder left
            let right_tree: Tree = TreeBuilder right

            let tree_node =
                { N = left_tree.N + right_tree.N
                  Reward = optionAdd (left_tree.Reward()) (right_tree.Reward()) }

            Branch(tree_node, TreeBuilder left, TreeBuilder right)

    retval


let BuildResolver (obs_list: ObsList) =
    let working_list = obs_list.toGeneric |> Seq.toList

    match obs_list with
    | ObservationList x -> Tree(TreeBuilder working_list)
    | ClassObservationList x ->
        // group the list by class, then build a tree for each class
        let grouped_obs_list =
            working_list
            |> List.groupBy (fun obs -> (obs.test_class))
            |> List.map (fun ((testClass), obsGroup) ->
                let class_tree = TreeBuilder obsGroup
                let mandatory_class = testClass.Value

                { class_type = mandatory_class
                  data = class_tree })

        ClassForest(grouped_obs_list)
