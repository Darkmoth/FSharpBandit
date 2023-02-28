open System.Data
open System.Linq
open System
open OptionMath
open Observations

type TreeNode = { N: float; Reward: float option }

type Tree =
    | Node of data: Observation
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

let rec TreeBuilder data_seq =
    let splitObservations observations =
        let sorted = List.sortBy (fun o -> o.test_level) observations
        let count = List.length sorted
        let midpoint = count / 2
        let firstHalf = List.take midpoint sorted
        let secondHalf = List.skip midpoint sorted
        firstHalf, secondHalf

    let retval =
        if Seq.length data_seq = 1 then
            let obs = Node(Seq.head data_seq)
            obs
        else
            let left, right = splitObservations data_seq

            let left_tree: Tree = TreeBuilder left
            let right_tree: Tree = TreeBuilder right

            let left_node =
                { N = left_tree.N + right_tree.N
                  Reward = optionAdd (left_tree.Reward()) (right_tree.Reward()) }

            Branch(left_node, TreeBuilder left, TreeBuilder right)

    retval

let rec PickNode data_tree =
    let logFunc (t: Tree) total_N =
        let log_stuff = 2.0 * log t.N
        let sqrt_stuff = sqrt log_stuff / total_N
        Some(sqrt_stuff)

    let NodeEval (l: Tree) (r: Tree) =
        if l.Reward() = None then
            PickNode l
        else if r.Reward() = None then
            PickNode r
        else
            let total_N = float (l.N + r.N)
            let l_score = optionAdd (logFunc l total_N) l.AvgReward
            let r_score = optionAdd (logFunc r total_N) r.AvgReward

            if l_score > r_score then
                PickNode l
            else
                PickNode r

    // evaluate missing data, then UCB
    match data_tree with
    | Node (x) -> x
    | Branch (d, l, r) -> NodeEval l r

[<EntryPoint>]
let main argv =
    (*
    let data_tree = TreeBuilder data_sequence

    let NextExperiment = PickNode data_tree
*)
    let init_seq = InitBuilder() |> ObsCompact |> AddTheory
    //|> Seq.choose

    printfn "next: %A" init_seq

    0 // return an integer exit code
