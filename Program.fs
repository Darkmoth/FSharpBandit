open System.Data
open System.Linq
open System
open OptionMath
open Observations
open Resolver


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
    let data_sequence = InitBuilder() |> ObsCompact |> AddTheory

    let data_resolver = BuildResolver data_sequence

    // let data_tree = TreeBuilder data_list

    //let NextExperiment = PickNode data_tree

    printfn "next: %A" data_resolver

    0 // return an integer exit code
