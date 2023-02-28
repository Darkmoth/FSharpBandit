module Resolver

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

type Resolver =
    | Tree
    | ClassForest of ClassTree list
and
    ClassTree = {
        class_type: string
        data: Tree
    }