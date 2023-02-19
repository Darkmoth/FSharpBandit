﻿open System.Data
open System.Linq

// Define a function to construct a message to print
let from whom = sprintf "from %s" whom

let floatAdd a b =
    match a, b with
    | Some x, Some y -> Some(x + y)
    | Some x, _ -> Some(x)
    | _, Some y -> Some(y)
    | _ -> None

type Observation =
    { test_level: int
      test_value: float option }

type TreeNode = { N: int; Reward: float option }

type Tree =
    | Node of data: Observation
    | Branch of data: TreeNode * left: Tree * right: Tree
    member this.N() =
        match this with
        | Node x -> 1
        | Branch (d, l, r) -> l.N() + r.N()

    member this.Reward() =
        match this with
        | Node x -> x.test_value
        | Branch (d, l, r) -> floatAdd (l.Reward()) (r.Reward())

let real_data: Observation list =
    [ { test_level = 1
        test_value = Some 1.0 }
      { test_level = 2
        test_value = Some 0.0 }
      { test_level = 4
        test_value = Some 1.0 }
      { test_level = 5
        test_value = Some 0.0 } ]

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
                { N = left_tree.N() + right_tree.N()
                  Reward = floatAdd (left_tree.Reward()) (right_tree.Reward()) }

            Branch(left_node, TreeBuilder left, TreeBuilder right)

    retval

[<EntryPoint>]
let main argv =
    let data_list = List.init 25 (fun i -> { test_level = i; test_value = None })

    let query1 =
        query {
            for data_val in data_list do
                leftOuterJoin real_val in real_data on (data_val.test_level = real_val.test_level) into result

                for real_val in result do
                    where (box real_val = null)
                    select data_val
        }

    let query2 =
        query {
            for data_val in data_list do
                leftOuterJoin real_val in real_data on (data_val.test_level = real_val.test_level) into result

                for real_val in result do
                    where (box real_val <> null)
                    select real_val
        }

    let data_sequence = query2.Union(query1) |> Seq.toList

    let data_tree = TreeBuilder data_sequence

    printfn "%A" data_tree

    0 // return an integer exit code
