open System.Data
open System.Linq
open System

// Helper function to construct a message
let from whom = sprintf "from %s" whom

// Safe addition for option types
let optionAdd a b =
    match a, b with
    | Some x, Some y -> Some(x + y)
    | Some x, _ -> Some(x)
    | _, Some y -> Some(y)
    | _ -> None

// Safe division for option types
let optionDiv a b =
    match a with
    | Some x -> Some(x / float b)
    | _ -> None

// Type definitions for experimental data
type NewObservation =
    { test_class: string
      test_level: int
      test_N: int
      test_value: float option }

type Observation =
    { test_level: int
      test_value: float option }

// Tree structure for Monte Carlo Tree Search
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
            optionDiv reward_sum cnt

// Available test classes
let TestClasses = seq [ "Red"; "Green"; "Blue" ]

// Sample real data
let real_data: Observation list =
    [ { test_level = 1; test_value = Some 1.0 }
      { test_level = 2; test_value = Some 0.0 }
      { test_level = 4; test_value = Some 1.0 }
      { test_level = 4; test_value = Some 0.0 }
      { test_level = 5; test_value = Some 0.0 } ]

// Function to generate random initial data
let InitBuilder () =
    let rand = Random()
    let randomTestValue () = Some(rand.NextDouble())
    let randomTestClass () = Seq.item (rand.Next(Seq.length TestClasses)) TestClasses
    let randomNewObservation dummy =
        { test_class = randomTestClass ()
          test_level = rand.Next(1, 11)
          test_N = 1
          test_value = randomTestValue () }
    Seq.init 1000 randomNewObservation

// Recursively build the search tree
let rec TreeBuilder data_seq =
    let splitObservations observations =
        let sorted = List.sortBy (fun o -> o.test_level) observations
        let count = List.length sorted
        let midpoint = count / 2
        List.take midpoint sorted, List.skip midpoint sorted

    if Seq.length data_seq = 1 then
        Node(Seq.head data_seq)
    else
        let left, right = splitObservations data_seq
        let left_tree: Tree = TreeBuilder left
        let right_tree: Tree = TreeBuilder right
        let left_node =
            { N = left_tree.N + right_tree.N
              Reward = optionAdd (left_tree.Reward()) (right_tree.Reward()) }
        Branch(left_node, TreeBuilder left, TreeBuilder right)

// Implement Upper Confidence Bound (UCB) algorithm for tree traversal
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
            if l_score > r_score then PickNode l else PickNode r

    match data_tree with
    | Node (x) -> x
    | Branch (d, l, r) -> NodeEval l r

// Compact observations by grouping and averaging
let ObsCompact observations =
    observations
    |> Seq.groupBy (fun obs -> (obs.test_class, obs.test_level))
    |> Seq.map (fun ((testClass, testLevel), obsGroup) ->
        let count = obsGroup |> Seq.length
        let avgValue =
            obsGroup
            |> Seq.choose (fun obs -> obs.test_value)
            |> Seq.average
        { test_class = testClass
          test_level = testLevel
          test_N = count
          test_value = Some avgValue })

// Add theoretical test levels to the dataset
let AddTheory (observations: seq<NewObservation>) =
    let AddTestLevels class_value =
        Seq.init 25 (fun i ->
            { test_class = class_value
              test_level = i
              test_N = 1
              test_value = None })

    let data_list =
        Seq.map AddTestLevels TestClasses
        |> Seq.collect (fun seq -> seq)

    query {
        for data_val in data_list do
            leftOuterJoin real_val in observations on (data_val.test_level = real_val.test_level) into result
            for real_val in result do
                select (
                    if (box real_val) = null then data_val
                    else real_val
                )
    }

[<EntryPoint>]
let main argv =
    // Generate initial data, compact it, and add theoretical points
    let init_seq =
        InitBuilder()
        |> ObsCompact
        |> AddTheory
        |> Seq.toList

    printfn "next: %A" init_seq

    0 // return an integer exit code