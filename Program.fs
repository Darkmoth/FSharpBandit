open System.Data
open System.Linq
open System

// Define a function to construct a message to print
let from whom = sprintf "from %s" whom

let optionAdd a b =
    match a, b with
    | Some x, Some y -> Some(x + y)
    | Some x, _ -> Some(x)
    | _, Some y -> Some(y)
    | _ -> None

let optionDiv a b =
    match a with
    | Some x -> Some(x / float b)
    | _ -> None

type NewObservation =
    { test_class: string 
      test_level: int
      test_value: float option }

type Observation =
    { test_level: int
      test_value: float option }

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

let real_data: Observation list =
    [ { test_level = 1
        test_value = Some 1.0 }
      { test_level = 2
        test_value = Some 0.0 }
      { test_level = 4
        test_value = Some 1.0 }
      { test_level = 4
        test_value = Some 0.0 }
      { test_level = 5
        test_value = Some 0.0 } ]

let InitBuilder () =
    let rand = Random()
    
    let randomTestValue () = 
        Some(rand.NextDouble())
    
    let randomTestClass () =
        match rand.Next(1, 5) with
        | 1 -> "Blue"
        | 2 -> "Green"
        | 3 -> "Purple"
        | _ -> "Gold"
    
    let randomNewObservation dummy =
        {
            test_class = randomTestClass()
            test_level = rand.Next(1, 51)
            test_value = randomTestValue()
        }
    
    let observations: seq<NewObservation> = Seq.init 10 randomNewObservation

    observations

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

let ObsCompact (observations: Observation list) =
    observations
    |> List.groupBy (fun o -> o.test_level)
    |> List.map (fun (testLevel, obsList) ->
        let values =
            obsList
            |> List.choose (fun o -> o.test_value)
            |> List.map float

        let average = values |> List.averageBy id |> Some

        { test_level = testLevel
          test_value = average })


[<EntryPoint>]
let main argv =
    let data_list = List.init 25 (fun i -> { test_level = i; test_value = None })

    let clean_data = ObsCompact real_data

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

    let NextExperiment = PickNode data_tree

    let init_seq = InitBuilder() |> Seq.toList

    printfn "next: %A" init_seq
    // printfn "data: %A" data_tree
    // printfn "next: %A" NextExperiment

    0 // return an integer exit code
