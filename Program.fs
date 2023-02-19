open System.Data
open System.Linq

// Define a function to construct a message to print
let from whom = sprintf "from %s" whom

type Observation =
    | Partial of test_level: int
    | Complete of test_level: int * test_value: float

type TreeNode =
    { N: int
      Reward: float
      Data: Observation list }

type Tree =
    | Node of TreeNode
    | Branch of Tree * Tree

let real_data =
    [ Complete(1, 1)
      Complete(2, 0)
      Complete(4, 1)
      Complete(5, 0) ]

let rec TreeBuilder data_seq =
    let splitObservations observations =
        let sorted =
            observations
            |> List.sortBy (function
                | Partial level -> level
                | Complete (level, _) -> level)

        let medianIndex = List.length sorted / 2

        let medianValue =
            sorted
            |> List.item medianIndex
            |> (function
            | Partial level -> level
            | Complete (level, _) -> level)

        let left, right = List.take medianIndex sorted, List.skip medianIndex sorted
        left, right

    let retval =
        if Seq.length data_seq <= 3 then
            let obs_count =
                data_seq
                |> Seq.map (fun obs ->
                    match obs with
                    | Partial test_level -> 0
                    | Complete (test_level, _) -> 1)
                |> Seq.sum

            let obs_reward =
                data_seq
                |> Seq.map (fun obs ->
                    match obs with
                    | Partial test_value -> 0.0
                    | Complete (_, test_value) -> test_value)
                |> Seq.sum

            Node(
                { N = obs_count
                  Reward = obs_reward
                  Data = data_seq }
            )
        else
            let left, right = splitObservations data_seq
            Branch(TreeBuilder left, TreeBuilder right)

    retval

[<EntryPoint>]
let main argv =
    let GetNonNulls =
        function
        | (_, Complete (x, y)) -> Complete(x, y)
        | (x, _) -> x

    let getTestValue =
        function
        | Partial x
        | Complete (x, _) -> x

    let data_list = List.init 25 Partial

    let query1 =
        query {
            for data_val in data_list do
                leftOuterJoin real_val in real_data on (getTestValue data_val = getTestValue real_val) into result

                for real_val in result do
                    select (data_val, real_val)
        }

    let data_sequence = Seq.map GetNonNulls query1 |> Seq.toList

    let data_tree = TreeBuilder data_sequence

    printfn "%A" data_tree

    0 // return an integer exit code
