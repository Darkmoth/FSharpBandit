open System.Data
open System.Linq

// Define a function to construct a message to print
let from whom =
    sprintf "from %s" whom

type Observation =
| Partial of test_level : int
| Complete of test_level : int * test_value: float

let real_data = [Complete(1,1); Complete(2,0); Complete(3,1)]

[<EntryPoint>]
let main argv =
    let getTestValue observation =
        match observation with
        | Partial (test_level) -> test_level
        | Complete (test_level, _) -> test_level

    let data_list = [for i in 1 .. 25 do yield Partial(i)]

    let query1 =
        query {
            for data_val in data_list do
            leftOuterJoin real_val in real_data
                on (getTestValue data_val = getTestValue real_val) into result
            for real_val in result do
            select (data_val, real_val)
        }

    let foo = Seq.toList query1

    printfn "%A" foo

    0 // return an integer exit code
