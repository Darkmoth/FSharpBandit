module Observations

open System

type Observation =
    { test_level: int
      test_N: int
      test_value: float option }

type ClassObservation =
    { test_class: string
      data: Observation }

type GenericObservation =
    | Observation of Observation
    | ClassObservation of ClassObservation
    member this.test_class =
        match this with
        | Observation x -> None
        | ClassObservation x -> Some x.test_class

    member this.test_level =
        match this with
        | Observation x -> x.test_level
        | ClassObservation x -> x.data.test_level

    member this.test_value =
        match this with
        | Observation x -> x.test_value
        | ClassObservation x -> x.data.test_value

type ObsList =
    | ObservationList of seq<Observation>
    | ClassObservationList of seq<ClassObservation>
    member this.toGeneric =
        let genericObsSeq =
            match this with
            | ObservationList obsSeq -> obsSeq |> Seq.map (fun obs -> Observation obs)
            | ClassObservationList obsSeq ->
                obsSeq
                |> Seq.map (fun obs -> ClassObservation obs)

        genericObsSeq
    member this.fromGeneric data_list =
        match this with
        | ObservationList _ ->
            ObservationList(
                data_list
                |> Seq.map (fun obs ->
                    match obs with
                    | Observation x -> x
                    | _ -> failwith "Invalid type")
            )
        | ClassObservationList _ ->
            ClassObservationList(
                data_list
                |> Seq.map (fun obs ->
                    match obs with
                    | ClassObservation x -> x
                    | _ -> failwith "Invalid type")
            )

let TestClasses = seq [ "Red"; "Green"; "Blue" ]

let rand = Random()

let randomTestValue () = Some(rand.NextDouble())

let randomObservation dummy : Observation =
    { test_level = rand.Next(1, 11)
      test_N = 1
      test_value = randomTestValue () }

let randomTestClass () =
    Seq.item (rand.Next(Seq.length TestClasses)) TestClasses

let randomClassObservation dummy : ClassObservation =
    { test_class = randomTestClass ()
      data = randomObservation 0 }

let InitBuilder () : ObsList =
    // pick the type of list
    let list_type = rand.Next(0, 2)

    printfn "list_type: %A" list_type

    let observations: ObsList =
        if list_type = 0 then
            ObservationList(Seq.init 1000 randomObservation)
        else
            ClassObservationList(Seq.init 1000 randomClassObservation)

    observations

let ObsCompact (obsList: ObsList) =
    let genericObsSeq = obsList.toGeneric

    let processedObsSeq =
        genericObsSeq
        |> Seq.groupBy (fun obs -> (obs.test_class, obs.test_level))
        |> Seq.map (fun ((testClass, testLevel), obsGroup) ->
            let count = Seq.length obsGroup

            let avgValue =
                obsGroup
                |> Seq.choose (fun obs -> obs.test_value)
                |> Seq.average

            match obsGroup |> Seq.head with
            | Observation x ->
                Observation
                    { x with
                        test_N = count
                        test_value = Some avgValue }
            | ClassObservation x ->
                ClassObservation
                    { x with
                        data =
                            { x.data with
                                test_N = count
                                test_value = Some avgValue } })

    obsList.fromGeneric processedObsSeq

let AddTheory (obsList: ObsList) =
    let data_list = obsList.toGeneric

    let GenDataBlock i =
        { test_level = i
          test_N = 1
          test_value = None }

    let real_list =
        match obsList with
        | ObservationList _ ->
            seq {
                for inner in [ 1..25 ] do
                    yield GenDataBlock inner
            }
            |> Seq.map Observation
        | ClassObservationList _ ->
            seq {
                for class_value in TestClasses do
                    for inner in [ 1..25 ] do
                        yield
                            { test_class = class_value
                              data = GenDataBlock inner }
            }
            |> Seq.map ClassObservation

    let data_sequence =
        query {
            for data_val in data_list do
                leftOuterJoin real_val in real_list
                                              on
                                              ((data_val.test_class, data_val.test_level) = (real_val.test_class,
                                                                                             real_val.test_level))
                                              into
                                              result

                for real_val in result do
                    select (
                        if (box real_val) = null then
                            data_val
                        else
                            real_val
                    )
        }

    obsList.fromGeneric data_sequence
