open System.Data
open System.Linq
open System
open OptionMath
open Observations
open Resolver

[<EntryPoint>]
let main argv =
    let data_sequence = InitBuilder() |> ObsCompact |> AddTheory

    let data_resolver = BuildResolver data_sequence

    let NextExperiment = Resolve data_resolver

    printfn "next: %A" NextExperiment

    0 // return an integer exit code
