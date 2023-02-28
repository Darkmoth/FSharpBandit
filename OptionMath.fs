module OptionMath

let optionAdd (a: float option) (b: float option) =
    match a, b with
    | Some x, Some y -> Some(x + y)
    | _ -> Option.orElse a b

let optionDiv (a: float option) (b: float option) =
    if b = Some(0.0) then None
    else
        match a, b with
        | Some x, Some y -> Some(x / y)
        | _ -> Option.orElse a b