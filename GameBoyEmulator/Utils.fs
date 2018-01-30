namespace GameBoyEmulator

type ResultBuilder () =
    member this.Bind (expr, f) = Result.bind f expr
    member this.Return expr = Ok expr

type MaybeBuilder () =
    member this.Bind (expr, f) = Option.bind f expr
    member this.Return expr = Some expr

module Utils =

    let result = ResultBuilder ()

    let maybe = MaybeBuilder ()

    let inline getBit value bit =
        (value &&& (LanguagePrimitives.GenericOne <<< bit)) <> LanguagePrimitives.GenericZero

    let inline setBit value bit = function
        | true -> value ||| (LanguagePrimitives.GenericOne <<< bit)
        | false -> value &&& ~~~(LanguagePrimitives.GenericOne <<< bit)
