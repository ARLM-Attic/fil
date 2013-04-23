module Fil

type Microsoft.FSharp.Reflection.FSharpType with
    static member MakeRecordType(name,fields) = FSharpType.MakeRecord(name,fields)
    static member MakeUnionType(name,cases) = 
        let cases =
            [|for name, types in cases ->
                match types with
                | [|single|] -> name, [|"Item",single|]
                | _ -> name, types |> Array.mapi (fun i t -> sprintf "Item%d" (i+1), t)
            |]
        FSharpType.MakeUnion(name, cases)

open FSharpFun
open System.Reflection.Emit
open Microsoft.FSharp.Quotations

type internal Marker = interface end
let CompileUntyped (quotation:Expr, returnType) =
    let f = DynamicMethod("f", returnType, [||], typeof<Marker>.Module)
    let il = f.GetILGenerator()
    quotation |> generate [] il
    il.Emit(OpCodes.Ret)
    fun () -> f.Invoke(null,[||])

let Compile (quotation:Expr<'TReturnValue>) =
    let returnType = typeof<'TReturnValue>
    let f = CompileUntyped(quotation, returnType)
    fun () -> f () :?> 'TReturnValue