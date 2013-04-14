module Fil

open System.Reflection.Emit
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.DerivedPatterns
open Microsoft.FSharp.Quotations.Patterns

let (|Int|_|) = function
    | Value(v,t) when t = typeof<int> -> Some(v :?> int)
    | _ -> None
let (|Int64|_|) = function
    | Value(v,t) when t = typeof<int64> -> Some(v :?> int64)
    | _ -> None
let (|Float|_|) = function
    | Value(v,t) when t = typeof<float> -> Some(v :?> float)
    | _ -> None
let (|Float32|_|) = function
    | Value(v,t) when t = typeof<float32> -> Some(v :?> float32)
    | _ -> None
let (|Byte|_|) = function
    | Value(v,t) when t = typeof<byte> -> Some(v :?> byte)
    | _ -> None
let (|Char|_|) = function
    | Value(v,t) when t = typeof<char> -> Some(v :?> char)
    | _ -> None
let (|String|_|) = function
    | Value(v,t) when t = typeof<string> -> Some(v :?> string)
    | _ -> None

let rec generate env (il:ILGenerator) = function
    | Int v -> generateInt il v
    | Int64 v  -> il.Emit(OpCodes.Ldc_I8, v)
    | Float v -> il.Emit(OpCodes.Ldc_R8, v)
    | Float32 v -> il.Emit(OpCodes.Ldc_R4, v)
    | Byte v -> generateInt il (int v)
    | Char v -> generateInt il (int v)
    | String v -> il.Emit(OpCodes.Ldstr, v)
    | Value(unit,_) -> ()
    | NewArray(t,args) -> generateArray env il t args
    | SpecificCall <@@ (+) @@> (None, _, [Int l;Int r]) -> generateInt il (l+r)
    | SpecificCall <@@ (+) @@> (None, _, args) -> generateOps env il args [OpCodes.Add]        
    | SpecificCall <@@ (-) @@> (None, _, args) -> generateOps env il args [OpCodes.Sub]
    | SpecificCall <@@ (*) @@> (None, _, args) -> generateOps env il args [OpCodes.Mul]
    | SpecificCall <@@ (/) @@> (None, _, args) -> generateOps env il args [OpCodes.Div]
    | SpecificCall <@@ (=) @@> (None, _, args) -> generateOps env il args [OpCodes.Ceq]
    | SpecificCall <@@ (<>) @@> (None, _, args) -> generateOps env il args [OpCodes.Ceq;OpCodes.Ldc_I4_0;OpCodes.Ceq]
    | SpecificCall <@@ (<) @@> (None, _, args) -> generateOps env il args [OpCodes.Clt]
    | SpecificCall <@@ (<=) @@> (None, _, args) -> generateOps env il args [OpCodes.Cgt;OpCodes.Ldc_I4_0;OpCodes.Ceq]
    | SpecificCall <@@ (>) @@> (None, _, args) -> generateOps env il args [OpCodes.Cgt]
    | SpecificCall <@@ (>=) @@> (None, _, args) -> generateOps env il args [OpCodes.Clt;OpCodes.Ldc_I4_0;OpCodes.Ceq]
    | Call(None,mi,args) -> 
        generateAll env il args
        il.EmitCall(OpCodes.Call, mi, null)
    | Let(var, assignment, cont) ->
        let local = il.DeclareLocal(var.Type)
        generate env il assignment
        il.Emit(OpCodes.Stloc, local)
        let env = (var.Name,local)::env
        generate env il cont
    | Var(var) ->
        match env |> List.tryFind (fst >> (=) var.Name) with
        | Some(_, local) -> il.Emit(OpCodes.Ldloc, local.LocalIndex)
        | None -> invalidOp ""
    | Sequential(lhs,rhs) -> 
        generate env il lhs
        generate env il rhs
    | arg -> raise <| System.NotSupportedException(arg.ToString())
and generateArray env (il:ILGenerator) t args =
    generateInt il args.Length
    il.Emit(OpCodes.Newarr,t)
    args |> Seq.iteri (fun i arg ->
        il.Emit(OpCodes.Dup)
        generateInt il i
        generate env il arg
        il.Emit(OpCodes.Stelem,t)
    )
and generateOps env (il:ILGenerator) args ops =
    generateAll env il args
    for op in ops do il.Emit(op)
and generateInt (il:ILGenerator) = function
    | 0 -> il.Emit(OpCodes.Ldc_I4_0)
    | 1 -> il.Emit(OpCodes.Ldc_I4_1)
    | 2 -> il.Emit(OpCodes.Ldc_I4_2)
    | 3 -> il.Emit(OpCodes.Ldc_I4_3)
    | 4 -> il.Emit(OpCodes.Ldc_I4_4)
    | 5 -> il.Emit(OpCodes.Ldc_I4_5)
    | 6 -> il.Emit(OpCodes.Ldc_I4_6)
    | 7 -> il.Emit(OpCodes.Ldc_I4_7)
    | 8 -> il.Emit(OpCodes.Ldc_I4_8)
    | s when s >= -127 && s <= 128 -> il.Emit(OpCodes.Ldc_I4_S, s) 
    | n -> il.Emit(OpCodes.Ldc_I4, n)
and generateAll env il args = for arg in args do generate env il arg

type internal Marker = interface end
let compileUntyped t (quotation:Expr) =
    let f = DynamicMethod("f", t, [||], typeof<Marker>.Module)
    let il = f.GetILGenerator()
    quotation |> generate [] il
    il.Emit(OpCodes.Ret)
    fun () -> f.Invoke(null,[||])

let compile (quotation:Expr<'TValue>) =
    let t = typeof<'TValue>
    let f = compileUntyped t quotation
    fun () -> f () :?> 'TValue