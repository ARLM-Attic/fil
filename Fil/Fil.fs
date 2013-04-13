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

let rec generate (il:ILGenerator) = function
    | Int v -> generateInt il v
    | Int64 v  -> il.Emit(OpCodes.Ldc_I8, v)
    | Float v -> il.Emit(OpCodes.Ldc_R8, v)
    | Float32 v -> il.Emit(OpCodes.Ldc_R4, v)
    | Byte v -> generateInt il (int v) //il.Emit(OpCodes.Conv_U1)
    | Char v -> generateInt il (int v) //il.Emit(OpCodes.Conv_U2)
    | String v -> il.Emit(OpCodes.Ldstr, v)
    | NewArray(t,args) -> generateArray il t args
    | SpecificCall <@@ (+) @@> (None, _, [Int l;Int r]) -> generateInt il (l+r)
    | SpecificCall <@@ (+) @@> (None, _, args) -> generateOps il args [OpCodes.Add]        
    | SpecificCall <@@ (-) @@> (None, _, args) -> generateOps il args [OpCodes.Sub]
    | SpecificCall <@@ (*) @@> (None, _, args) -> generateOps il args [OpCodes.Mul]
    | SpecificCall <@@ (/) @@> (None, _, args) -> generateOps il args [OpCodes.Div]
    | SpecificCall <@@ (=) @@> (None, _, args) -> generateOps il args [OpCodes.Ceq]
    | SpecificCall <@@ (<>) @@> (None, _, args) -> generateOps il args [OpCodes.Ceq;OpCodes.Ldc_I4_0;OpCodes.Ceq]
    | SpecificCall <@@ (<) @@> (None, _, args) -> generateOps il args [OpCodes.Clt]
    | SpecificCall <@@ (<=) @@> (None, _, args) -> generateOps il args [OpCodes.Cgt;OpCodes.Ldc_I4_0;OpCodes.Ceq]
    | SpecificCall <@@ (>) @@> (None, _, args) -> generateOps il args [OpCodes.Cgt]
    | SpecificCall <@@ (>=) @@> (None, _, args) -> generateOps il args [OpCodes.Clt;OpCodes.Ldc_I4_0;OpCodes.Ceq]
    | Call(None,mi,args) -> 
        generateAll il args
        il.EmitCall(OpCodes.Call, mi, null)
    | arg -> raise <| System.NotSupportedException(arg.ToString())
and generateArray (il:ILGenerator) t args =
    generateInt il args.Length
    il.Emit(OpCodes.Newarr,t)
    args |> Seq.iteri (fun i arg ->
        il.Emit(OpCodes.Dup)
        generateInt il i
        generate il arg
        generateArrayStoreElem il t
    )
and generateArrayStoreElem il t =
    match t with
    | _ when t = typeof<int> -> il.Emit(OpCodes.Stelem_I4)
    | _ -> raise <| System.NotImplementedException()
and generateOps (il:ILGenerator) args ops =
    generateAll il args
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
and generateAll il args = for arg in args do generate il arg

type internal Marker = interface end
let compileUntyped t (quotation:Expr) =
    let f = DynamicMethod("f", t, [||], typeof<Marker>.Module)
    let il = f.GetILGenerator()
    quotation |> generate il
    il.Emit(OpCodes.Ret)
    fun () -> f.Invoke(null,[||])

let compile (quotation:Expr<'TValue>) =
    let t = typeof<'TValue>
    let f = compileUntyped t quotation
    fun () -> f () :?> 'TValue