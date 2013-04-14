module Fil

open System.Reflection.Emit
open Microsoft.FSharp.Reflection
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
let (|Bool|_|) = function
    | Value(v,t) when t = typeof<bool> -> Some(v :?> bool)
    | _ -> None

let rec generate env (il:ILGenerator) = function
    | Int v -> generateInt il v
    | Int64 v  -> il.Emit(OpCodes.Ldc_I8, v)
    | Float v -> il.Emit(OpCodes.Ldc_R8, v)
    | Float32 v -> il.Emit(OpCodes.Ldc_R4, v)
    | Byte v -> generateInt il (int v)
    | Char v -> generateInt il (int v)
    | Bool true -> generateInt il 1
    | Bool false -> generateInt il 0
    | String v -> il.Emit(OpCodes.Ldstr, v)
    | Value(unit,_) -> ()
    | NewArray(t,args) -> generateArray env il t args
    | NewTuple(args) -> generateTuple env il args
    | SpecificCall <@@ (+) @@> (None, _, [Int l;Int r]) -> generateInt il (l+r)
    | SpecificCall <@@ (+) @@> (None, _, args) -> generateOps env il args [OpCodes.Add]        
    | SpecificCall <@@ (-) @@> (None, _, args) -> generateOps env il args [OpCodes.Sub]
    | SpecificCall <@@ (*) @@> (None, _, args) -> generateOps env il args [OpCodes.Mul]
    | SpecificCall <@@ (/) @@> (None, _, args) -> generateOps env il args [OpCodes.Div]
    | SpecificCall <@@ (%) @@> (None, _, args) -> generateOps env il args [OpCodes.Rem]
    | SpecificCall <@@ ( ** ) @@> (None, _, args) -> generatePow env il args
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
        let _, local = env |> List.find (fst >> (=) var.Name)
        il.Emit(OpCodes.Ldloc, local)
    | VarSet(var,assignment) ->
        let _, local = env |> List.find (fst >> (=) var.Name)
        generate env il assignment
        il.Emit(OpCodes.Stloc, local)
    | Sequential(lhs,rhs) -> generate env il lhs; generate env il rhs
    | IfThenElse(condition, t, f) -> generateIfThenElse env il condition t f
    | ForIntegerRangeLoop(var,Int a,Int b,body) -> generateForLoop env il var a b body
    | arg -> raise <| System.NotSupportedException(arg.ToString())
and generateTuple env (il:ILGenerator) args =
    for arg in args do generate env il arg
    let types = [|for arg in args -> arg.Type|]
    let tuple = FSharpType.MakeTupleType(types)
    let ci = tuple.GetConstructor(types)
    il.Emit(OpCodes.Newobj, ci)
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
and generatePow env (il:ILGenerator) args =
    generateAll env il args
    let mi = typeof<System.Math>.GetMethod("Pow")
    il.EmitCall(OpCodes.Call, mi, null)
and generateIfThenElse env (il:ILGenerator) condition t f =
    generate env il condition
    let endLabel = il.DefineLabel()
    let trueBranchLabel = il.DefineLabel()
    il.Emit(OpCodes.Brtrue_S, trueBranchLabel)        
    generate env il f
    il.Emit(OpCodes.Br_S, endLabel)
    il.MarkLabel(trueBranchLabel)        
    generate env il t
    il.MarkLabel(endLabel)
and generateForLoop env (il:ILGenerator) (var:Var) a b body =
    let loopLabel = il.DefineLabel()
    let exitLabel = il.DefineLabel()
    let local = il.DeclareLocal(var.Type)
    let env = (var.Name, local)::env
    generateInt il a
    il.MarkLabel(loopLabel)
    il.Emit(OpCodes.Dup)
    il.Emit(OpCodes.Stloc, local)
    generateInt il b
    il.Emit(OpCodes.Bgt_S, exitLabel)
    generate env il body
    il.Emit(OpCodes.Ldloc, local)
    generateInt il 1
    il.Emit(OpCodes.Add)
    il.Emit(OpCodes.Br_S, loopLabel)
    il.MarkLabel(exitLabel)
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