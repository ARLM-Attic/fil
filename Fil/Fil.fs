module Fil

open System.Reflection.Emit
open Microsoft.FSharp.Reflection
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.DerivedPatterns
open Microsoft.FSharp.Quotations.Patterns
  
let (|GetArray|_|) = function
    | Call(None,mi,[xs; Int32 index]) when 
        xs.Type.IsArray &&
        mi.DeclaringType.Name="IntrinsicFunctions" &&
        mi.Name = "GetArray" ->
        Some (xs,index)
    | _ -> None
let (|SetArray|_|) = function
    | Call(None,mi,[xs; Int32 index; x]) when 
        xs.Type.IsArray &&
        mi.DeclaringType.Name="IntrinsicFunctions" && 
        mi.Name = "SetArray" ->
        Some (xs,index, x)
    | _ -> None

let rec generate env (il:ILGenerator) = function
    | Value(_,t) when t = typeof<unit> -> ()
    | Int32 v -> generateInt il v
    | Int64 v  -> il.Emit(OpCodes.Ldc_I8, v)
    | Double v -> il.Emit(OpCodes.Ldc_R8, v)
    | Single v -> il.Emit(OpCodes.Ldc_R4, v)
    | Byte v -> generateInt il (int v)
    | Char v -> generateInt il (int v)
    | Bool true -> generateInt il 1
    | Bool false -> generateInt il 0
    | String v -> il.Emit(OpCodes.Ldstr, v)
    | NewObject(ci,args) -> generateAll env il args; il.Emit(OpCodes.Newobj, ci)
    | NewArray(t,args) -> generateArray env il t args
    | GetArray(xs,index) -> generateGetArray env il xs index
    | SetArray(xs,index,x) -> generateSetArray env il xs index x
    | NewTuple(args) -> generateTuple env il args
    | TupleGet(tuple,index) -> generateTupleGet env il tuple index
    | SpecificCall <@@ not @@> (None, _, args) -> generateOps env il args [OpCodes.Ldc_I4_0;OpCodes.Ceq]
    | AndAlso (lhs,rhs) -> generateOps env il [lhs;rhs] [OpCodes.And]
    | OrElse (lhs,rhs) -> generateOps env il [lhs;rhs] [OpCodes.Or]
    | SpecificCall <@@ (+) @@> (None, _, [Int32 l;Int32 r]) -> generateInt il (l+r)
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
    | Call(None,mi,args) -> generateAll env il args; il.EmitCall(OpCodes.Call, mi, null)
    | Let(var, expr, body) -> generateLet env il var expr body
    | Var(var) -> generateVar env il var
    | VarSet(var,expr) -> generateVarSet env il var expr
    | Sequential(lhs,rhs) -> generate env il lhs; generate env il rhs
    | IfThenElse(condition, t, f) -> generateIfThenElse env il condition t f
    | ForIntegerRangeLoop(var,Int32 a,Int32 b,body) -> generateForLoop env il var a b body
    | WhileLoop(condition, body) -> generateWhileLoop env il condition body
    | arg -> raise <| System.NotSupportedException(arg.ToString())
and generateVar env (il:ILGenerator) var =
    let _, (local:LocalBuilder) = env |> List.find (fst >> (=) var.Name)
    generateLdloc il local
and generateVarSet env il (var:Var) expr =
    let _, local = env |> List.find (fst >> (=) var.Name)
    generate env il expr
    generateStloc il local
and generateTuple env (il:ILGenerator) args =
    for arg in args do generate env il arg
    let types = [|for arg in args -> arg.Type|]
    let tuple = FSharpType.MakeTupleType(types)
    let ci = tuple.GetConstructor(types)
    il.Emit(OpCodes.Newobj, ci)
and generateTupleGet env il tuple index =    
    generate env il tuple
    let pi = tuple.Type.GetProperty(sprintf "Item%d" (index+1))
    let mi = pi.GetGetMethod()
    il.EmitCall(OpCodes.Call, mi, null)
and generateArray env (il:ILGenerator) t args =
    generateInt il args.Length
    il.Emit(OpCodes.Newarr,t)
    args |> Seq.iteri (fun i arg ->
        il.Emit(OpCodes.Dup)
        generateInt il i
        generate env il arg
        il.Emit(OpCodes.Stelem,t)
    )
and generateGetArray env il xs index =
    generate env il xs
    generateInt il index
    il.Emit(OpCodes.Ldelem, xs.Type.GetElementType())
and generateSetArray env il xs index x =
    generate env il xs
    generateInt il index
    generate env il x
    il.Emit(OpCodes.Stelem, xs.Type.GetElementType())
and generateOps env (il:ILGenerator) args ops =
    generateAll env il args
    for op in ops do il.Emit(op)
and generatePow env (il:ILGenerator) args =
    generateAll env il args
    let mi = typeof<System.Math>.GetMethod("Pow")
    il.EmitCall(OpCodes.Call, mi, null)
and generateLet env (il:ILGenerator) (var:Var) expr body =
    let local = il.DeclareLocal(var.Type)
    generate env il expr
    generateStloc il local
    let env = (var.Name,local)::env
    generate env il body
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
    generateLdloc il local
    generateInt il 1
    il.Emit(OpCodes.Add)
    il.Emit(OpCodes.Br_S, loopLabel)
    il.MarkLabel(exitLabel)
and generateWhileLoop env il condition body =
    let loopLabel = il.DefineLabel()
    let exitLabel = il.DefineLabel()
    il.MarkLabel(loopLabel)
    generate env il condition
    il.Emit(OpCodes.Brfalse_S, exitLabel)
    generate env il body
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
    | -1 -> il.Emit(OpCodes.Ldc_I4_M1)
    | s when s >= -127 && s <= 128 -> il.Emit(OpCodes.Ldc_I4_S, byte s) 
    | n -> il.Emit(OpCodes.Ldc_I4, n)
and generateLdloc (il:ILGenerator) (local:LocalBuilder) = 
    match local.LocalIndex with
    | 0 -> il.Emit(OpCodes.Ldloc_0)
    | 1 -> il.Emit(OpCodes.Ldloc_1)
    | 2 -> il.Emit(OpCodes.Ldloc_2)
    | 3 -> il.Emit(OpCodes.Ldloc_3)
    | s when s < 256 -> il.Emit(OpCodes.Ldloc_S, byte s)
    | n -> il.Emit(OpCodes.Ldloc, n)
and generateStloc (il:ILGenerator) (local:LocalBuilder) =
    match local.LocalIndex with
    | 0 -> il.Emit(OpCodes.Stloc_0)
    | 1 -> il.Emit(OpCodes.Stloc_1)
    | 2 -> il.Emit(OpCodes.Stloc_2)
    | 3 -> il.Emit(OpCodes.Stloc_3)
    | s  when s < 256 -> il.Emit(OpCodes.Stloc_S, byte s)
    | n -> il.Emit(OpCodes.Stloc, local)
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