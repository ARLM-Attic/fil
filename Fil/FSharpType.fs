﻿module FSharpType

open System
open System.Reflection
open System.Reflection.Emit

let internal MakeRecord(typeName:string, fields:(string * Type)[]) =
    let name = "GeneratedAssembly"
    let domain = AppDomain.CurrentDomain
    let assembly = domain.DefineDynamicAssembly(AssemblyName(name), AssemblyBuilderAccess.RunAndSave)
    let name = "GeneratedModule"
    let dm = assembly.DefineDynamicModule(name, name+".dll")
    let attributes = TypeAttributes.Public ||| TypeAttributes.Class ||| TypeAttributes.Sealed
    let typeBuilder = dm.DefineType(typeName, attributes)
    let con = typeof<CompilationMappingAttribute>.GetConstructor([|typeof<SourceConstructFlags>|])
    let customBuilder = CustomAttributeBuilder(con, [|SourceConstructFlags.RecordType|])
    typeBuilder.SetCustomAttribute(customBuilder)
    let makeField name t =
        let attributes = FieldAttributes.Assembly
        let fieldBuilder = typeBuilder.DefineField(name+"@", t, attributes)
        let attributes = PropertyAttributes.None
        let propertyBuilder = typeBuilder.DefineProperty(name, attributes, t, [||])
        let customBuilder = CustomAttributeBuilder(con, [|SourceConstructFlags.Field|])
        propertyBuilder.SetCustomAttribute(customBuilder)
        let attributes = MethodAttributes.Public ||| MethodAttributes.HideBySig ||| MethodAttributes.SpecialName
        let methodBuilder = typeBuilder.DefineMethod("get_"+name, attributes, t, [||])
        let il = methodBuilder.GetILGenerator()
        il.Emit(OpCodes.Ldarg_0)
        il.Emit(OpCodes.Ldfld, fieldBuilder)
        il.Emit(OpCodes.Ret)
        propertyBuilder.SetGetMethod(methodBuilder)
        fieldBuilder
    let types = fields |> Array.map snd
    let cb = typeBuilder.DefineConstructor(MethodAttributes.Public, CallingConventions.Standard, types)
    let il = cb.GetILGenerator()
    il.Emit(OpCodes.Ldarg_0)
    il.Emit(OpCodes.Call, typeof<obj>.GetConstructor(Type.EmptyTypes))
    fields |> Array.iteri (fun i (name, t) -> 
        let paramName = name.Substring(0,1).ToLower()+name.Substring(1)
        let param = cb.DefineParameter(i+1, ParameterAttributes.In, paramName)
        let builder = makeField name t
        il.Emit(OpCodes.Ldarg_0)
        il.Emit(OpCodes.Ldarg, param.Position)
        il.Emit(OpCodes.Stfld, builder)
    )
    il.Emit(OpCodes.Ret)
    typeBuilder.CreateType()

let internal MakeUnion (name:string, cases:(string * Type[])[]) =
    raise <| NotImplementedException()