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

type MyUnion =
    | Case0
    | Case1
    | Case2 of int
    | Case3 of int * string

type CaseName = string
type Field = string * Type

let internal MakeUnion (typeName:string, cases:(CaseName * Field[])[]) =
    let name = "GeneratedAssembly"
    let domain = AppDomain.CurrentDomain
    let assembly = domain.DefineDynamicAssembly(AssemblyName(name), AssemblyBuilderAccess.RunAndSave)
    let name = "GeneratedModule"
    let dm = assembly.DefineDynamicModule(name, name+".dll")
    let attributes = TypeAttributes.Public ||| TypeAttributes.Class ||| TypeAttributes.Abstract
    let unionTypeBuilder = dm.DefineType(typeName, attributes)
    
    // Set CompilationMappingAttribute to SumType
    let con = typeof<CompilationMappingAttribute>.GetConstructor([|typeof<SourceConstructFlags>|])
    let customBuilder = CustomAttributeBuilder(con, [|SourceConstructFlags.SumType|])
    unionTypeBuilder.SetCustomAttribute(customBuilder)
   
    // Define Tag field
    let attributes = FieldAttributes.Assembly ||| FieldAttributes.InitOnly
    let tagFieldBuilder = unionTypeBuilder.DefineField("_tag", typeof<int>, attributes)

    // Define Tag property
    let attributes = PropertyAttributes.None
    let tagPropertyBuilder = unionTypeBuilder.DefineProperty("Tag", attributes, typeof<int>, [||])
    let attributes = MethodAttributes.Public ||| MethodAttributes.HideBySig ||| MethodAttributes.SpecialName
    let tagMethodBuilder = unionTypeBuilder.DefineMethod("get_Tag", attributes, typeof<int>, [||])
    let il = tagMethodBuilder.GetILGenerator()
    il.Emit(OpCodes.Ldarg_0)
    il.Emit(OpCodes.Ldfld, tagFieldBuilder)
    il.Emit(OpCodes.Ret)
    tagPropertyBuilder.SetGetMethod(tagMethodBuilder)

    // Define union constructor
    let unionTypeConstructor = 
        unionTypeBuilder.DefineConstructor(MethodAttributes.Public, CallingConventions.Standard, [|typeof<int>|])
    let il = unionTypeConstructor.GetILGenerator()
    il.Emit(OpCodes.Ldarg_0)
    il.Emit(OpCodes.Call, typeof<obj>.GetConstructor(Type.EmptyTypes))
    let param = unionTypeConstructor.DefineParameter(1, ParameterAttributes.In, "tag")
    il.Emit(OpCodes.Ldarg_0)
    il.Emit(OpCodes.Ldarg, param.Position)
    il.Emit(OpCodes.Stfld, tagFieldBuilder)
    il.Emit(OpCodes.Ret)

    /// Cases
    let cases = cases |> Array.mapi (fun tag (name,fields) -> tag,name,fields)

    // Define tags
    let attributes = TypeAttributes.Class ||| TypeAttributes.NestedPublic ||| TypeAttributes.Abstract ||| TypeAttributes.Sealed
    let tagsBuilder = unionTypeBuilder.DefineNestedType("Tags", attributes)
    for tag,name,_ in cases do
        let attributes = FieldAttributes.Public ||| FieldAttributes.Static ||| FieldAttributes.Literal
        let tagFieldBuilder = tagsBuilder.DefineField(name, typeof<int>, attributes)
        tagFieldBuilder.SetConstant(tag)
    tagsBuilder.CreateType() |> ignore

    // Define case properties
    cases 
    |> Array.filter (fun (_,_,fields) -> fields.Length = 0)
    |> Array.iter (fun (tag, caseName, _) ->
        let attributes = PropertyAttributes.None
        let propertyBuilder = 
            unionTypeBuilder.DefineProperty(caseName, attributes, CallingConventions.Standard, unionTypeBuilder, [||])
        let attributes = MethodAttributes.Public ||| MethodAttributes.HideBySig ||| MethodAttributes.SpecialName
        let methodBuilder = unionTypeBuilder.DefineMethod("get_"+caseName, attributes, unionTypeBuilder, [||])
        let il = methodBuilder.GetILGenerator()
        il.Emit(OpCodes.Ldarg_0)
        il.Emit(OpCodes.Ldc_I4, tag)
        il.Emit(OpCodes.Newobj, unionTypeConstructor)
        il.Emit(OpCodes.Ret)
        propertyBuilder.SetGetMethod(methodBuilder)
    )

    // Define case types
    let caseTypes =
        cases 
        |> Array.filter (fun (_,_,fields) -> fields.Length > 0) 
        |> Array.map (fun (tag,name,fields) ->
            let attributes = TypeAttributes.Class ||| TypeAttributes.NestedAssembly
            let caseBuilder = unionTypeBuilder.DefineNestedType(name, attributes)
            let types = fields |> Array.map snd
            let cb = caseBuilder.DefineConstructor(MethodAttributes.Public, CallingConventions.Standard, types)
            let il = cb.GetILGenerator()
            il.Emit(OpCodes.Ldarg_0)
            il.Emit(OpCodes.Call, typeof<obj>.GetConstructor(Type.EmptyTypes))
            il.Emit(OpCodes.Ret)
            name, fields, caseBuilder
        )

    // Define case new methods
    for caseName, fields, caseBuilder in caseTypes do
        let types = fields |> Array.map snd
        let methodBuilder = unionTypeBuilder.DefineMethod("New"+caseName, MethodAttributes.Static, unionTypeBuilder, types)
        let il = methodBuilder.GetILGenerator()
        types |> Array.iteri (fun i t -> il.Emit(OpCodes.Ldarg, i+1))
        il.Emit(OpCodes.Newobj, caseBuilder)
        il.Emit(OpCodes.Ret)
    
    /// Parent type
    let parent = unionTypeBuilder.CreateType()

    // Create case types
    for _,_,caseBuilder in caseTypes do 
        caseBuilder.SetParent(parent)
        caseBuilder.CreateType() |> ignore

    // Debug
    assembly.Save("GeneratedModule.dll")

    // Code not complete
    //raise <| NotImplementedException()
    parent