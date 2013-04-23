﻿module TypeTest

open Fil
open System
open NUnit.Framework
open Microsoft.FSharp.Reflection

let [<Test>] ``can call GetRecordFields on generated record`` () =
    let expected = [|"A",typeof<int>|]
    let recordType = FSharpType.MakeRecordType("MyRecord", expected)
    let properties = FSharpType.GetRecordFields(recordType)
    let actual = [|for pi in properties -> pi.Name, pi.PropertyType|]
    Assert.AreEqual(expected, actual)

let [<Test>] ``can create valid instance of generated record`` () =
    let values = [|"A", 1|]
    let fields = [|for name,value in values -> name, value.GetType()|]
    let recordType = FSharpType.MakeRecordType("MyRecord", fields)
    let args = [|for _, value in values -> box value|] 
    let instance = Activator.CreateInstance(recordType, args) 
    let getValue name = recordType.GetProperty(name).GetValue(instance,[||])
    let actual = [|for name, _ in fields -> getValue name|] 
    let expected = [|for _,value in values -> value|]
    Assert.AreEqual(expected, actual)
            
let [<Test>] ``can call GetUnionCases on generated union`` () =
    let cases =
        [|"A", [||]
          "B", [|typeof<int>|]
        |]
    let unionType = FSharpType.MakeUnionType("Union", cases)
    let actual = FSharpType.GetUnionCases(unionType)
    Assert.AreEqual(cases.Length, actual.Length)