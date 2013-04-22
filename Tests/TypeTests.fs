﻿module TypeTest

open Fil
open System
open NUnit.Framework
open Microsoft.FSharp.Reflection

let [<Test>] ``fields of generated record`` () =
    let expected = [|"A",typeof<int>|]
    let recordType = FSharpType.MakeRecord("MyRecord", expected)
    let properties = FSharpType.GetRecordFields(recordType)
    let actual = [|for pi in properties -> pi.Name, pi.PropertyType|]
    Assert.AreEqual(expected, actual)

let [<Test>] ``constructor of generated record`` () =
    let values = [|"A", 1|]
    let fields = [|for name,value in values -> name, value.GetType()|]
    let recordType = FSharpType.MakeRecord("MyRecord", fields)
    let args = [|for _, value in values -> box value|] 
    let instance = Activator.CreateInstance(recordType, args) 
    let actual = 
        [|for name, _ in fields -> 
            recordType.GetProperty("A").GetValue(instance,[||])
        |]
    let expected = [|for _,value in values -> value|]
    Assert.AreEqual(expected, actual)