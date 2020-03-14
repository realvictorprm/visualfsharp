// Copyright (c) Microsoft Corporation.  All Rights Reserved.  See License.txt in the project root for license information.

// Various tests for Microsoft.FSharp.Quotations

namespace FSharp.Core.UnitTests.FSharp_Core.Microsoft_FSharp_Quotations

open System
open FSharp.Core.UnitTests.LibraryTestFx
open NUnit.Framework
open FSharp.Quotations
open FSharp.Quotations.Patterns

type E = Microsoft.FSharp.Quotations.Expr;;

type StaticIndexedPropertyTest() =
    static member IdxProp with get (n : int) = n + 1

module Check =
    let argumentException f =
        let mutable ex = false
        try
            f () |> ignore
        with
        |   :? System.ArgumentException-> ex <- true
        Assert.IsTrue(ex, "InvalidOperationException expected")

[<TestFixture>]
type FSharpQuotationsTests() =
    
    [<Test>]
    member x.MethodInfoNRE() =
        let f() = 
            E.Call(null, []) |> ignore
        CheckThrowsArgumentNullException f

    [<Test>]
    member x.FieldInfoNRE() =
        let f() =
            E.FieldGet(null) |> ignore
        CheckThrowsArgumentNullException f
    
    [<Test>]
    member x.ConstructorNRE() =
        let f() =
            E.NewObject(null,[]) |> ignore
        CheckThrowsArgumentNullException f

    [<Test>]
    member x.PropertyInfoNRE() =
        let f() =
            E.PropertyGet(null,[]) |> ignore
        CheckThrowsArgumentNullException f
        
    [<Test>]
    member x.UnionCaseInfoNRE() =
        let f() =
            E.NewUnionCase(Unchecked.defaultof<Microsoft.FSharp.Reflection.UnionCaseInfo>,[]) |> ignore
        CheckThrowsArgumentNullException f
    
    [<Test>]
    member x.ReShapeTypechecking_Let() = 
        let q0 = <@ let a = 1 in a @>
        match q0 with
        |   ExprShape.ShapeCombination(shape, [value;lambda]) ->
                let goodValue = <@ 2 @>
                ExprShape.RebuildShapeCombination(shape, [goodValue;lambda]) |> ignore
        |   _ -> Assert.Fail()
        let q1 = <@ let a = 1 in a @>
        match q1 with
        |   ExprShape.ShapeCombination(shape, [value;lambda]) ->
                let wrongValue = <@ "!" @>
                Check.argumentException(fun () -> ExprShape.RebuildShapeCombination(shape, [wrongValue;lambda]))
        |   _ -> Assert.Fail()

    [<Test>]
    member x.ReShapeStaticIndexedProperties() = 
        let q0 = <@ StaticIndexedPropertyTest.IdxProp 5 @>
        match q0 with
        |   ExprShape.ShapeCombination(shape, args) ->
                try
                    ExprShape.RebuildShapeCombination(shape, args) |> ignore
                with
                | _ -> Assert.Fail()
        |   _ -> Assert.Fail()

    [<Test>]
    member x.GetConstructorFiltersOutStaticConstructor() =
        ignore <@ System.Exception() @>

    [<Test>]
    member x.``NewStructTuple literal should be recognized by NewStructTuple active pattern`` () =
        match <@ struct(1, "") @> with
        | NewStructTuple [ Value(:? int as i, _) ; Value(:? string as s, _) ] when i = 1 && s = "" -> ()
        | _ -> Assert.Fail()


    [<Test>]
    member x.``NewStructTuple literal should be recognized by NewTuple active pattern`` () =
        match <@ struct(1, "") @> with
        | NewTuple [ Value(:? int as i, _) ; Value(:? string as s, _) ] when i = 1 && s = "" -> ()
        | _ -> Assert.Fail()

    [<Test>]
    member x.``NewTuple literal should not be recognized by NewStructTuple active pattern`` () =
        match <@ (1, "") @> with
        | NewStructTuple _ -> Assert.Fail()
        | _ -> ()

    [<Test>]
    member x.``NewStructTuple should be recognized by NewStructTuple active pattern`` () =
        let expr = Expr.NewStructTuple(typeof<struct(_ * _)>.Assembly, [ <@@ 1 @@>; <@@ "" @@> ])
        match expr with
        | NewStructTuple [ Value(:? int as i, _) ; Value(:? string as s, _) ] when i = 1 && s = "" -> ()
        | _ -> Assert.Fail()

    [<Test>]
    member x.``NewStructTuple should be recognized by NewTuple active pattern`` () =
        let expr = Expr.NewStructTuple(typeof<struct(_ * _)>.Assembly, [ <@@ 1 @@>; <@@ "" @@> ])
        match expr with
        | NewTuple [ Value(:? int as i, _) ; Value(:? string as s, _) ] when i = 1 && s = "" -> ()
        | _ -> Assert.Fail()

    [<Test>]
    member x.``NewTuple should not be recognized by NewStructTuple active pattern`` () =
        let expr = Expr.NewTuple [ <@@ 1 @@>; <@@ "" @@> ]
        match expr with
        | NewStructTuple _ -> Assert.Fail()
        | _ -> ()