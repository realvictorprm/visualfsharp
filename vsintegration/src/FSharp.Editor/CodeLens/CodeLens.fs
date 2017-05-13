﻿// Copyright (c) Microsoft Corporation.  All Rights Reserved.  Licensed under the Apache License, Version 2.0.  See License.txt in the project root for license information.

namespace rec Microsoft.VisualStudio.FSharp.Editor

open System
open System.Windows.Controls
open System.Windows.Media
open Microsoft.VisualStudio.Text
open Microsoft.VisualStudio.Text.Editor
open Microsoft.VisualStudio.Text.Formatting
open System.ComponentModel.Composition
open Microsoft.VisualStudio.Utilities
open Microsoft.CodeAnalysis
open System.Threading
open Microsoft.FSharp.Compiler.SourceCodeServices
open Microsoft.VisualStudio.Shell
open Microsoft.VisualStudio
open Microsoft.VisualStudio.LanguageServices
open System.Windows
open System.Collections.Generic
open Microsoft.FSharp.Compiler.Range
open Microsoft.FSharp.Compiler
open Microsoft.CodeAnalysis.Editor.Shared.Extensions
open Microsoft.CodeAnalysis.Editor.Shared.Utilities
open Microsoft.CodeAnalysis.Classification
open Internal.Utilities.StructuredFormat
open Microsoft.VisualStudio.Text.Tagging
open System.Collections.Concurrent
open System.Collections
open System.Windows.Media.Animation
open System.Globalization

open Microsoft.VisualStudio.FSharp.Editor.Logging

type CodeLensTag(width, topSpace, baseline, textHeight, bottomSpace, affinity, tag:obj, providerTag:obj) =
    inherit SpaceNegotiatingAdornmentTag(width, topSpace, baseline, textHeight, bottomSpace, affinity, tag, providerTag)
    
type internal CodeLens =
    { TaggedText: Async<(ResizeArray<Layout.TaggedText> * QuickInfoNavigation) option> 
      TrackingSpan: ITrackingSpan
      mutable Computed: bool 
      mutable Ui: UIElement}

type internal CodeLensCacheOption =
    | Unique of CodeLens
    | Ambigious of CodeLens list

type internal CodeLensTagger  
    (
        workspace: Workspace, 
        documentId: Lazy<DocumentId>,
        buffer: ITextBuffer, 
        checker: FSharpChecker,
        projectInfoManager: ProjectInfoManager,
        typeMap: Lazy<ClassificationTypeMap>,
        gotoDefinitionService: FSharpGoToDefinitionService
     ) as self =
    
    let tagsChanged = new Event<EventHandler<SnapshotSpanEventArgs>,SnapshotSpanEventArgs>()
    let formatMap = lazy typeMap.Value.ClassificationFormatMapService.GetClassificationFormatMap "tooltip"
    let visibleAdornments = ConcurrentDictionary()
    let mutable codeLensUIElementCache = ConcurrentDictionary()
    let mutable firstTimeChecked = false
    let mutable bufferChangedCts = new CancellationTokenSource()
    let mutable layoutChangedCts = new CancellationTokenSource()
    let mutable view: IWpfTextView option = None
    let mutable codeLensLayer: IAdornmentLayer option = None
    let mutable recentFirstVsblLineNmbr, recentLastVsblLineNmbr = 0, 0
    let mutable updated = false
    let candidate = "abcdefghijklmnopqrstuvwxyz->ABCDEFGHIJKLMNOPQRSTUVWXYZ"

    let mutable codeLensResultsTracking = ConcurrentDictionary()

    let MeasureString candidate (textBox:TextBox)=
            let formattedText = 
                FormattedText(candidate, CultureInfo.CurrentUICulture, FlowDirection.LeftToRight,
                    Typeface(textBox.FontFamily, textBox.FontStyle, textBox.FontWeight, textBox.FontStretch),
                    textBox.FontSize, Brushes.Black)
            Size(formattedText.Width, formattedText.Height)

    let MeasureTextBox textBox =
        MeasureString candidate textBox

    let layoutTagToFormatting (layoutTag: LayoutTag) =
        layoutTag
        |> RoslynHelpers.roslynTag
        |> ClassificationTags.GetClassificationTypeName
        |> typeMap.Value.GetClassificationType
        |> formatMap.Value.GetTextProperties   

    let executeCodeLenseAsync () =  
        let uiContext = SynchronizationContext.Current
        updated <- true
        asyncMaybe {
            let! view = view
            do! Async.Sleep 800 |> liftAsync
            logInfof "Rechecking code due to buffer edit!"
            let! document = workspace.CurrentSolution.GetDocument(documentId.Value) |> Option.ofObj
            let! options = projectInfoManager.TryGetOptionsForEditingDocumentOrProject(document)
            let! _, _, checkFileResults = checker.ParseAndCheckDocument(document, options, allowStaleResults = true)
            logInfof "Getting uses of all symbols!"
            let! symbolUses = checkFileResults.GetAllUsesOfAllSymbolsInFile() |> liftAsync
            let textSnapshot = view.TextSnapshot.TextBuffer.CurrentSnapshot
            logInfof "Updating due to buffer edit!"
            // Clear existing data and cache flags
            let newUIElementCache = ConcurrentDictionary()
            let newCodeLensResultsTracking = ConcurrentDictionary()
            
            let setResultToComputed lineNumber lineStr =
                let res = codeLensResultsTracking.[lineStr]
                let c = 
                    match res with
                    | Unique c -> c
                    | Ambigious l ->
                        List.find (fun c -> 
                                    let current = view.TextBuffer.CurrentSnapshot
                                    current.GetLineNumberFromPosition(int(c.TrackingSpan.GetSpan(current).Start)) = lineNumber) l
                c.Computed <- true
                
            let useResults (displayContext: FSharpDisplayContext, func: FSharpMemberOrFunctionOrValue, lineStr: string) =
                async {
                    let lineNumber = Line.toZ func.DeclarationLocation.StartLine
                    //logInfof "Computing cache for line %A with content %A" lineNumber lineStr
                    if (lineNumber >= 0 || lineNumber < textSnapshot.LineCount) && 
                        not func.IsPropertyGetterMethod && 
                        not func.IsPropertySetterMethod then
                
                        match func.FullTypeSafe with
                        | Some ty ->
                            let! displayEnv = checkFileResults.GetDisplayEnvForPos(func.DeclarationLocation.Start)
                            
                            let displayContext =
                                match displayEnv with
                                | Some denv -> FSharpDisplayContext(fun _ -> denv)
                                | None -> displayContext
                            
                            let typeLayout = ty.FormatLayout(displayContext)
                            let taggedText = ResizeArray()
                            Layout.renderL (Layout.taggedTextListR taggedText.Add) typeLayout |> ignore
                            let navigation = QuickInfoNavigation(gotoDefinitionService, document, func.DeclarationLocation)
                            let line = textSnapshot.GetLineFromLineNumber(lineNumber)
                            // This is only used with a cached async so flag the data as ready to use
                            setResultToComputed lineNumber lineStr
                            // Because the data is available notify that this line should be updated, displaying the results
                            tagsChanged.Trigger(self, SnapshotSpanEventArgs(SnapshotSpan(line.Start, line.End)))
                            return Some (taggedText, navigation)
                        | None -> return None
                    else return None
                }
            let reuseResults taggedText navigation =
                async{
                     return Some (taggedText, navigation)
                }
            
            let isTrackingSpanValid (trackingSpan:ITrackingSpan) text =
                let span = trackingSpan.GetSpan(textSnapshot)
                let spanText = span.GetText()
                spanText = text
            
            let isResultStillValid lineNumber text =
                let res = codeLensResultsTracking.[text]
                match res with
                | Unique c -> 
                    if c.Computed && isTrackingSpanValid c.TrackingSpan text then
                        Some c
                    else None
                | Ambigious l ->
                    let isCorrectTrackingSpan (res:CodeLens) =
                        let tspan = res.TrackingSpan
                        let tLineNumber = textSnapshot.GetLineNumberFromPosition(int (tspan.GetStartPoint textSnapshot))
                        isTrackingSpanValid tspan text && tLineNumber = lineNumber 
                    l |> List.tryFind isCorrectTrackingSpan
            
            let addResult text result =
                if newCodeLensResultsTracking.ContainsKey(text) then
                    let current = newCodeLensResultsTracking.[text]
                    match current with
                    | Unique c -> newCodeLensResultsTracking.[text] <- Ambigious [c; result]
                    | Ambigious l -> newCodeLensResultsTracking.[text] <- Ambigious (l |> List.append [result])
                else
                    newCodeLensResultsTracking.[text] <- Unique result
                    
            for symbolUse in symbolUses do
                if symbolUse.IsFromDefinition then
                    match symbolUse.Symbol with
                    | :? FSharpEntity as entity ->
                        for func in entity.MembersFunctionsAndValues do
                            let lineNumber = Line.toZ func.DeclarationLocation.StartLine
                            //let start, length = (func.DeclarationLocation.Start, int func.DeclarationLocation.End - int func.DeclarationLocation.Start)
                            let line = textSnapshot.GetLineFromLineNumber(int lineNumber)
                            let text = line.GetText()
                            if codeLensResultsTracking.ContainsKey(text) then
                                let resultOption = isResultStillValid lineNumber text
                                match resultOption with
                                | None -> ()
                                | Some res ->
                                    let! taggedText, _ = res.TaggedText
                                    let updatedResult = 
                                        { TaggedText = Async.cache (reuseResults taggedText (QuickInfoNavigation(gotoDefinitionService, document, func.DeclarationLocation)))
                                          TrackingSpan = textSnapshot.CreateTrackingSpan(line.Extent.Span, SpanTrackingMode.EdgePositive)
                                          Computed = true
                                          Ui = null}
                                    addResult text updatedResult
                                    if codeLensUIElementCache.ContainsKey(text) then
                                        newUIElementCache.[text] <- codeLensUIElementCache.[text]
                            else
                                let result = 
                                    { TaggedText = Async.cache (useResults (symbolUse.DisplayContext, func, text))
                                      TrackingSpan = textSnapshot.CreateTrackingSpan(line.Extent.Span, SpanTrackingMode.EdgePositive)
                                      Computed = false
                                      Ui = null}
                                codeLensResultsTracking.[text] <- Unique result
                                    
                    | _ -> ()
            
            let unusedUIElements = 
                    let current = Set newUIElementCache.Keys
                    let last = Set codeLensUIElementCache.Keys
                    Set.difference last current
            codeLensResultsTracking <- newCodeLensResultsTracking
            do! Async.SwitchToContext uiContext |> liftAsync
           
            let! layer = codeLensLayer
            unusedUIElements |> Set.iter(fun key -> layer.RemoveAdornment(codeLensUIElementCache.[key]))
            codeLensUIElementCache <- newUIElementCache
            updated <- true
            tagsChanged.Trigger(self, SnapshotSpanEventArgs(SnapshotSpan(view.TextViewLines.FirstVisibleLine.Start, view.TextViewLines.LastVisibleLine.End)))
            logInfof "Finished updating code lens."
            
            if not firstTimeChecked then
                firstTimeChecked <- true
        } |> Async.Ignore
    
    do async {
          let mutable numberOfFails = 0
          while not firstTimeChecked && numberOfFails < 10 do
              try
                  do! executeCodeLenseAsync()
                  do! Async.Sleep(1000)
              with
              | e -> logErrorf "Code Lens startup failed with: %A" e
                     numberOfFails <- numberOfFails + 1
       } |> Async.Start

    let layoutUIElementOnLine (view:IWpfTextView) (line:SnapshotSpan) (ui:UIElement) =
        // Get the real offset so that the code lens are placed respectively to their content
        let offset =
            [0..line.Length - 1] |> Seq.tryFind (fun i -> not (Char.IsWhiteSpace (line.Start.Add(i).GetChar())))
            |> Option.defaultValue 0
        let realStart = line.Start.Add(offset)
        // Get the geometry which respects the changed height due to the SpaceAdornmentTag
        let __ = MeasureTextBox (ui :?> TextBox)
        let geometry = 
            view.TextViewLines.GetCharacterBounds(realStart)
        if (ui.GetValue(Canvas.LeftProperty) :?> float) < geometry.Left then
                Canvas.SetLeft(ui, geometry.Left)
        Canvas.SetTop(ui, geometry.Top)
    
    /// Creates the code lens ui elements for the specified text view line
    let createCodeLensUIElementByLine (line: ITextViewLine) =
        let text = line.Extent.GetText() // Get our unique identifier (the content of the line)
        asyncMaybe {
            let! view = view
            let self.SpanCon
            let! lens = codeLensResults.TryFind(text) // Check whether this line has code lens, if not return None
            match lens.Computed, codeLensUIElementCache.ContainsKey(text) with
            // The line is already computed and has an existing UI element which is proved to be safe to use
            | true, true -> 
                let textBox = codeLensUIElementCache.[text]
                layoutUIElementOnLine view line.Extent textBox
                return Some textBox
            // The line isn't computed yet so start the computation and return None (we won't stop the UI thread further!)
            | false, true ->
                lens.TaggedText |> Async.Ignore |> RoslynHelpers.StartAsyncSafe CancellationToken.None
                let unusedElement = null
                if codeLensUIElementCache.TryGetValue(text, &unusedElement) && not(isNull(unusedElement)) then
                    return Some unusedElement
                else
                    return None
            // The line is already computed but the UI element hasn't been created yet
            | _, _ ->
                let! taggedText, navigation = lens.TaggedText
                let textBox = new TextBlock(Width = view.ViewportWidth, Background = Brushes.Transparent, Opacity = 0.5, TextTrimming = TextTrimming.WordEllipsis)
                DependencyObjectExtensions.SetDefaultTextProperties(textBox, formatMap.Value)
                for text in taggedText do
                    let run = Documents.Run text.Text
                    DependencyObjectExtensions.SetTextProperties (run, layoutTagToFormatting text.Tag)
                    let inl =
                        match text with
                        | :? Layout.NavigableTaggedText as nav when navigation.IsTargetValid nav.Range ->
                            let h = Documents.Hyperlink(run, ToolTip = nav.Range.FileName)
                            h.Click.Add (fun _ -> navigation.NavigateTo nav.Range)
                            h :> Documents.Inline
                        | _ -> run :> _
                    textBox.Inlines.Add inl
                layoutUIElementOnLine view line.Extent textBox
                textBox.Opacity <- 0.5
                return Some textBox
        } |> Async.map (fun ui ->
               match ui with
               | Some (Some ui) ->
                   codeLensUIElementCache.TryAdd(text, ui) |> ignore
                   Some ui
               | _ -> 
                   let unusedElement = null
                   codeLensUIElementCache.TryRemove(text, &unusedElement) |> ignore
                   None)
    do buffer.Changed.AddHandler(fun _ e -> (self.BufferChanged e))

    member __.BufferChanged ___ =
        bufferChangedCts.Cancel() // Stop all ongoing async workflow. 
        bufferChangedCts.Dispose()
        bufferChangedCts <- new CancellationTokenSource()
        executeCodeLenseAsync () |> Async.Ignore |> RoslynHelpers.StartAsyncSafe bufferChangedCts.Token
    
    member __.SetView value = 
        view <- Some value
        codeLensLayer <- Some (value.GetAdornmentLayer "CodeLens")

    member __.TriggerTagsChanged args = tagsChanged.Trigger(self, args)

    member val MaxCacheOfNonVisibleLines = 20 with get, set

    // Process the layout changed event from the ITextView
    member this.LayoutChanged (__) =
        let uiContext = SynchronizationContext.Current
        let recentVisibleLineNumbers = Set [recentFirstVsblLineNmbr .. recentLastVsblLineNmbr]
        let firstVisibleLineNumber, lastVisibleLineNumber =
            match view with
            | None -> 0, 0
            | Some view ->
                let first, last = 
                    view.TextViewLines.FirstVisibleLine, 
                    view.TextViewLines.LastVisibleLine
                let buffer = buffer.CurrentSnapshot
                buffer.GetLineNumberFromPosition(first.Start.Position),
                buffer.GetLineNumberFromPosition(last.Start.Position)
        let visibleLineNumbers = Set [firstVisibleLineNumber .. lastVisibleLineNumber]
        let nonVisibleLineNumbers = Set.difference recentVisibleLineNumbers visibleLineNumbers
        let newVisibleLineNumbers = Set.difference visibleLineNumbers recentVisibleLineNumbers
        
        if nonVisibleLineNumbers.Count > 0 || newVisibleLineNumbers.Count > 0 then
            let buffer = buffer.CurrentSnapshot
            let view = view.Value
            for lineNumber in nonVisibleLineNumbers do
                if lineNumber > 0 && lineNumber < buffer.LineCount then
                    let line = buffer.GetLineFromLineNumber(lineNumber)
                    let text = line.Extent.GetText()
                    let mutable ui = null
                    if(codeLensUIElementCache.TryGetValue(line.Extent.GetText(), &ui)) then
                        ui.Visibility <- Visibility.Collapsed
                        let mutable temp = null
                        visibleAdornments.TryRemove(text, &temp) |> ignore
            for lineNumber in newVisibleLineNumbers do
                let line = buffer.GetLineFromLineNumber(lineNumber)
                let mutable ui = null
                let text = line.Extent.GetText()
                if(codeLensUIElementCache.TryGetValue(text, &ui)) then
                    ui.Visibility <- Visibility.Visible
                    layoutUIElementOnLine view line.Extent ui
                    visibleAdornments.TryAdd(text, ui) |> ignore
        // Save the new first and last visible lines for tracking
        recentFirstVsblLineNmbr <- firstVisibleLineNumber
        recentLastVsblLineNmbr <- lastVisibleLineNumber
        // We can cancel existing stuff because the algorithm supports abortion without any data loss
        layoutChangedCts.Cancel()
        layoutChangedCts.Dispose()
        layoutChangedCts <- new CancellationTokenSource()

        asyncMaybe {
            do! Async.SwitchToContext uiContext |> liftAsync
            do! Async.Sleep(5) |> liftAsync
            let! view = view
            let! layer = codeLensLayer
            if nonVisibleLineNumbers.Count > 0 || newVisibleLineNumbers.Count > 0 || updated then
                let buffer = buffer.CurrentSnapshot
                let linesContent = 
                        Set[for lineNumber in visibleLineNumbers do
                            let line = buffer.GetLineFromLineNumber(lineNumber)
                            let mutable ui = null
                            let text = line.Extent.GetText()
                            if(codeLensUIElementCache.TryGetValue(text, &ui)) then
                                ui.Visibility <- Visibility.Visible
                                layoutUIElementOnLine view line.Extent ui
                                ()
                            yield text ]
                let toMap dictionary = 
                    (dictionary :> seq<_>)
                    |> Seq.map (|KeyValue|)
                    |> Map.ofSeq
                let wrongAdornments = 
                    visibleAdornments |> toMap 
                    |> Map.filter(fun key _ -> not(linesContent.Contains(key)))
                wrongAdornments 
                |> Map.iter 
                    (fun key value -> 
                        value.Visibility <- Visibility.Collapsed
                        let mutable temp = null
                        visibleAdornments.TryRemove(key, &temp) |> ignore)
                updated <- false

            do! Async.Sleep(495) |> liftAsync

            let visibleSpan =
                let first, last = 
                    view.TextViewLines.FirstVisibleLine, 
                    view.TextViewLines.LastVisibleLine
                SnapshotSpan(first.Start, last.End)
            let customVisibleLines = view.TextViewLines.GetTextViewLinesIntersectingSpan visibleSpan
            let isLineVisible (line:ITextViewLine) = line.IsValid &&  not(codeLensUIElementCache.ContainsKey(line.Extent.GetText()))
            let linesToProcess = customVisibleLines |> Seq.filter isLineVisible

            for line in linesToProcess do
                try
                    if not (Seq.isEmpty (line.GetAdornmentTags this)) then
                        let da = DoubleAnimation(From = Nullable 0., To = Nullable 0.5, Duration = Duration(TimeSpan.FromSeconds 0.4))
                        let! res = createCodeLensUIElementByLine line |> liftAsync
                        match res with
                        | Some textBox ->
                            layer.AddAdornment(AdornmentPositioningBehavior.ViewportRelative, Nullable(), 
                                this, textBox, AdornmentRemovedCallback(fun _ _ -> ())) |> ignore
                            textBox.BeginAnimation(UIElement.OpacityProperty, da)
                        | None -> ()
                with e -> logExceptionWithContext (e, "LayoutChanged, processing new visible lines")
        }
        |> Async.Ignore |> RoslynHelpers.StartAsyncSafe layoutChangedCts.Token
    
    member __.Tags = ConcurrentDictionary()

    member __.GetResultsForSpan(tagSpan:SnapshotSpan) =
        let res = codeLensResultsTracking.[tagSpan.GetText()]
        let snapshot = buffer.CurrentSnapshot
        let trackingSpanMatchesSpan (tSpan: ITrackingSpan) =
            let span = tSpan.GetSpan(snapshot)
            span.IntersectsWith(tagSpan)

        match res with
        | Unique c -> trackingSpanMatchesSpan c.TrackingSpan
        | Ambigious l -> 
            l |> List.exists (fun c -> trackingSpanMatchesSpan c.TrackingSpan)

    member __.SpanContainsTag (tagSpan:SnapshotSpan) =
        let res = codeLensResultsTracking.[tagSpan.GetText()]
        let snapshot = buffer.CurrentSnapshot
        let trackingSpanMatchesSpan (tSpan: ITrackingSpan) =
            let span = tSpan.GetSpan(snapshot)
            span.IntersectsWith(tagSpan)

        match res with
        | Unique c -> trackingSpanMatchesSpan c.TrackingSpan
        | Ambigious l -> 
            l |> List.exists (fun c -> trackingSpanMatchesSpan c.TrackingSpan)

    interface ITagger<CodeLensTag> with
        [<CLIEvent>]
        member __.TagsChanged = tagsChanged.Publish
        override this.GetTags spans =
            seq {
                let translatedSpans = 
                    spans 
                    |> Seq.map (fun span -> span.TranslateTo(buffer.CurrentSnapshot, SpanTrackingMode.EdgeExclusive))
                    |> NormalizedSnapshotSpanCollection
                
                for tagSpan in translatedSpans do
                    if self.SpanContainsTag tagSpan then
                        yield TagSpan(tagSpan, CodeLensTag(0., 12., 0., 0., 0., PositionAffinity.Predecessor, this, this)) :> ITagSpan<CodeLensTag>
            }

[<Export(typeof<IWpfTextViewCreationListener>)>]
[<Export(typeof<ITaggerProvider>)>]
[<TagType(typeof<CodeLensTag>)>]
[<ContentType(FSharpConstants.FSharpContentTypeName)>]
[<TextViewRole(PredefinedTextViewRoles.Document)>]
type internal CodeLensProvider  
    [<ImportingConstructor>]
    (
        textDocumentFactory: ITextDocumentFactoryService,
        checkerProvider: FSharpCheckerProvider,
        projectInfoManager: ProjectInfoManager,
        typeMap: Lazy<ClassificationTypeMap>,
        gotoDefinitionService: FSharpGoToDefinitionService
    ) as __ =

    let taggers = ResizeArray()
    
    let componentModel = Package.GetGlobalService(typeof<ComponentModelHost.SComponentModel>) :?> ComponentModelHost.IComponentModel
    let workspace = componentModel.GetService<VisualStudioWorkspace>()

    /// Returns an provider for the textView if already one has been created. Else create one.
    let getSuitableAdornmentProvider (buffer) =
        let res = taggers |> Seq.tryFind(fun (view, _) -> view = buffer)
        match res with
        | Some (_, res) -> res
        | None ->
            let documentId = 
                lazy (
                    match textDocumentFactory.TryGetTextDocument(buffer) with
                    | true, textDocument ->
                         Seq.tryHead (workspace.CurrentSolution.GetDocumentIdsWithFilePath(textDocument.FilePath))
                    | _ -> None
                    |> Option.get
                )


            let tagger = CodeLensTagger(workspace, documentId, buffer, checkerProvider.Checker, projectInfoManager, typeMap, gotoDefinitionService)
            taggers.Add((buffer, tagger))
            tagger
    [<Export(typeof<AdornmentLayerDefinition>); Name("CodeLens");
      Order(Before = PredefinedAdornmentLayers.Text);
      TextViewRole(PredefinedTextViewRoles.Document)>]

    member val CodeLensAdornmentLayerDefinition : AdornmentLayerDefinition = null with get, set

    interface IWpfTextViewCreationListener with
        
        override __.TextViewCreated view =
            let tagger = getSuitableAdornmentProvider view.TextBuffer
            tagger.SetView view
            view.LayoutChanged.AddHandler(fun _ e -> tagger.LayoutChanged e)
            // The view has been initialized. Notify that we can now theoretically display CodeLens
            tagger.TriggerTagsChanged (SnapshotSpanEventArgs(SnapshotSpan(view.TextViewLines.FirstVisibleLine.Start, view.TextViewLines.LastVisibleLine.End)))
            ()
             

    interface ITaggerProvider with
        override __.CreateTagger(buffer) = box (getSuitableAdornmentProvider buffer) :?> _
