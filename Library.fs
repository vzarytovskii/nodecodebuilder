namespace IcedTasks

[<AutoOpen>]
module NodeCode =
    open System
    open System.Runtime.CompilerServices
    open System.Threading
    open System.Threading.Tasks
    open Microsoft.FSharp.Core.CompilerServices
    open Microsoft.FSharp.Core.CompilerServices.StateMachineHelpers

    type NodeCode<'T> = unit -> Task<'T>
    type NodeCode = unit -> Task

    [<Struct; NoComparison; NoEquality>]
    type NodeCodeStateMachineData<'T> =
        [<DefaultValue(false)>]
        val mutable Result: 'T

        [<DefaultValue(false)>]
        val mutable MethodBuilder: AsyncTaskMethodBuilder<'T>

    and NodeCodeStateMachine<'TOverall> = ResumableStateMachine<NodeCodeStateMachineData<'TOverall>>
    and NodeCodeResumptionFunc<'TOverall> = ResumptionFunc<NodeCodeStateMachineData<'TOverall>>

    and NodeCodeResumptionDynamicInfo<'TOverall> =
        ResumptionDynamicInfo<NodeCodeStateMachineData<'TOverall>>

    and NodeCodeCode<'TOverall, 'T> = ResumableCode<NodeCodeStateMachineData<'TOverall>, 'T>

    [<NoComparison; NoEquality>]
    type NodeCodeBuilder() =
 
        member inline _.Delay(generator: unit -> NodeCodeCode<'TOverall, 'T>) : NodeCodeCode<'TOverall, 'T> =
            NodeCodeCode<'TOverall, 'T>(fun sm -> (generator ()).Invoke(&sm))

        [<DefaultValue>]
        member inline _.Zero() : NodeCodeCode<'TOverall, unit> = ResumableCode.Zero()

        member inline _.Return(value: 'T) : NodeCodeCode<'T, 'T> =
            NodeCodeCode<'T, _>(fun sm ->
                sm.Data.Result <- value
                true
            )

        member inline _.Combine(task1: NodeCodeCode<'TOverall, unit>, task2: NodeCodeCode<'TOverall, 'T>) : NodeCodeCode<'TOverall, 'T> =
            ResumableCode.Combine(task1, task2)

        member inline _.While([<InlineIfLambda>]guard: unit -> bool, body: NodeCodeCode<'TOverall, unit>) : NodeCodeCode<'TOverall, unit> =
            ResumableCode.While(guard, body)

        member inline _.TryWith(body: NodeCodeCode<'TOverall, 'T>, [<InlineIfLambda>] catch: exn -> NodeCodeCode<'TOverall, 'T>) : NodeCodeCode<'TOverall, 'T> =
            ResumableCode.TryWith(body, catch)

        member inline _.TryFinally(body: NodeCodeCode<'TOverall, 'T>, [<InlineIfLambda>] compensation: unit -> unit) : NodeCodeCode<'TOverall, 'T> =
            ResumableCode.TryFinally(
                body,
                ResumableCode<_, _>(fun _sm ->
                    compensation ()
                    true
                )
            )

        member inline _.For (sequence: seq<'T>, [<InlineIfLambda>]body: 'T -> NodeCodeCode<'TOverall, unit>) : NodeCodeCode<'TOverall, unit> =
            ResumableCode.For(sequence, body)

        static member inline RunDynamic(code: NodeCodeCode<'T, 'T>) : NodeCode<'T> =
            let mutable sm = NodeCodeStateMachine<'T>()
            let initialResumptionFunc = NodeCodeResumptionFunc<'T>(fun sm -> code.Invoke(&sm))

            let resumptionInfo =
                { new NodeCodeResumptionDynamicInfo<'T>(initialResumptionFunc) with
                    member info.MoveNext(sm) =
                        let mutable savedExn = null

                        try
                            sm.ResumptionDynamicInfo.ResumptionData <- null
                            let step = info.ResumptionFunc.Invoke(&sm)

                            if step then
                                sm.Data.MethodBuilder.SetResult(sm.Data.Result)
                            else
                                let mutable awaiter =
                                    sm.ResumptionDynamicInfo.ResumptionData
                                    :?> ICriticalNotifyCompletion

                                assert not (isNull awaiter)
                                sm.Data.MethodBuilder.AwaitUnsafeOnCompleted(&awaiter, &sm)

                        with exn ->
                            savedExn <- exn
                        match savedExn with
                        | null -> ()
                        | exn -> sm.Data.MethodBuilder.SetException exn

                    member _.SetStateMachine(sm, state) =
                        sm.Data.MethodBuilder.SetStateMachine(state)
                }

            fun () ->
                sm.ResumptionDynamicInfo <- resumptionInfo
                sm.Data.MethodBuilder <- AsyncTaskMethodBuilder<'T>.Create ()
                sm.Data.MethodBuilder.Start(&sm)
                sm.Data.MethodBuilder.Task

        member inline _.Run(code: NodeCodeCode<'T, 'T>) : NodeCode<'T> =
            if __useResumableCode then
                __stateMachine<NodeCodeStateMachineData<'T>, NodeCode<'T>>
                    (MoveNextMethodImpl<_>(fun sm ->
                        __resumeAt sm.ResumptionPoint

                        try
                            let __stack_code_fin = code.Invoke(&sm)

                            if __stack_code_fin then
                                sm.Data.MethodBuilder.SetResult(sm.Data.Result)
                        with exn ->
                            sm.Data.MethodBuilder.SetException exn
                    ))
                    (SetStateMachineMethodImpl<_>(fun sm state ->
                        sm.Data.MethodBuilder.SetStateMachine(state)
                    ))
                    (AfterCode<_, NodeCode<'T>>(fun sm ->
                        if
                            isNull SynchronizationContext.Current
                            && obj.ReferenceEquals(TaskScheduler.Current, TaskScheduler.Default)
                        then
                            let mutable sm = sm

                            fun () ->
                                sm.Data.MethodBuilder <- AsyncTaskMethodBuilder<'T>.Create ()
                                sm.Data.MethodBuilder.Start(&sm)
                                sm.Data.MethodBuilder.Task
                        else
                            let sm = sm // copy

                            fun () ->
                                Task.Run<'T>(fun () ->
                                    let mutable sm = sm
                                    sm.Data.MethodBuilder <- AsyncTaskMethodBuilder<'T>.Create ()
                                    sm.Data.MethodBuilder.Start(&sm)
                                    sm.Data.MethodBuilder.Task
                                )
                    ))
            else
                if
                    isNull SynchronizationContext.Current
                    && obj.ReferenceEquals(TaskScheduler.Current, TaskScheduler.Default)
                then
                    NodeCodeBuilder.RunDynamic(code)
                else

                    fun () -> Task.Run<'T>(fun () -> NodeCodeBuilder.RunDynamic (code) ())

    [<AutoOpen>]
    module NodeCodeBuilder =
        let nodeCode = NodeCodeBuilder()

    [<AutoOpen>]
    module Extensions =
        type Awaiter<'TResult1, 'Awaiter
             when 'Awaiter :> ICriticalNotifyCompletion
             and  'Awaiter: (member IsCompleted: bool)
             and  'Awaiter: (member GetResult: unit -> 'TResult1)> = 'Awaiter
        type NodeCodeBuilder with
            [<NoEagerConstraintApplication>]
            static member inline BindDynamic<'TResult1, 'TResult2, 'Awaiter, 'TOverall when Awaiter<'TResult1, 'Awaiter>>
                (sm: byref<_>, [<InlineIfLambda>] getAwaiter: unit -> 'Awaiter, [<InlineIfLambda>] continuation: ('TResult1 -> NodeCodeCode<'TOverall, 'TResult2>)) : bool =

                let mutable awaiter = getAwaiter ()

                let cont =
                    (NodeCodeResumptionFunc<'TOverall>(fun sm ->
                        let result = awaiter.GetResult()
                        (continuation result).Invoke(&sm)
                    ))

                if awaiter.IsCompleted then
                    cont.Invoke(&sm)
                else
                    sm.ResumptionDynamicInfo.ResumptionData <-
                        (awaiter :> ICriticalNotifyCompletion)

                    sm.ResumptionDynamicInfo.ResumptionFunc <- cont
                    false

            [<NoEagerConstraintApplication>]
            member inline _.Bind<'TResult1, 'TResult2, 'Awaiter, 'TOverall when Awaiter<'TResult1, 'Awaiter>>
                ([<InlineIfLambda>] getAwaiter: unit -> 'Awaiter, [<InlineIfLambda>]continuation: ('TResult1 -> NodeCodeCode<'TOverall, 'TResult2>)) : NodeCodeCode<'TOverall, 'TResult2> =

                NodeCodeCode<'TOverall, _>(fun sm ->
                    if __useResumableCode then
                        //-- RESUMABLE CODE START
                        let mutable awaiter = getAwaiter ()

                        let mutable __stack_fin = true

                        if not awaiter.IsCompleted then
                            // This will yield with __stack_yield_fin = false
                            // This will resume with __stack_yield_fin = true
                            let __stack_yield_fin = ResumableCode.Yield().Invoke(&sm)
                            __stack_fin <- __stack_yield_fin

                        if __stack_fin then
                            let result = awaiter.GetResult()
                            (continuation result).Invoke(&sm)
                        else
                            sm.Data.MethodBuilder.AwaitUnsafeOnCompleted(&awaiter, &sm)
                            false
                    else
                        NodeCodeBuilder.BindDynamic<'TResult1, 'TResult2, 'Awaiter, 'TOverall>(
                            &sm,
                            getAwaiter,
                            continuation
                        )
                )

            [<NoEagerConstraintApplication>]
            member inline this.ReturnFrom<'TResult1, 'TResult2, 'Awaiter, 'TOverall when Awaiter<'TResult1, 'Awaiter>>
                ([<InlineIfLambda>] getAwaiter: unit -> 'Awaiter) : NodeCodeCode<_, _> =
                this.Bind((fun () -> getAwaiter ()), (fun v -> this.Return v))

            [<NoEagerConstraintApplication>]
            member inline this.BindReturn<'TResult1, 'TResult2, 'Awaiter, 'TOverall when Awaiter<'TResult1, 'Awaiter>>
                ([<InlineIfLambda>] getAwaiter: unit -> 'Awaiter, [<InlineIfLambda>] f) : NodeCodeCode<'TResult2, 'TResult2> =
                this.Bind((fun () -> getAwaiter ()), (fun v -> this.Return(f v)))


            [<NoEagerConstraintApplication>]
            member inline _.Source<'TResult1, 'TResult2, 'Awaiter, 'TOverall when Awaiter<'TResult1, 'Awaiter>>(getAwaiter: 'Awaiter) : unit -> 'Awaiter =
                (fun () -> getAwaiter)


            [<NoEagerConstraintApplication>]
            member inline _.Source<'TResult1, 'TResult2, 'Awaiter, 'TOverall when Awaiter<'TResult1, 'Awaiter>>
                ([<InlineIfLambda>]getAwaiter: unit -> 'Awaiter) : unit -> 'Awaiter =
                getAwaiter

            [<NoEagerConstraintApplication>]
            member inline _.Source<'TaskLike, 'Awaiter, 'T
                when 'TaskLike: (member GetAwaiter: unit -> 'Awaiter)
                and Awaiter<'T, 'Awaiter>>
                (task: 'TaskLike) : unit -> 'Awaiter =
                task.GetAwaiter
                
            [<NoEagerConstraintApplication>]
            member inline _.Source<'TaskLike, 'Awaiter, 'T
                when ^TaskLike: (member GetAwaiter: unit -> 'Awaiter)
                and Awaiter<'T, 'Awaiter>>
                ([<InlineIfLambda>] task: unit -> ^TaskLike)
                : unit -> 'Awaiter = (task ()).GetAwaiter

            member inline _.Using<'Resource, 'TOverall, 'T when 'Resource :> IDisposable>
                (resource: 'Resource, [<InlineIfLambda>] body: 'Resource -> NodeCodeCode<'TOverall, 'T>) =
                ResumableCode.Using(resource, body)