module Program

open Internal.Utilities.CancellableTasks
open System.Threading
open System.Threading.Tasks

type Foobar =
    static member ReturnsAsync() = async { return "async" }
    static member ReturnsTask() = task { return "task" }
    static member ReturnsNonGenericTask() = task { return "foo" } :> Tasks.Task
    static member ReturnsCancelledTask<'T>() : Task<'T> =
        printfn "ReturnsCancelledTask"
        let cancelledCt = CancellationToken(true)
        Task.FromCanceled<'T>(cancelledCt)
    static member ReturnCancellableTask() =
        cancellableTask {
            let! ct = CancellableTask.getCurrentCancellationToken()
            do printfn $"CancellableTaskBuilder CancellationToken: {ct.GetHashCode()}"
            do printfn $"CancellableTaskBuilder CancellationToken.IsCancellationRequested: {ct.IsCancellationRequested}"
            return "cancellableTask"
        }
    static member ReturnsOptionSome() : string option = Some "some value"
    static member ReturnsOptionNone() : string option = None
    static member ReturnsTaskOptionSome() : Tasks.Task<string option> = task { return Some "some task value" }
    static member ReturnsTaskOptionNone() : Tasks.Task<string option> = task { return None }
    static member CancellsTheToken() =
        cancellableTask {
            let! ct = CancellableTask.getCurrentCancellationToken()
            do printfn $"CancellsTheTokenViaLinkedCts CancellationToken: {ct.GetHashCode()}"

            do printfn $"CancellsTheTokenViaLinkedCts CancellationToken.IsCancellationRequested: {ct.IsCancellationRequested}"

            return "cancellableTaskCancellationRequested"
        }

[<EntryPoint>]
let main _ =

    let cts = new CancellationTokenSource()
    let cancellationToken = cts.Token

    let computation =
        cancellableTask {
            let! ct = CancellableTask.getCurrentCancellationToken()
            printfn $"Topmost CancellationToken: {ct.GetHashCode()}"

            let! asyncthing = Foobar.ReturnsAsync()
            do printfn $"async: {asyncthing}"

            let! taskthing = Foobar.ReturnsTask()
            do printfn $"task: {taskthing}"

            let! nongenerictaskthing = Foobar.ReturnsNonGenericTask()

            let! cancellableTaskthing = Foobar.ReturnCancellableTask()
            do printfn $"cancellableTask: {cancellableTaskthing}"

            let! cancellationRequested = Foobar.CancellsTheToken()
            do printfn $"cancellableTaskCancellationRequested: {cancellationRequested}"


            let! cancellableTaskthing = Foobar.ReturnCancellableTask()
            do printfn $"cancellableTask: {cancellableTaskthing}"

            let! taskOptionSome = Foobar.ReturnsTaskOptionSome()

            do printfn $"taskOptionSome: {taskOptionSome}"

            let! taskOptionNone = Foobar.ReturnsTaskOptionNone()

            do printfn $"taskOptionNone: {taskOptionNone}"

            let! optionthing = Foobar.ReturnsOptionSome()

            do printfn $"optionthing: {optionthing}"

            do printfn $"Topmost CancellationToken.IsCancellationRequested: {ct.IsCancellationRequested}"

            // let! nonething = Foobar.ReturnsOptionNone() // cancels, since Cancelled task is returned
            // let! (cancelledTask: string) = Foobar.ReturnsCancelledTask()

            return "finished"
        }

    let result = CancellableTask.start cancellationToken computation
    //printfn $"Created CancellationToken: {cancellationToken.GetHashCode()}"

    let ret = result.Result
    //printfn $"Result: {ret}"

    0