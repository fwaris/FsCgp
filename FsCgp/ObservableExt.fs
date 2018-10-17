namespace FsCgp
//combination of MailboxProcessor and Observable 
module Observable =
    open System

    let createObservableAgent<'T> (token:System.Threading.CancellationToken) backpressureAtDepth =
        let finished = ref false
        let subscribers = ref (Map.empty : Map<int, IObserver<'T>>)

        let inline publish msg = 
            !subscribers 
            |> Seq.iter (fun (KeyValue(_, sub)) ->
                try
                        sub.OnNext(msg)
                with ex -> 
                    System.Diagnostics.Debug.Write(ex))

        let completed() = 
            lock subscribers (fun () ->
            finished := true
            !subscribers |> Seq.iter (fun (KeyValue(_, sub)) -> sub.OnCompleted())
            subscribers := Map.empty)

        token.Register(fun () -> completed()) |> ignore //callback for when token is cancelled
            
        let count = ref 0
        let agent =
            MailboxProcessor.Start
                ((fun inbox ->
                    async {
                        while true do
                            let! msg = inbox.Receive()
                            publish msg} ),
                    token)
            
        let obs = 
            { new IObservable<'T> with 
                member this.Subscribe(obs) =
                    let key1 =
                        lock subscribers (fun () ->
                            if !finished then failwith "Observable has already completed"
                            let key1 = !count
                            count := !count + 1
                            subscribers := subscribers.Value.Add(key1, obs)
                            key1)
                    { new IDisposable with  
                        member this.Dispose() = 
                            lock subscribers (fun () -> 
                                subscribers := subscribers.Value.Remove(key1)) } }
        let fPost = 
            backpressureAtDepth 
            |> Option.map (fun depth ->
                fun item -> 
                    if agent.CurrentQueueLength > depth then
                        System.Threading.Thread.Yield() |> ignore
                    agent.Post item
                )
            |> Option.defaultValue agent.Post
        obs,fPost

    let together (obs1:IObservable<'a>) (obs2:IObservable<'b>) =
        let mutable state:('a option * 'b option) = (None,None)
        { new IObservable<'a option *'b option> with
            member x.Subscribe(observer) = //TODO: need to dispose both 
              obs1.Subscribe (fun a -> state <-  (Some a, snd state); observer.OnNext state) |> ignore
              obs2.Subscribe (fun b -> state <- (fst state, Some b); observer.OnNext state)
        }

    let separate (obs:IObservable<'a option*'b option>) =
        { new IObservable<'a> with
            member x.Subscribe(observer) =
              obs.Subscribe (function (Some a,_) ->  observer.OnNext a | _ -> () ) 
        }
        ,
        { new IObservable<'b> with
            member x.Subscribe(observer) =
              obs.Subscribe (function (_,Some b) ->  observer.OnNext b | _ -> () )
        }

    let withI (obs:IObservable<'a>) =
        let mutable state = 0
        { new IObservable<int *'a> with
            member x.Subscribe(observer) = //TODO: need to dispose both 
              obs.Subscribe(fun a -> state <- state + 1; observer.OnNext (state,a))
        }

    let min (obs:IObservable<'a>)=
        let mutable state = Unchecked.defaultof<'a>
        { new IObservable<'a> with
            member x.Subscribe(observer) = //TODO: need to dispose both 
              obs.Subscribe(fun a -> 
                if state = Unchecked.defaultof<'a> then
                  state <- a
                  observer.OnNext (state)
                elif a < state then
                  state <- a
                  observer.OnNext (state))
        }

    let max (obs:IObservable<'a>)=
        let mutable state = Unchecked.defaultof<'a>
        { new IObservable<'a> with
            member x.Subscribe(observer) = //TODO: need to dispose both 
              obs.Subscribe(fun a -> 
                if state = Unchecked.defaultof<'a> then
                  state <- a
                  observer.OnNext (state)
                elif a > state then
                  state <- a
                  observer.OnNext (state))
        }

