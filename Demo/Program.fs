// Learn more about F# at http://fsharp.org



type Command = | Increment | Decrement

type Event =
    | Incremented of int 
    | Decremented of int

type Error = BadCommand | BadData

type Transformer <'TState, 'TCommand, 'TEvent, 'TError> = {
    ZeroState : 'TState
    Reducer : 'TState -> 'TEvent -> 'TState
    Producer : 'TState -> 'TCommand -> Result<'TEvent,'TError> }

let createHandler (transformer:Transformer <'TState, 'TCommand, 'TEvent, 'TError>,
                    load: unit -> Async<obj seq>,
                        commit : (int * int) -> obj -> Async<unit>) =
    fun (id, version) command -> async{
        let! events = load()
        let events = events |> Seq.cast :> 'TEvent seq
        let state = Seq.fold transformer.Reducer transformer.ZeroState events
        let event = transformer.Producer state command
        match event with 
        | Ok e -> do! commit (id, version) e
                  return event  
        | Error msg -> return msg |> failwith
        }

let tf: Transformer<int, Command, Event, Error> = {
        ZeroState = 0;

        Reducer = fun s e ->
                match e with
                | Incremented v -> v 
                | Decremented v -> v

        Producer = fun s c ->
                match c with
                | Increment -> Ok(Incremented (s + 1))
                | Decrement -> Ok (Decremented (s - 1))
        }

let mutable data = 0;   
             
let loader () = async {
    return seq { yield data }
}

let commit (id, version) = async {
    data <- id
}
        
let handler = createHandler (tf,loader, commit)

open System

[<EntryPoint>]
let main argv =

    loader() |> printfn "%A"
    0 // return an integer exit code
