namespace SM

module StateMachine =
    
    type StateMachineContext<'State, 'Event when 'Event : comparison and 'State : comparison> = 
        {
            State: 'State
            Transitions: Map<'State, Map<'Event, 'State>>
        }
    and StateMachine<'State, 'Event when 'Event : comparison and 'State : comparison> = 
        StateMachine of stateMachine: StateMachineContext<'State, 'Event>

    type NotAssembledStateMachineContext<'State, 'Event when 'Event : comparison and 'State : comparison> = 
        {
            CurrentState: 'State option
            Transitions: Map<'State, Map<'Event, 'State>>
        }
    and NotAssembledMachine<'State, 'Event when 'Event : comparison and 'State : comparison> =
        NotAssembledMachine of stateMachine: NotAssembledStateMachineContext<'State, 'Event>
    and StateTransitions<'State, 'Event when 'Event : comparison and 'State : comparison> = {
        OnState: 'State
        Transitions: Map<'Event, 'State>
    }

    type StateTransitionsBuilder<'State, 'Event when 'Event : comparison and 'State : comparison>(fromState: 'State) = 
        member _.Yield _ = Map.empty
        [<CustomOperation("when'")>]
        member _.When' (ctx: Map<'Event, 'State>, command: 'Event, handleCommand: unit -> 'State): Map<'Event, 'State> =
            ctx |> Map.add command (handleCommand ())
        member _.Run(ctx: Map<'Event, 'State>): StateTransitions<'State, 'Event> = 
            { OnState = fromState; Transitions = ctx }

    let on<'State, 'Event when 'Event : comparison and 'State : comparison>(inititalState: 'State) = 
        StateTransitionsBuilder<'State, 'Event> inititalState

    let then' state _ = state

    type StateMachineStateTransitionsBuilder<'State, 'Event when 'Event : comparison and 'State : comparison>() =
        member _.Yield(()) = ()
        member _.Yield(stateTransitions: StateTransitions<'State, 'Event>) =
            Map.ofArray [| stateTransitions.OnState, stateTransitions.Transitions |]
    
        member _.Delay(f: unit -> Map<'State, Map<'Event, 'State>>) = f()
    
        member _.Combine(newTransitions: Map<'State, Map<'Event, 'State>>, previousTransitions: Map<'State, Map<'Event, 'State>>) =
            newTransitions |> Map.fold (fun acc key value -> Map.add key value acc) previousTransitions
    
        member this.For(transitions: Map<'State, Map<'Event, 'State>>, f: unit -> Map<'State, Map<'Event, 'State>>) =
            this.Combine(transitions, f())
    
        member _.Run(transitions: Map<'State, Map<'Event, 'State>>) = transitions 
    
    type StateMachineAssembler<'State, 'Event when 'Event : comparison and 'State : comparison>(name: string) =
        member _.Yield(_) = NotAssembledMachine { CurrentState = None; Transitions = Map.empty } 
    
        [<CustomOperation "initialState">]
        member  _.InitialState ((NotAssembledMachine machine): NotAssembledMachine<'State, 'Event>, state: 'State) : NotAssembledMachine<'State, 'Event> =
            NotAssembledMachine { machine with CurrentState = Some state } 
    
        member _.Run((NotAssembledMachine machine): NotAssembledMachine<'State, 'Event>) =
            match machine.CurrentState with
            | None -> failwith "You need to provide initial state for a state machine using `initialState` operator."
            | Some state -> StateMachine { State = state; Transitions = machine.Transitions }
    
        member _.Yield((transitions): Map<'State, Map<'Event, 'State>>) =
            NotAssembledMachine { CurrentState = None; Transitions = transitions }
        
        member _.Delay(f: unit -> NotAssembledMachine<'State, 'Event>) = f()
    
        member this.For((NotAssembledMachine machine): NotAssembledMachine<'State, 'Event>, f: unit -> NotAssembledMachine<'State, 'Event>) =
            this.Combine(NotAssembledMachine machine, f())
    
        member _.Combine((NotAssembledMachine newMachine): NotAssembledMachine<'State, 'Event>, (NotAssembledMachine existingMachine): NotAssembledMachine<'State, 'Event>) =
            let newTransitions =
                newMachine.Transitions
                |> Map.fold (fun acc key value -> Map.add key value acc) existingMachine.Transitions
            
            NotAssembledMachine { CurrentState =  newMachine.CurrentState; Transitions = newTransitions  }
    
    
    let states<'State, 'Event when 'Event : comparison and 'State : comparison> =
        StateMachineStateTransitionsBuilder<'State, 'Event>()

    let stateMachine<'State, 'Event when 'Event : comparison and 'State : comparison> (name: string)  =
        StateMachineAssembler<'State, 'Event>  name

    type Process<'State, 'Event when 'State : comparison and 'Event : comparison> =
        'Event -> StateMachine<'State, 'Event> -> StateMachine<'State, 'Event>