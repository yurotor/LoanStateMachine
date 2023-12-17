namespace SM

module Loans =
    open StateMachine
    type LoanState = Originated | Funded | ReadyForSale | Sold | OnHold
    type LoanEvent = FundingStarted | SeasoningEnded | Purchased | Canceled
    
    let machine: StateMachine<LoanState, LoanEvent> = stateMachine "loan lifecycle" {
        initialState Originated
        states {
            on Originated {
                when' FundingStarted (then' Funded)
                when' Canceled       (then' OnHold)
            }
            on Funded {
                when' SeasoningEnded (then' ReadyForSale)
                when' Canceled       (then' OnHold)
            } 
            on ReadyForSale {
                when' Purchased      (then' Sold)
                when' Canceled       (then' OnHold)
            }           
        }
    }

    let process': Process<LoanState, LoanEvent> =
        fun event (StateMachine fsm) ->
            let newState =
                match fsm.Transitions |> Map.tryFind fsm.State with
                | Some transitions ->
                    match transitions |> Map.tryFind event with
                    | Some transition -> transition
                    | None -> fsm.State
                | None -> fsm.State
    
            StateMachine { fsm with State = newState }
    
    let getState (StateMachine machine) = 
        machine.State