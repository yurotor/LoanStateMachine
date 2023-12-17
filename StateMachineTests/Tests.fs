namespace StateMachineTests

module Tests =
    open Xunit
    open FsUnit.Xunit
    open SM
    open SM.Loans

    [<Fact>]
    let ``When seasoning ends a funded loan turns into ready for sale`` () =
        let machine = Loans.machine 
        let result = 
            machine
            |> process' FundingStarted
            |> process' SeasoningEnded
            |> getState

        result |> should equal ReadyForSale

    [<Fact>]
    let ``When purchased a ready for sale turns into sold`` () =
        let machine = Loans.machine 
        let result = 
            machine
            |> process' FundingStarted
            |> process' SeasoningEnded
            |> process' Purchased
            |> getState

        result |> should equal Sold
