module Client.Financial.Deposits.DepositTypes

open Shared.Read

type CreateOrUpdate =
    | Create
    | Update of Deposit

type CreatedOrUpdated =
    | Created of Deposit
    | Updated of Deposit