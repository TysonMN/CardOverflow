namespace CardOverflow.Test

open CardOverflow.Api
open System
open System.Runtime.CompilerServices
open System.Text.RegularExpressions
open Xunit
open CardOverflow.Entity
open Microsoft.EntityFrameworkCore
open Microsoft.EntityFrameworkCore.Diagnostics

type TestConnectionStringProvider(dbName: string) =
    interface IConnectionStringProvider with
        member __.Get = sprintf "Server=localhost;Database=CardOverflow_%s;Trusted_Connection=True;" dbName

type TestDbFactory(connectionStringProvider: IConnectionStringProvider) =
    interface IDbFactory with
        member __.Create() =
            DbContextOptionsBuilder()
                .UseSqlServer(connectionStringProvider.Get)
                .ConfigureWarnings(fun warnings -> warnings.Throw(RelationalEventId.QueryClientEvaluationWarning) |> ignore)
                .Options
            |> fun o -> new CardOverflowDb(o)

type TempDbService( [<CallerMemberName>] ?memberName: string) =
    let dbFactory =
        match memberName with
        | Some testName -> Regex.Replace(testName, "[^A-Za-z0-9 _]", "").Replace(' ', '_') |> TestConnectionStringProvider |> TestDbFactory :> IDbFactory
        | _ -> failwith "Missing the caller's member name somehow."
    do 
        dbFactory |> InitializeDatabase.deleteAndRecreateDatabase()

    interface IDisposable with
        member __.Dispose() =
            use db = dbFactory.Create()
            db.Database.EnsureDeleted() |> ignore

    member __.DbService =
        dbFactory |> DbService
