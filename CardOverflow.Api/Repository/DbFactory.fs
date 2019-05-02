﻿namespace CardOverflow.Api

open CardOverflow.Entity
open CardOverflow.Entity.Anki
open Microsoft.EntityFrameworkCore
open Microsoft.EntityFrameworkCore.Diagnostics

type CreateCardOverflowDb = unit -> CardOverflowDb

type DbFactory(connectionString: string) =
    member __.Create() =
        DbContextOptionsBuilder()
            .UseSqlServer(connectionString)
            .ConfigureWarnings(fun warnings -> warnings.Throw(RelationalEventId.QueryClientEvaluationWarning) |> ignore)
            .Options
        |> fun o -> new CardOverflowDb(o)

type DbService(createCardOverflowDb: CreateCardOverflowDb) =
    member __.Query(q) =
        use db = createCardOverflowDb()
        q db
    member __.Command(q): unit =
        use db = createCardOverflowDb()
        q db |> ignore
        db.SaveChanges() |> ignore

type AnkiDbFactory(dbPath: string) =
    member __.Create() =
        DbContextOptionsBuilder()
            .UseSqlite("DataSource=" + dbPath)
            .ConfigureWarnings(fun warnings -> warnings.Throw(RelationalEventId.QueryClientEvaluationWarning) |> ignore)
            .Options
        |> fun o -> new AnkiDb(o)

type AnkiDbService(dbFactory: AnkiDbFactory) =
    member __.Query(q) =
        use db = dbFactory.Create()
        q db
    member __.Command(q): unit =
        use db = dbFactory.Create()
        q db |> ignore
        db.SaveChanges() |> ignore
