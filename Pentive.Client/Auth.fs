module Pentive.Client.Auth

open System
open Pentive.Client
open Elmish
open Bolero
open Bolero.Html
open Bolero.Remoting

type Model =
    {
        Username: string option
    }

let initModel =
    {
        Username = None
    }

type AuthService =
    {
        /// Sign into the application.
        signIn : string * string -> Async<option<string>>

        /// Get the user's name
        getUsername : unit -> Async<string>

        /// Sign out from the application.
        signOut : unit -> Async<unit>
    }

    interface IRemoteService with
        member _.BasePath = "/auth"

type Redirect =
    | InitialLoad
    | Page of Page

type Trigger =
    | Manual
    | Auto

type Msg =
    | LoggedOut
    | LoginAttempted of username: string option * Trigger

let autoLoginAttempted username =
    LoginAttempted(username, Auto)

let manualLoginAttempted username =
    LoginAttempted(username, Manual)

type Cmd =
    | LoginSuccessful
    | Logout
    | FailLogin
    | AttemptLogin of username: string * password: string
    | Initialize

let logout model =
    { model with Username = None }

let update message model =
    match message with
    | LoggedOut                    -> logout model
    | LoginAttempted (username, _) -> { model with Username = username }

let generate message =
    match message with
    | LoggedOut               -> [Logout]
    | LoginAttempted (username, trigger) ->
        match username, trigger with
        | Some _, _       -> [LoginSuccessful]
        | None  , Manual  -> [FailLogin]
        | None  , Auto    -> []