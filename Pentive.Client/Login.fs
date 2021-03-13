module Pentive.Client.Login

open System
open Pentive.Client
open Elmish
open Bolero
open Bolero.Html

type Model =
    {
        username: string
        password: string
        loginFailed: bool
    }

let initModel =
    {
        username = ""
        password = ""
        loginFailed = false
    }

type Message =
    | SetUsername of string
    | SetPassword of string
    | SendSignIn
    | LoginFailed

let update message model =
    match message with
    | SetUsername s ->
        { model with username = s }, []
    | SetPassword s ->
        { model with password = s }, []
    | SendSignIn ->
        { model with password = ""; loginFailed = false }, [Auth.CmdMsg.CM_AttemptLogin (model.username, model.password)]
    | LoginFailed ->
        { model with loginFailed = true }, []

type Login = Template<"wwwroot/login.html">
type Main  = Template<"wwwroot/main.html">

let view model dispatch =
    Login()
        .Username(model.username, fun s -> dispatch (SetUsername s))
        .Password(model.password, fun s -> dispatch (SetPassword s))
        .SignIn(fun _ -> dispatch SendSignIn)
        .ErrorNotification(
            cond model.loginFailed <| function
            | false -> empty
            | true ->
                Main.ErrorNotification()
                    .HideClass("is-hidden")
                    .Text("Sign in failed. Use any username and the password \"password\".")
                    .Elt()
        )
        .Elt()
