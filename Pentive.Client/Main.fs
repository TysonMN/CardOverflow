module Pentive.Client.Main

open System
open Elmish
open Bolero
open Bolero.Html
open Bolero.Remoting
open Bolero.Remoting.Client
open Bolero.Templating.Client

/// The Elmish application's model.
type Model =
    {
        Page: Page
        Counter: Counter.Model
        Book: Book.Model
        Error: string option
        Login: Login.Model
        Auth: Auth.Model
    }

let initModel =
    {
        Page = Home
        Counter = Counter.initModel
        Book = Book.initModel
        Error = None
        Login = Login.initModel
        Auth = Auth.initModel
    }

/// The Elmish application's update messages.
type Message =
    | SetPage of Page
    | CounterMsg of Counter.Message
    | LoginMsg of Login.Message
    | BookMsg of Book.Message
    | AuthMsg of Auth.Message
    | Error of exn
    | ClearError

type CmdMsg =
    | CM_SetPage of Page
    | CM_Auth of Auth.CmdMsg
    | CM_Book of Book.CmdMsg

let isPermitted page (auth: Auth.Model) =
    if Page.requireAuthenticated page then
        auth.Username |> Option.isSome
    else true

let update message (model: Model) =
    match message with
    | SetPage page ->
        let model = // navigating from Login resets it
            match model.Page with
            | Login -> { model with Login = Login.initModel}
            | _ -> model
        if isPermitted page model.Auth then
            let initializeCmds =
                match page with
                | Book -> [CmdMsg.CM_Book Book.CM_Initialize]
                | _ -> []
            { model with Page = page }, initializeCmds
        else
            { model with Error = Some "You must login to view that page." }, [CM_SetPage Login]

    | CounterMsg msg ->
        let counter = Counter.update msg model.Counter
        { model with Counter = counter }, []
    | BookMsg msg ->
        let book, cmds = Book.update msg model.Book
        { model with Book = book }, cmds |> List.map CmdMsg.CM_Book
    | LoginMsg msg ->
        let login, cmds = Login.update msg model.Login
        { model with Login = login }, cmds |> List.map CmdMsg.CM_Auth
    | AuthMsg msg ->
        let auth, cmds = Auth.update msg model.Auth
        { model with Auth = auth }, cmds |> List.map CmdMsg.CM_Auth

    | Error RemoteUnauthorizedException ->
        { model with Error = Some "You have been logged out."; Auth = Auth.logout model.Auth }, []
    | Error exn ->
        { model with Error = Some exn.Message }, []
    | ClearError ->
        { model with Error = None }, []

/// Connects the routing system to the Elmish application.
let router = Router.infer SetPage (fun model -> model.Page)

type Main = Template<"wwwroot/main.html">

let homePage =
    Main.Home().Elt()

let menuItem (model: Model) (page: Page) (text: string) =
    Main.MenuItem()
        .Active(if model.Page = page then "is-active" else "")
        .Url(router.Link page)
        .Text(text)
        .Elt()

let view model dispatch =
    Main()
        .Menu(concat [
            menuItem model Home "Home"
            menuItem model Login "Login"
            menuItem model Profile "Profile"
            menuItem model Counter "Counter"
            menuItem model Book "Download Books"
        ])
        .Body(
            cond model.Page <| function
            | Home -> homePage
            | Login -> Login.view model.Login (LoginMsg >> dispatch)
            | Counter -> Counter.view model.Counter (CounterMsg >> dispatch)
            | Book -> Book.view model.Auth.Username model.Book (BookMsg >> dispatch)
            | Profile -> Profile.view model.Auth (AuthMsg >> dispatch)
        )
        .Error(
            cond model.Error <| function
            | None -> empty
            | Some err ->
                Main.ErrorNotification()
                    .Text(err)
                    .Hide(fun _ -> dispatch ClearError)
                    .Elt()
        )
        .Elt()

let toCmd (authRemote: Auth.AuthService) (bookRemote: Book.BookService) = function
    | CM_SetPage page -> SetPage page |> Cmd.ofMsg
    | CM_Book cmdMsg ->
        match cmdMsg with
        | Book.CM_GetBooks
        | Book.CM_Initialize ->
            Cmd.OfAsync.either bookRemote.getBooks ()
                (Book.GotBooks >> Message.BookMsg)
                (Book.GotBooksError >> Message.BookMsg)
    | CM_Auth cmdMsg ->
        match cmdMsg with
        | Auth.CM_AttemptLogin (username, password) -> Cmd.OfAsync.either authRemote.signIn (username, password) (Auth.loginAttemptedTo Page.Profile >> Message.AuthMsg) Error
        | Auth.CM_Logout -> Cmd.OfAsync.attempt authRemote.signOut () Error
        | Auth.CM_SetPage page -> SetPage page |> Cmd.ofMsg
        | Auth.CM_LoginFailed -> Login.Message.LoginFailed |> Message.LoginMsg |> Cmd.ofMsg
        | Auth.CM_Initialize -> Cmd.OfAuthorized.either authRemote.getUsername () (Auth.initialLoginAttempted >> Message.AuthMsg) Error

let toCmds auth book =
    List.map (toCmd auth book)

type MyApp() =
    inherit ProgramComponent<Model, Message>()

    override this.Program =
        let bookRemote = this.Remote<Book.BookService>()
        let authRemote = this.Remote<Auth.AuthService>()
        let update msg model =
            let model, cmdMsgs = update msg model
            model, toCmds authRemote bookRemote cmdMsgs |> Cmd.batch
        Program.mkProgram (fun _ -> initModel, Auth.CM_Initialize |> CmdMsg.CM_Auth |> toCmd authRemote bookRemote) update view
        |> Program.withRouter router
#if DEBUG
        |> Program.withHotReload
#endif
