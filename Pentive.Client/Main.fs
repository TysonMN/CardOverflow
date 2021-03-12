module Pentive.Client.Main

open System
open Elmish
open Bolero
open Bolero.Html
open Bolero.Remoting
open Bolero.Remoting.Client
open Bolero.Templating.Client

/// Routing endpoints definition.
type Page =
    | [<EndPoint "/">] Home
    | [<EndPoint "/counter">] Counter
    | [<EndPoint "/data">] Data
    | [<EndPoint "/login">] Login

/// The Elmish application's model.
type Model =
    {
        page: Page
        counter: Counter.Model
        books: Book[] option
        error: string option
        username: string
        password: string
        signedInAs: option<string>
        signInFailed: bool
    }

and Book =
    {
        title: string
        author: string
        publishDate: DateTime
        isbn: string
    }

let initModel =
    {
        page = Home
        counter = Counter.initModel
        books = None
        error = None
        username = ""
        password = ""
        signedInAs = None
        signInFailed = false
    }

/// Remote service definition.
type BookService =
    {
        /// Get the list of all books in the collection.
        getBooks: unit -> Async<Book[]>

        /// Add a book in the collection.
        addBook: Book -> Async<unit>

        /// Remove a book from the collection, identified by its ISBN.
        removeBookByIsbn: string -> Async<unit>

        /// Sign into the application.
        signIn : string * string -> Async<option<string>>

        /// Get the user's name, or None if they are not authenticated.
        getUsername : unit -> Async<string>

        /// Sign out from the application.
        signOut : unit -> Async<unit>
    }

    interface IRemoteService with
        member this.BasePath = "/books"

/// The Elmish application's update messages.
type Message =
    | SetPage of Page
    | CounterMsg of Counter.Message
    | GetBooks
    | GotBooks of Book[]
    | SetUsername of string
    | SetPassword of string
    | GetSignedInAs
    | RecvSignedInAs of option<string>
    | SendSignIn
    | RecvSignIn of option<string>
    | SendSignOut
    | RecvSignOut
    | Error of exn
    | ClearError

type CmdMsg =
    | CM_SetPage of Page
    | CM_GetBooks
    | CM_GotBooks
    | CM_RecvSignIn of Model
    | CM_RecvSignedInAs
    | CM_RecvSignOut

let update message (model: Model) =
    let onSignIn = function
        | Some _ -> [CM_GetBooks]
        | None -> []
    match message with
    | SetPage page ->
        match page with
        | Data ->
            match model.signedInAs with
            | Some _ -> { model with page = page }, []
            | None ->  { model with error = Some "You must login to view the Download Data page." }, [CM_SetPage Login]
        | _ -> { model with page = page }, []

    | CounterMsg msg ->
        let counter = Counter.update msg model.counter
        { model with counter = counter }, []

    | GetBooks ->
        { model with books = None }, [CM_GotBooks]
    | GotBooks books ->
        { model with books = Some books }, []

    | SetUsername s ->
        { model with username = s }, []
    | SetPassword s ->
        { model with password = s }, []
    | GetSignedInAs ->
        model, [CM_RecvSignedInAs]
    | RecvSignedInAs username ->
        { model with signedInAs = username }, onSignIn username
    | SendSignIn ->
        model, [CM_RecvSignIn model]
    | RecvSignIn username ->
        { model with signedInAs = username; signInFailed = Option.isNone username }, onSignIn username
    | SendSignOut ->
        model, [CM_RecvSignOut]
    | RecvSignOut ->
        { model with signedInAs = None; signInFailed = false }, []

    | Error RemoteUnauthorizedException ->
        { model with error = Some "You have been logged out."; signedInAs = None }, []
    | Error exn ->
        { model with error = Some exn.Message }, []
    | ClearError ->
        { model with error = None }, []

/// Connects the routing system to the Elmish application.
let router = Router.infer SetPage (fun model -> model.page)

type Main = Template<"wwwroot/main.html">

let homePage =
    Main.Home().Elt()

let dataPage (model: Model) dispatch =
    match model.signedInAs with
    | Some username ->
        Main.Data()
            .Reload(fun _ -> dispatch GetBooks)
            .Username(username)
            .SignOut(fun _ -> dispatch SendSignOut)
            .Rows(cond model.books <| function
                | None ->
                    Main.EmptyData().Elt()
                | Some books ->
                    forEach books <| fun book ->
                        tr [] [
                            td [] [text book.title]
                            td [] [text book.author]
                            td [] [text (book.publishDate.ToString("yyyy-MM-dd"))]
                            td [] [text book.isbn]
                        ])
            .Elt()
    | None -> text "You must login to view the Download Data page."

let loginPage model dispatch =
    Main.SignIn()
        .Username(model.username, fun s -> dispatch (SetUsername s))
        .Password(model.password, fun s -> dispatch (SetPassword s))
        .SignIn(fun _ -> dispatch SendSignIn)
        .ErrorNotification(
            cond model.signInFailed <| function
            | false -> empty
            | true ->
                Main.ErrorNotification()
                    .HideClass("is-hidden")
                    .Text("Sign in failed. Use any username and the password \"password\".")
                    .Elt()
        )
        .Elt()

let menuItem (model: Model) (page: Page) (text: string) =
    Main.MenuItem()
        .Active(if model.page = page then "is-active" else "")
        .Url(router.Link page)
        .Text(text)
        .Elt()

let view model dispatch =
    Main()
        .Menu(concat [
            menuItem model Home "Home"
            menuItem model Login "Login"
            menuItem model Counter "Counter"
            menuItem model Data "Download data"
        ])
        .Body(
            cond model.page <| function
            | Home -> homePage
            | Login -> loginPage model dispatch
            | Counter -> Counter.view model.counter (CounterMsg >> dispatch)
            | Data -> dataPage model dispatch
        )
        .Error(
            cond model.error <| function
            | None -> empty
            | Some err ->
                Main.ErrorNotification()
                    .Text(err)
                    .Hide(fun _ -> dispatch ClearError)
                    .Elt()
        )
        .Elt()

let toCmd remote = function
    | CM_SetPage page -> SetPage page |> Cmd.ofMsg
    | CM_GetBooks
    | CM_GotBooks -> Cmd.OfAsync.either remote.getBooks () GotBooks Error
    | CM_RecvSignIn model -> Cmd.OfAsync.either remote.signIn (model.username, model.password) RecvSignIn Error
    | CM_RecvSignedInAs -> Cmd.OfAuthorized.either remote.getUsername () RecvSignedInAs Error
    | CM_RecvSignOut -> Cmd.OfAsync.either remote.signOut () (fun () -> RecvSignOut) Error

let toCmds remote =
    List.map (toCmd remote)

type MyApp() =
    inherit ProgramComponent<Model, Message>()

    override this.Program =
        let bookService = this.Remote<BookService>()
        let update msg model =
            let model, cmdMsgs = update msg model
            model, toCmds bookService cmdMsgs |> Cmd.batch
        Program.mkProgram (fun _ -> initModel, Cmd.ofMsg GetSignedInAs) update view
        |> Program.withRouter router
#if DEBUG
        |> Program.withHotReload
#endif
