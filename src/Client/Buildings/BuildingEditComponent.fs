module Client.Buildings.BuildingEditComponent

open System
open Fable.React
open Fable.React.Props
open Elmish
open Elmish.React
open Shared.Read
open Shared.Library
open Shared.Write
open Shared.MediaLibrary
open Client.ClientStyle
open Client.ClientStyle.Helpers
open Client.Components
open Client.Components.BasicModal
open Client.Upload

type Message =
    | NameChanged of string
    | CodeChanged of string
    | AddressChanged of Address
    | OrganizationNumberChanged of string
    | GeneralMeetingPeriodChanged of (DateTime * DateTime) option
    | YearOfConstructionChanged of string
    | YearOfDeliveryChanged of string
    | ShowDefaultImage
    | OpenImageUploadDialog
    | CloseImageUploadDialog
    | TemporaryPictureIdSet of pictureId: Guid option
    | SaveImageUploadDialog of pictureId: Guid
    | BankAccountEditComponentMsg of BankAccountEditComponent.Msg

type State = {
    Building: Building
    GeneralMeetingPeriod: (DateTime * DateTime) option
    BankAccountsComponentState: BankAccountEditComponent.State
    ShowingDefaultImage: bool
    ShowingImageUploadDialog: bool
    TemporaryPictureId: Guid option
    Errors: (string * string) list
}

let init (building: Building) =
    let today = DateTime.Today
    let dateTime (month, day) =
        new DateTime(today.Year, month, day)

    let period =
        building.GeneralMeetingPeriod 
        |> Option.map (fun p -> Some (dateTime(p.FromMonth, p.FromDay), dateTime(p.UntilMonth, p.UntilDay)))
        |> Option.defaultValue None

    let componentState, componentCmd = 
        BankAccountEditComponent.init 
            {|
                BankAccounts = building.BankAccounts
                BasePath = nameof (building.BankAccounts)
            |}

    { 
        Building = building
        GeneralMeetingPeriod = period
        ShowingDefaultImage = false
        ShowingImageUploadDialog = false
        TemporaryPictureId = None
        BankAccountsComponentState = componentState
        Errors = []
    }, componentCmd |> Cmd.map BankAccountEditComponentMsg

let private formatOrganizationNumber (orgNr: string) =
    let digitString = orgNr |> String.filter Char.IsDigit
    match digitString.Length with
    | x when x > 7 -> sprintf "%s.%s.%s" digitString.[0..3] digitString.[4..6] digitString.[7..]
    | x when x > 4 -> sprintf "%s.%s" digitString.[0..3] digitString.[4..]
    | _ -> digitString

let update (message: Message) (state: State): State * Cmd<Message> =
    let changeBuilding f =
        { state with Building = f state.Building }

    let parseInt str =
        match str |> String.toOption with
        | Some s ->
            let isParsed, parsed = s |> String.filter Char.IsDigit |> Int32.TryParse
            if isParsed then Some parsed else None
        | None ->
            None

    let recalculateValidationErrors (state: State) =
        match state.Errors with
        | [] -> state
        | _errors ->
            match ValidatedBuilding.Validate state.Building with
            | Ok _validated -> state
            | Error validationErrors -> { state with Errors = validationErrors }

    match message with
    | NameChanged x ->
        changeBuilding (fun building -> { building with Name = x }), Cmd.none
    | CodeChanged x ->
        changeBuilding (fun b -> { b with Code = x }), Cmd.none
    | AddressChanged addr ->
        changeBuilding (fun b -> { b with Address = addr }), Cmd.none
    | OrganizationNumberChanged x ->
        let organizationNumber =
            x
            |> String.toOption
            |> Option.map formatOrganizationNumber

        changeBuilding (fun b -> { b with OrganizationNumber = organizationNumber }), Cmd.none
    | GeneralMeetingPeriodChanged periodOption ->
        let generalMeetingPeriod =
            periodOption
            |> Option.map (fun (fromDate, untilDate) -> {
                FromDay = fromDate.Day
                FromMonth = fromDate.Month
                UntilDay = untilDate.Day
                UntilMonth = untilDate.Month
            })
        changeBuilding (fun building -> { building with GeneralMeetingPeriod = generalMeetingPeriod })
        |> (fun state -> { state with GeneralMeetingPeriod = periodOption }), Cmd.none
    | YearOfConstructionChanged s ->
        changeBuilding (fun b -> { b with YearOfConstruction = parseInt s }), Cmd.none
    | YearOfDeliveryChanged s ->
        changeBuilding (fun b -> { b with YearOfDelivery = parseInt s }), Cmd.none
    | ShowDefaultImage ->
        { state with ShowingDefaultImage = true }, Cmd.none
    | OpenImageUploadDialog ->
        { state with ShowingImageUploadDialog = true }, Cmd.none
    | CloseImageUploadDialog ->
        { state with ShowingImageUploadDialog = false; ShowingDefaultImage = false; TemporaryPictureId = None }, Cmd.none
    | TemporaryPictureIdSet temporaryId ->
        { state with TemporaryPictureId = temporaryId }, Cmd.none
    | SaveImageUploadDialog pictureId ->
        changeBuilding (fun b -> { b with PictureId = Some pictureId })
        |> (fun state -> { state with ShowingImageUploadDialog = false; ShowingDefaultImage = false; TemporaryPictureId = None })
        , Cmd.none
    | BankAccountEditComponentMsg msg ->
        let componentState, componentCmd =
            BankAccountEditComponent.update msg state.BankAccountsComponentState
        { state with 
            BankAccountsComponentState = componentState
            Building = { state.Building with BankAccounts = componentState.BankAccounts }
        }, componentCmd |> Cmd.map BankAccountEditComponentMsg

    |> (fun (state, cmd) -> state |> recalculateValidationErrors, cmd)

let inColumn x = div [ Class Bootstrap.col ] [ x ]
let inColumn' columnClass x = div [ Class columnClass ] [ x ]

let private renderBankAccounts (state: State) (dispatch: Message -> unit) =
    fieldset [] [
        legend [] [ str "Bankrekeningen" ]
        div [] [
            BankAccountEditComponent.view (state.Errors) (state.BankAccountsComponentState) (BankAccountEditComponentMsg >> dispatch)
        ]
    ]

let private renderImageUploadDialog (state: State) (dispatch: Message -> unit) =
    BasicModal.render 
        {|
            ModalProps = [
                ModalProp.IsOpen state.ShowingImageUploadDialog
                ModalProp.OnDismiss (fun _ -> dispatch CloseImageUploadDialog)
                ModalProp.Header [
                    HeaderProp.HasDismissButton true
                    HeaderProp.Title "Foto van het gebouw veranderen"
                ]
                ModalProp.Body [
                    let parseGuid (guidString: string) =
                        match Guid.TryParse(guidString) with
                        | true, someIdentifier -> Some someIdentifier
                        | false, _ -> None
                    filePond
                        {|
                            BuildingId = Some state.Building.BuildingId
                            EntityId = state.Building.BuildingId
                            Partition = Partitions.BuildingImages
                            Options = [
                                FilePondOptions.MaxFiles 1
                                FilePondOptions.AllowMultiple false
                                FilePondOptions.AcceptedFileTypes [| "image/*" |]
                                FilePondOptions.OnProcessFile (fun error file -> 
                                    if String.IsNullOrWhiteSpace(error) then
                                        TemporaryPictureIdSet (parseGuid (file.serverId)) |> dispatch)
                                FilePondOptions.OnRemoveFile (fun error file -> 
                                    if String.IsNullOrWhiteSpace(error) then
                                        TemporaryPictureIdSet None |> dispatch)
                            ]
                        |}
                ]
                ModalProp.Footer [
                    FooterProp.Buttons [
                        button
                            [
                                classes [ Bootstrap.btn; Bootstrap.btnPrimary ]
                                OnClick (fun _ -> 
                                    match state.TemporaryPictureId with
                                    | Some pictureId -> SaveImageUploadDialog pictureId |> dispatch
                                    | None -> ())
                                HTMLAttr.Disabled (state.TemporaryPictureId.IsNone)
                            ]
                            [ str "Ok" ]
                    ]
                    FooterProp.ShowDismissButton (Some "Annuleren")
                ]
            ]
        |}

let view (state: State) (dispatch: Message -> unit) =
    let errorFor (path: string) =
        state.Errors |> List.tryPick (fun (p, error) -> if p = path then Some error else None)

    div [] [        
        fieldset [ classes [ Bootstrap.row; Bootstrap.dFlex; Bootstrap.flexNowrap ] ] [
            legend [] [ str "Algemeen" ]
            div [] [
                match state.Building.PictureId with
                | Some pictureId when not (state.ShowingDefaultImage) ->
                    img [
                        Src (Client.Upload.downloadUri Partitions.BuildingImages pictureId) 
                        Alt "Building image"
                        Style [ Height "250px" ]
                        Class "pointer"
                        OnError (fun _ -> dispatch ShowDefaultImage)
                        OnClick (fun _ -> dispatch OpenImageUploadDialog)
                    ]
                | _ ->
                    img [
                        Src "https://i.ibb.co/rQnJ0hn/architecture-768432-640.jpg"
                        Alt "Building image"
                        Class "pointer"
                        Style [ Height "240px" ]
                        OnClick (fun _ -> dispatch OpenImageUploadDialog)
                    ]
            ]
            div [ Class Bootstrap.flexGrow1 ] [
                div [ Class Bootstrap.row ] [
                    formGroup [ 
                        Label "Naam"
                        Input [ 
                            Type "text"
                            MaxLength 255.0
                            Helpers.valueOrDefault state.Building.Name
                            OnChange (fun e -> NameChanged e.Value |> dispatch)
                        ] 
                        FormError (errorFor (nameof state.Building.Name))
                    ]
                    |> inColumn
                    formGroup [ 
                        Label "Code"
                        Input [ 
                            Type "text"
                            MaxLength 16.0
                            Helpers.valueOrDefault state.Building.Code
                            OnChange (fun e -> CodeChanged e.Value |> dispatch)
                        ] 
                        FormError (errorFor (nameof state.Building.Code))
                    ]
                    |> inColumn
                    formGroup [
                        Label "Ondernemingsnr."
                        Input [ 
                            Type "text"
                            Pattern "[0-9]{4}\.[0-9]{3}\.[0-9]{3}"
                            MaxLength 12.0
                            Placeholder "xxxx.xxx.xxx"
                            Style [ Width "120px" ]
                            Helpers.valueOrDefault state.Building.OrganizationNumber
                            OnChange (fun e -> OrganizationNumberChanged e.Value |> dispatch)
                        ] 
                        FormError (errorFor (nameof state.Building.OrganizationNumber))
                    ]
                ]
                AddressEditComponent.render 
                    state.Building.Address 
                    (Some (AddressChanged >> dispatch))
                    (nameof state.Building.Address)
                    state.Errors
                |> inColumn
            ]
        ]
        div [ Class Bootstrap.row ] [
            formGroup [
                Label "Bouwjaar"
                Input [
                    Type "number"
                    Helpers.valueOrDefault state.Building.YearOfConstruction
                    OnChange (fun e -> YearOfConstructionChanged e.Value |> dispatch)
                    Style [ Width "120px" ]
                ]
                FormError (errorFor (nameof state.Building.YearOfConstruction))
            ]

            div [ Style [ MarginLeft "15px" ] ] [
                formGroup [
                    Label "Opleveringsjaar"
                    Input [
                        Type "number"
                        Helpers.valueOrDefault state.Building.YearOfDelivery
                        OnChange (fun e -> YearOfDeliveryChanged e.Value |> dispatch)
                        Style [ Width "120px" ]
                    ]
                    FormError (errorFor (nameof state.Building.YearOfDelivery))
                ]
            ]
        ]

        div [ Class Bootstrap.row ] [
            formGroup [
                Label "Periode algemene vergadering"
                Date [
                    match state.GeneralMeetingPeriod with
                    | Some (start, until) -> 
                        yield Flatpickr.Values [| start; until |]
                    | _ -> 
                        ()
                    yield Flatpickr.Style [ Width "320px" ]
                    yield Flatpickr.SelectionMode Flatpickr.Mode.Range
                    yield Flatpickr.DateFormat "j F"
                    yield
                        Flatpickr.OnManyChanged (fun (dates: list<DateTime>) ->                                    
                            match dates with
                            | [ fromDate; toDate ] -> GeneralMeetingPeriodChanged (Some (fromDate, toDate)) |> dispatch
                            | _ -> GeneralMeetingPeriodChanged None |> dispatch)
                ]
            ]
        ]

        renderBankAccounts state dispatch
        renderImageUploadDialog state dispatch
    ]