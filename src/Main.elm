module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import String
import Maybe
import Char
import Time exposing (Posix, Month(..), toYear, toMonth, toDay, toHour, toMinute, utc, now, posixToMillis)
import Task
import Http
import Json.Encode as Encode

apiUrl : String
apiUrl = "http://localhost:3000/api/registrations"

type alias Registration =
    { id : Int
    , name : String
    , nickname : String
    , phone : String
    , city : String
    , vehicleNumber : String
    , registrationTime : String
    , departureTime : Maybe String
    , isActive : Bool
    }

type alias Model =
    { registrations : List Registration
    , showForm : Bool
    , formData : FormData
    , filterActive : Bool
    , nextId : Int
    , currentTime : Posix
    , error : Maybe String
    , loading : Bool
    }

type alias FormData =
    { name : String
    , nickname : String
    , phone : String
    , city : String
    , vehicleNumber : String
    }

type Msg
    = ToggleFilter
    | ShowAddForm
    | HideForm
    | UpdateName String
    | UpdateNickname String
    | UpdatePhone String
    | UpdateCity String
    | UpdateVehicleNumber String
    | SaveForm
    | MarkDeparture Int
    | DepartureMarked Int Posix
    | SetCurrentTime Posix
    | RemoveFilters
    | DataSaved (Result Http.Error String)
    | DataSyncError String

init : () -> ( Model, Cmd Msg )
init _ =
    ( { registrations = []
      , showForm = False
      , formData = { name = "", nickname = "", phone = "", city = "", vehicleNumber = "" }
      , filterActive = True
      , nextId = 1
      , currentTime = Time.millisToPosix 0
      , error = Nothing
      , loading = False
      }
    , Task.perform SetCurrentTime Time.now
    )

sendRegistrationsToServer : List Registration -> Cmd Msg
sendRegistrationsToServer registrations =
    Http.post
        { url = apiUrl
        , body = Http.jsonBody (encodeRegistrations registrations)
        , expect = Http.expectString DataSaved
        }

encodeRegistrations : List Registration -> Encode.Value
encodeRegistrations registrations =
    Encode.list encodeRegistration registrations

encodeRegistration : Registration -> Encode.Value
encodeRegistration reg =
    Encode.object
        [ ("id", Encode.int reg.id)
        , ("name", Encode.string reg.name)
        , ("nickname", Encode.string reg.nickname)
        , ("phone", Encode.string reg.phone)
        , ("city", Encode.string reg.city)
        , ("vehicleNumber", Encode.string reg.vehicleNumber)
        , ("registrationTime", Encode.string reg.registrationTime)
        , ("departureTime",
            case reg.departureTime of
                Just time -> Encode.string time
                Nothing -> Encode.null
          )
        , ("isActive", Encode.bool reg.isActive)
        ]

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ToggleFilter ->
            ( { model | filterActive = not model.filterActive }
            , Cmd.none
            )

        ShowAddForm ->
            ( { model | showForm = True, error = Nothing }
            , Cmd.none
            )

        HideForm ->
            ( { model | showForm = False
                      , formData = { name = "", nickname = "", phone = "", city = "", vehicleNumber = "" }
                      , error = Nothing
              }
            , Cmd.none
            )

        UpdateName name ->
            let
                formData = model.formData
                newFormData = { formData | name = name }
            in
            ( { model | formData = newFormData, error = Nothing }, Cmd.none )

        UpdateNickname nick ->
            let
                formData = model.formData
                newFormData = { formData | nickname = nick }
            in
            ( { model | formData = newFormData, error = Nothing }, Cmd.none )

        UpdatePhone phone ->
            let
                -- Фильтруем только цифры при вводе
                digitsOnly = String.filter Char.isDigit phone
                formData = model.formData
                newFormData = { formData | phone = digitsOnly }
            in
            ( { model | formData = newFormData, error = Nothing }, Cmd.none )

        UpdateCity city ->
            let
                formData = model.formData
                newFormData = { formData | city = city }
            in
            ( { model | formData = newFormData, error = Nothing }, Cmd.none )

        UpdateVehicleNumber vnum ->
            let
                formData = model.formData
                newFormData = { formData | vehicleNumber = vnum }
            in
            ( { model | formData = newFormData, error = Nothing }, Cmd.none )

        SaveForm ->
            let
                data = model.formData

                -- Проверка российского номера телефона
                phoneOk = isValidRussianPhone data.phone
                nameOk = not (String.trim data.name == "")
                cityOk = not (String.trim data.city == "")

                formattedPhone = formatPhone data.phone
            in
            if nameOk && cityOk && phoneOk then
                let
                    currentTime = formatDateTime model.currentTime

                    newRegistration =
                        { id = model.nextId
                        , name = data.name
                        , nickname = data.nickname
                        , phone = formattedPhone
                        , city = data.city
                        , vehicleNumber = data.vehicleNumber
                        , registrationTime = currentTime
                        , departureTime = Nothing
                        , isActive = True
                        }

                    newRegistrations = newRegistration :: model.registrations

                    newModel =
                        { model
                            | registrations = newRegistrations
                            , showForm = False
                            , formData = { name = "", nickname = "", phone = "", city = "", vehicleNumber = "" }
                            , nextId = model.nextId + 1
                            , loading = True
                            , error = Nothing
                        }
                in
                ( newModel, sendRegistrationsToServer newRegistrations )

            else
                ( { model | error = Just "" }, Cmd.none )

        MarkDeparture regId ->
            ( { model | loading = True, error = Nothing }, Task.perform (DepartureMarked regId) Time.now )

        DepartureMarked regId time ->
            let
                updateReg reg =
                    if reg.id == regId then
                        { reg | isActive = False
                                , departureTime = Just (formatDateTime time)
                        }
                    else
                        reg

                updatedRegs = List.map updateReg model.registrations
            in
            ( { model | registrations = updatedRegs, loading = True }
            , sendRegistrationsToServer updatedRegs
            )

        SetCurrentTime time ->
            ( { model | currentTime = time }, Cmd.none )

        RemoveFilters ->
            ( { model | filterActive = False }, Cmd.none )

        DataSaved result ->
            case result of
                Ok _ ->
                    ( { model | loading = False, error = Nothing }, Cmd.none )

                Err error ->
                    ( { model | loading = False, error = Just (parseHttpError error) }, Cmd.none )

        DataSyncError errorMessage ->
            ( { model | error = Just errorMessage, loading = False }, Cmd.none )

parseHttpError : Http.Error -> String
parseHttpError error =
    case error of
        Http.BadUrl url ->
            "Неверный URL: " ++ url
        Http.Timeout ->
            "Таймаут запроса"
        Http.NetworkError ->
            "Сетевая ошибка"
        Http.BadStatus statusCode ->
            "Ошибка сервера: " ++ String.fromInt statusCode
        Http.BadBody body ->
            "Ошибка в данных: " ++ body

-- Проверка российского номера телефона
isValidRussianPhone : String -> Bool
isValidRussianPhone phone =
    let
        digits = String.filter Char.isDigit phone
        length = String.length digits
    in
    -- Российские номера: 10 цифр (без кода страны) или 11 цифр (с кодом страны 7)
    (length == 10 && String.startsWith "9" digits) ||
    (length == 11 && String.startsWith "79" digits)

formatPhone : String -> String
formatPhone raw =
    let
        digits = String.filter Char.isDigit raw
        length = String.length digits

        formatRussianPhone ds =
            if String.length ds == 10 then
                -- Номер без кода страны: 9123456789 -> +7 912 345-67-89
                "+7 " ++ String.slice 0 3 ds ++ " " ++ String.slice 3 6 ds ++ "-" ++ String.slice 6 8 ds ++ "-" ++ String.slice 8 10 ds
            else if String.length ds == 11 && String.startsWith "7" ds then
                -- Номер с кодом страны: 79123456789 -> +7 912 345-67-89
                "+7 " ++ String.slice 1 4 ds ++ " " ++ String.slice 4 7 ds ++ "-" ++ String.slice 7 9 ds ++ "-" ++ String.slice 9 11 ds
            else
                -- Для других форматов просто добавляем +
                "+" ++ ds
    in
    formatRussianPhone digits

formatDateTime : Posix -> String
formatDateTime time =
    let
        year = String.fromInt (Time.toYear utc time)
        month = String.fromInt (monthToInt (Time.toMonth utc time))
        day = String.fromInt (Time.toDay utc time)
        hour = String.fromInt (Time.toHour utc time)
        minute = String.fromInt (Time.toMinute utc time)

        pad n = if String.length n == 1 then "0" ++ n else n
    in
    year ++ "-" ++ pad month ++ "-" ++ pad day ++ " " ++ pad hour ++ ":" ++ pad minute

monthToInt : Time.Month -> Int
monthToInt month =
    case month of
        Jan -> 1
        Feb -> 2
        Mar -> 3
        Apr -> 4
        May -> 5
        Jun -> 6
        Jul -> 7
        Aug -> 8
        Sep -> 9
        Oct -> 10
        Nov -> 11
        Dec -> 12

view : Model -> Html Msg
view model =
    div [ class "container" ]
        [ h1 [] [ text "Лиза Алерт регистрация" ]
        , if model.loading then
            div [ class "loading" ] [ text "Синхронизация с сервером..." ]
          else
            text ""

        , case model.error of
            Just errorMessage ->
                div [ class "error" ] [ text ("Ошибка: " ++ errorMessage) ]
            Nothing ->
                text ""

        , button [ onClick ShowAddForm, class "add-button", disabled model.loading ]
            [ text (if model.loading then "Загрузка..." else "Добавить поисковика") ]

        , label [ class "filter-toggle" ]
            [ input
                [ type_ "checkbox"
                , checked model.filterActive
                , onCheck (\_ -> ToggleFilter)
                , disabled model.loading
                ] []
            , text (if model.filterActive then " Показать только активных" else " Показать всех")
            ]

        , table [ class "registration-table" ]
            [ thead []
                [ tr []
                    [ th [] [ text "Статус" ]
                    , th [] [ text "Фамилия и имя" ]
                    , th [] [ text "Ник (с форума)" ]
                    , th [] [ text "Телефон" ]
                    , th [] [ text "Город" ]
                    , th [] [ text "N T/C" ]
                    , th [] [ text "Дата-время регистрации" ]
                    , th [] [ text "Дата-время отбытия" ]
                    ]
                ]
            , tbody [] (List.map (rowView model.filterActive model.loading) (filteredRegistrations model))
            ]
        , if model.showForm then
            formView model.formData model.loading
          else
            text ""
        ]

filteredRegistrations : Model -> List Registration
filteredRegistrations model =
    if model.filterActive then
        List.filter (\r -> r.isActive) model.registrations
    else
        model.registrations

rowView : Bool -> Bool -> Registration -> Html Msg
rowView filterOnly loading reg =
    if filterOnly && not reg.isActive then
        text ""
    else
        tr []
            [ td []
                [ text (if reg.isActive then "+" else "-") ]
            , td [] [ text reg.name ]
            , td [] [ text reg.nickname ]
            , td [] [ text reg.phone ]
            , td [] [ text reg.city ]
            , td [] [ text reg.vehicleNumber ]
            , td [] [ text reg.registrationTime ]
            , td []
                [ case reg.departureTime of
                    Just time -> text time
                    Nothing ->
                        button
                            [ onClick (MarkDeparture reg.id)
                            , disabled loading
                            ]
                            [ text (if loading then "..." else "Выбыл") ]
                ]
            ]

formView : FormData -> Bool -> Html Msg
formView formData loading =
    div [ class "form-overlay" ]
        [ div [ class "form-container" ]
            [ h2 [] [ text "Добавить поисковика" ]
            , div [ class "form-group" ]
                [ label [] [ text "Фамилия и имя *" ]
                , input
                    [ type_ "text"
                    , value formData.name
                    , onInput UpdateName
                    , required True
                    , disabled loading
                    ] []
                , span [ class "validation-error" ]
                    [ text (if String.trim formData.name == "" then "Поле не может быть пустым" else "") ]
                ]
            , div [ class "form-group" ]
                [ label [] [ text "Ник (с форума)" ]
                , input
                    [ type_ "text"
                    , value formData.nickname
                    , onInput UpdateNickname
                    , disabled loading
                    ] []
                ]
            , div [ class "form-group" ]
                [ label [] [ text "Телефон *" ]
                , input
                    [ type_ "tel"
                    , value formData.phone
                    , onInput UpdatePhone
                    , placeholder "79123456789 или 9123456789"
                    , required True
                    , disabled loading
                    , pattern "[0-9]*" -- Разрешаем ввод только цифр
                    , title "Введите только цифры (10 или 11 цифр)"
                    ] []
                , span [ class "validation-error" ]
                    [ text
                        (if not (isValidRussianPhone formData.phone) then
                            "Номер должен содержать 10 цифр (начинается с 9) или 11 цифр (начинается с 79)"
                         else
                            "")
                    ]
                ]
            , div [ class "form-group" ]
                [ label [] [ text "Город *" ]
                , input
                    [ type_ "text"
                    , value formData.city
                    , onInput UpdateCity
                    , required True
                    , disabled loading
                    ] []
                , span [ class "validation-error" ]
                    [ text (if String.trim formData.city == "" then "Поле не может быть пустым" else "") ]
                ]
            , div [ class "form-group" ]
                [ label [] [ text "N T/C" ]
                , input
                    [ type_ "text"
                    , value formData.vehicleNumber
                    , onInput UpdateVehicleNumber
                    , placeholder "Номер транспортного средства"
                    , disabled loading
                    ] []
                ]
            , div [ class "form-buttons" ]
                [ button
                    [ type_ "button"
                    , onClick SaveForm
                    , class "save-btn"
                    , disabled (loading || not (isValidRussianPhone formData.phone))
                    ]
                    [ text (if loading then "Сохранение..." else "Сохранить") ]
                , button
                    [ type_ "button"
                    , onClick HideForm
                    , class "cancel-btn"
                    , disabled loading
                    ]
                    [ text "Закрыть" ]
                ]
            ]
        ]

isValidPhone : String -> Bool
isValidPhone phone =
    isValidRussianPhone phone

