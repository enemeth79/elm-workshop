module Main exposing (main)

import Browser
import Debug
import Generated.Api as BE
import Html as H
import Html.Attributes as HA
import Html.Attributes.Aria as HAA
import Html.Events as HE
import Http
import RemoteData exposing (RemoteData)
import Session
import Utils


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type Msg
    = HandleLoginResp (Result Http.Error String)
    | SetLoginPlayerId String 
    | SetLoginPassword String
    | LoginSubmit
    | SetregisterPlayerId String 
    | SetregisterPassword String
    | SetregisterPasswordAgain String
    | RegisterSubmit
    | HandleRegisterResp (Result Http.Error String)


type alias Model =
    { backendOK : Bool
    , backendError : Maybe String
    , loginPlayerId : String
    , loginPassword : String
    , registerPlayerId : String
    , registerPassword : String
    , registerPasswordAgain : String
    , token : Maybe String
    -- , registerBackendOK : Bool
    , registerValidationIssues : List String
    -- , registerError : Maybe String
    }


init : flags -> ( Model, Cmd Msg )
init _ =
    ( { backendOK = True
      , backendError = Nothing
      , loginPlayerId = ""
      , loginPassword = ""
      , registerPlayerId = ""
      , registerPassword = ""
      , registerPasswordAgain = ""
      , token = Nothing
    --   , registerBackendOK = True
      , registerValidationIssues = []
    --   , registerError = Nothing
      }
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update action model =
    case action of
        HandleLoginResp (Ok r) ->
            ( { model | backendOK = True, backendError = Nothing }
            , Cmd.none )

        HandleLoginResp (Err err) ->
            ( { model | backendError = Just "Backend login failed", backendOK = False }
            , Cmd.none )

        SetLoginPlayerId playerId ->
            ( { model | loginPlayerId = playerId }
            , Cmd.none )
        
        SetLoginPassword pwd ->
            ( { model | loginPassword = pwd }
            , Cmd.none )
        
        LoginSubmit ->
            ( { model | backendError = Nothing, backendOK = True }
            , BE.postApiLogin (BE.DbPlayer model.loginPlayerId model.loginPassword) HandleLoginResp)

        SetregisterPlayerId playerId ->
            ( { model | registerPlayerId = playerId }
            , Cmd.none )
        
        SetregisterPassword pwd ->
            ( { model | registerPassword = pwd }
            , Cmd.none )
        
        SetregisterPasswordAgain pwd ->
            ( { model | registerPasswordAgain = pwd }
            , Cmd.none )
        
        RegisterSubmit ->
            case validateRegisterDbPlayer model of
                Ok dbPlayer ->
                    ( { model 
                        | registerValidationIssues = []
                        , token = Nothing 
                      }
                    , BE.postApiPlayers (BE.DbPlayer model.registerPlayerId model.registerPassword) HandleRegisterResp
                    )
                Err errlist ->
                    ( { model 
                        | registerValidationIssues =errlist
                        , token = Nothing 
                      }
                    , Cmd.none
                    )
            
        HandleRegisterResp (Ok r) ->
            ( { model 
                | registerValidationIssues = []
                , token = Nothing 
              }
              , Cmd.none 
            )

        HandleRegisterResp (Err err) ->
            ( { model 
                | registerValidationIssues = [ "Backend registration failed" ]
                , token = Nothing 
              }
              , Cmd.none 
            )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none

view : Model -> H.Html Msg
view model =
    H.div []
        [ H.div [ HA.class "login-box" ]
            [ H.h1 [] [ H.text "Login" ]
            , H.form [ HE.onSubmit LoginSubmit ]
                [ H.input
                    [ HA.placeholder "Player Id"
                    , HAA.ariaLabel "Player ID"
                    , HE.onInput SetLoginPlayerId
                    , HA.value model.loginPlayerId
                    ]
                    []
                , H.input
                    [ HA.placeholder "Password"
                    , HA.type_ "password"
                    , HAA.ariaLabel "Password"
                    , HE.onInput SetLoginPassword
                    , HA.value model.loginPassword
                    ]
                    []
                , H.h3 []
                    [ 
                    (if model.backendOK 
                    then
                        H.span [] [H.text "Login Worked. All good!"] 
                    else
                        H.span [HA.class "err"] [H.text "Login Failed. Check network tab."]
                    )
                    ]
                , H.button
                    [ HA.class "btn primary" ]
                    [ H.text "Login" ]
                ]
            ]
        , H.div [ HA.class "register-box" ]
            [ H.h1 [] [ H.text "Register" ]
            , H.form [ HE.onSubmit RegisterSubmit ]
                [ H.input
                    [ HA.placeholder "Player Id"
                    , HAA.ariaLabel "Player ID"
                    , HE.onInput SetregisterPlayerId
                    , HA.value model.registerPlayerId
                    ]
                    []
                -- , H.br [] []
                , H.input
                    [ HA.placeholder "Password"
                    , HA.type_ "password"
                    , HAA.ariaLabel "Password"
                    , HE.onInput SetregisterPassword
                    , HA.value model.registerPassword
                    ]
                    []
                , H.input
                    [ HA.placeholder "Password Again"
                    , HA.type_ "password"
                    , HAA.ariaLabel "Password Again"
                    , HE.onInput SetregisterPasswordAgain
                    , HA.value model.registerPasswordAgain
                    ]
                    []
                , H.h3 []
                    [ 
                    (if model.registerValidationIssues == [] 
                    then
                        H.span [] [H.text "Register Worked. All good!"] 
                    else
                        H.span [HA.class "err"] 
                        [ H.text "Register Failed. Check network tab."
                        , H.br [] []
                        , H.text 
                            ( String.concat (List.map (\x -> String.append x " ") model.registerValidationIssues ) )
                        ]
                    )
                    ]
                , H.button
                    [ HA.class "btn primary" ]
                    [ H.text "Register" ]
                ]
            ]
        ]



validateRegisterDbPlayer : Model -> Result.Result (List String) BE.DbPlayer
validateRegisterDbPlayer model =
    let
        playerIdError =
            if model.registerPlayerId == ""
            then 
                [ "PlayerId cannon be empty." ]
            else 
                []
        passwordError =
            if model.registerPassword == ""
            then 
                [ "Password cannon be empty." ]
            else 
                []
        passwordAgainError =
            if model.registerPasswordAgain == ""
            then 
                [ "Password cannon be empty." ]
            else 
                []
        passwordMatchingError =
            if model.registerPassword == model.registerPasswordAgain
            then 
                []
            else 
                [ "Passwords do not match." ]
        
        errors = List.concat [ playerIdError, passwordError, passwordAgainError, passwordMatchingError ]
    in
        if errors == []
        then
            Ok { dbPlayerId = model.registerPlayerId, dbPlayerPassword = model.registerPassword }
        else 
            Err errors    