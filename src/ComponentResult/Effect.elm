module ComponentResult.Effect exposing (..)


import ComponentResult
    exposing
        ( ComponentResult
        , escape
        , justError
        , mapModel
        , withCmds
        , withExternalMsg
        , withModel
        , resolve
        )


withEffect :
    effect -- effect to add to result
    -> ComponentResult model msg extMsg err -- current result
    -> ComponentResult ( model, effect ) msg extMsg err -- result with effect
withEffect eff =
    mapModel (\m -> ( m, eff ))


resolveAll :
    (effect -> Cmd msg)
    -> ComponentResult ( model, effect ) msg Never Never
    -> (model, Cmd msg)
resolveAll preformEffect =
    resolveEffects preformEffect >> resolve

resolveEffects :
    (effect -> Cmd msg) -- performEffect
    -> ComponentResult ( model, effect ) msg extMsg error -- result with effect
    -> ComponentResult model msg extMsg error -- normalized result
resolveEffects performEffect effectfulResult =
    effectfulResult
        |> escape
        |> (\r ->
                case r of
                    Result.Ok ( ( model, effs ), msg, mExtMsg ) ->
                        let
                            res =
                                withModel model
                                    |> withCmds
                                        [ performEffect effs
                                        , msg
                                        ]
                        in
                        Maybe.map
                            (\extMsg ->
                                res |> withExternalMsg extMsg
                            )
                            mExtMsg
                            |> Maybe.withDefault res

                    Result.Err e ->
                        justError e
           )