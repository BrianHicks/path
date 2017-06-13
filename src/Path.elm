module Path exposing (..)

import String


type Error
    = ParsingFailed -- TODO: parser error
    | BadPush
    | DescendsBelowRoot
    | Wasnt Kind


type Kind
    = Absolute
    | Relative


type Path
    = Valid Kind (List String)
    | Invalid Error


{-| naively parse a path
-}
parse : String -> Path
parse raw =
    -- TODO: make sure we don't have any empty elements
    resolveTraversal <|
        case String.split "/" raw of
            "" :: path ->
                Valid Absolute path

            path ->
                Valid Relative path


{-| TODO: resolve `..` and make sure traversal doesn't go above root. Also just
remove `.`s.
-}
resolveTraversal : Path -> Path
resolveTraversal path =
    let
        simplify : List String -> Result Error (List String)
        simplify parts =
            case parts of
                ".." :: _ ->
                    Err DescendsBelowRoot

                element :: ".." :: rest ->
                    simplify rest

                "." :: rest ->
                    simplify rest

                element :: rest ->
                    Result.map ((::) element) (simplify rest)

                [] ->
                    Ok []
    in
        case path of
            Valid Absolute parts ->
                case simplify parts of
                    Ok simplified ->
                        Valid Absolute simplified

                    Err err ->
                        Invalid err

            Valid Relative parts ->
                path

            Invalid _ ->
                path


dontCare : String -> Path
dontCare =
    parse


expect : Kind -> String -> Path
expect expected raw =
    case parse raw of
        Valid actual path ->
            if expected == actual then
                Valid actual path
            else
                Invalid <|
                    Wasnt
                        (if actual == Absolute then
                            Relative
                         else
                            Absolute
                        )

        Invalid reason ->
            Invalid reason


relative : String -> Path
relative =
    -- TODO: this may need to be resolved in the context of a path immediately.
    -- It'd make sense, but we also need to keep actual relative values somehow.
    -- I'm pretty sure it'd just be another function, but I'm not sure if that
    -- logic belongs here or elsewhere.
    expect Relative


absolute : String -> Path
absolute =
    expect Absolute


push : Path -> Path -> Path
push new base =
    case ( base, new ) of
        ( Valid Absolute baseElements, Valid Relative newElements ) ->
            Valid Absolute (baseElements ++ newElements) |> resolveTraversal

        ( Valid Relative baseElements, Valid Relative newElements ) ->
            Valid Absolute (baseElements ++ newElements) |> resolveTraversal

        ( Valid _ baseElements, _ ) ->
            -- we're not allowed to add in any other orders
            Invalid BadPush

        ( _, _ ) ->
            base


pop : Path -> Path
pop =
    push (relative "..")


(/+) : Path -> String -> Path
(/+) base new =
    base |> push (relative new)


(/-) : Path -> Int -> Path
(/-) base count =
    if count > 0 then
        (pop base) /- (count - 1)
    else
        base


root : Path
root =
    Valid Absolute []


toString : Path -> Result Error String
toString path =
    case path of
        Valid Relative elements ->
            Ok <| String.join "/" elements

        Valid Absolute elements ->
            Ok <| "/" ++ String.join "/" elements

        Invalid reason ->
            Err reason


example =
    let
        basePath =
            absolute "/path/to/my/elm/project"

        testPath =
            relative "tests"

        elmPackage =
            relative "elm-package.json"
    in
        basePath
            |> push testPath
            |> push elmPackage
