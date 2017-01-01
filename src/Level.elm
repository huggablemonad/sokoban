module Level
    exposing
        ( Model
        , init
        , update
        , view
        , currentLevel
        )

{-| Level-related functions.

# Definition
@docs Model

# Elm Architecture API
@docs init, update, view

# API
@docs currentLevel

-}

import Element exposing (Element)
import Html exposing (Html)
import Keyboard
import List.Extra as List


{-| (x, y) coordinates.
-}
type alias Pos =
    ( Int, Int )


{-| Level state.
-}
type alias Model =
    { walls : List Pos
    , endPoints : List Pos
    , boxes : List Pos
    , player : Pos
    , levelWidth : Int
    , levelHeight : Int
    , currentLevel : Int
    }


{-| Initialize the level state.
-}
init : Int -> Model
init level =
    parseLevel level


{-| Direction in which the player is moving.
-}
type Direction
    = Up
    | Down
    | Left
    | Right
    | Ignore


{-| Update the level state.
-}
update : Keyboard.KeyCode -> Model -> Model
update keyCode model =
    let
        dir =
            case keyCode of
                38 ->
                    Up

                40 ->
                    Down

                37 ->
                    Left

                39 ->
                    Right

                _ ->
                    Ignore
    in
        movePlayer model dir


{-| View the level state as HTML.
-}
view : Model -> Html msg
view model =
    [ updateLevel model, showVictoryScreen model ]
        |> Element.layers
        |> Element.toHtml


{-| Show a "Level Complete!" image if the level was completed.

The image was created with `ImageMagick`:

    convert -size 560x75 canvas:none -pointsize 72
        -draw 'text 25,60 "Level Complete!"'
        -channel RGBA -blur 0x6 -fill mediumseagreen -stroke darkgreen
        -draw 'text 20,55 "Level Complete!"' level-complete.png

Using `Text` causes a runtime error. Cause currently unknown.
-}
showVictoryScreen : Model -> Element
showVictoryScreen model =
    let
        victoryScreen =
            imgDir
                ++ "level-complete.png"
                |> Element.image 560 75
                |> createContainer model Element.middle

        isLevelComplete =
            List.sort (model.endPoints) == List.sort (model.boxes)
    in
        if isLevelComplete then
            victoryScreen
        else
            Element.empty


{-| Return the current level.
-}
currentLevel : Model -> Int
currentLevel model =
    model.currentLevel


{-| Move the player in the given direction.

If the player is next to a box and it can be pushed, then move the box as well.
-}
movePlayer : Model -> Direction -> Model
movePlayer model dir =
    let
        ( newX, newY ) =
            move model.player dir

        newModel =
            if isWall model ( newX, newY ) then
                model
            else if isBox model ( newX, newY ) then
                let
                    newBoxes =
                        moveBox model ( newX, newY ) dir
                in
                    if newBoxes.boxes == model.boxes then
                        model
                    else
                        { newBoxes | player = ( newX, newY ) }
            else
                { model | player = ( newX, newY ) }
    in
        newModel


{-| Move the box in the given direction.
-}
moveBox : Model -> Pos -> Direction -> Model
moveBox model ( x, y ) dir =
    let
        ( newX, newY ) =
            move ( x, y ) dir

        ( finalX, finalY ) =
            if
                isWall model ( newX, newY )
                    || isBox model ( newX, newY )
            then
                ( x, y )
            else
                ( newX, newY )

        newBoxes =
            model.boxes
                |> List.updateIf
                    (\pos -> pos == ( x, y ))
                    (\_ -> ( finalX, finalY ))
    in
        { model | boxes = newBoxes }


{-| Return `True` if there's a wall at the given `Pos`.
-}
isWall : Model -> Pos -> Bool
isWall model pos =
    model.walls
        |> List.member pos


{-| Return `True` if there's a box at the given `Pos`.
-}
isBox : Model -> Pos -> Bool
isBox model pos =
    model.boxes
        |> List.member pos


{-| Return the new `Pos` in the given direction.
-}
move : Pos -> Direction -> Pos
move ( x, y ) dir =
    case dir of
        Up ->
            ( x, y - 1 )

        Down ->
            ( x, y + 1 )

        Left ->
            ( x - 1, y )

        Right ->
            ( x + 1, y )

        Ignore ->
            ( x, y )


{-| Update the graphical representation of the level.
-}
updateLevel : Model -> Element
updateLevel model =
    [ layoutLevel model
    , placeBoxes model
    , placePlayer model
    ]
        |> Element.layers


{-| Place the player on the level.
-}
placePlayer : Model -> Element
placePlayer model =
    let
        ( x, y ) =
            model.player

        leftPos =
            x
                * tileWidth
                |> Element.absolute

        rightPos =
            y
                * tileHeight
                |> Element.absolute

        pos =
            Element.topLeftAt leftPos rightPos
    in
        createPlayer
            |> createContainer model pos


{-| Place the boxes on the level.
-}
placeBoxes : Model -> Element
placeBoxes model =
    List.map (placeBox model) model.boxes
        |> Element.layers


{-| Place a box on the level.

Helper function for [`placeBoxes`](#placeBoxes).
-}
placeBox : Model -> Pos -> Element
placeBox model ( x, y ) =
    let
        box =
            if List.member ( x, y ) model.endPoints then
                createBoxHome
            else
                createBox

        leftPos =
            x
                * tileWidth
                |> Element.absolute

        rightPos =
            y
                * tileHeight
                |> Element.absolute

        pos =
            Element.topLeftAt leftPos rightPos
    in
        createContainer model pos box


{-| Return the level's static components as a graphical representation.
-}
layoutLevel : Model -> Element
layoutLevel model =
    createLevel model
        |> List.map (Element.flow Element.right)
        |> Element.flow Element.down


{-| Return the level's static components as graphical elements.

Helper function for [`layoutLevel`](#layoutLevel).
-}
createLevel : Model -> List (List Element)
createLevel model =
    let
        f x =
            case x of
                '#' ->
                    createWall

                '.' ->
                    createEndPoint

                _ ->
                    createSpacer

        row xs =
            String.toList xs
                |> List.map f
    in
        load model.currentLevel
            |> Tuple.second
            |> List.map row


{-| Return the wall image.
-}
createWall : Element
createWall =
    imgDir
        ++ "wall.png"
        |> Element.image tileWidth tileHeight


{-| Return the end point image.
-}
createEndPoint : Element
createEndPoint =
    imgDir
        ++ "endpoint.png"
        |> Element.image 32 32
        |> Element.container tileWidth tileHeight Element.middle


{-| Return the box image.
-}
createBox : Element
createBox =
    imgDir
        ++ "box.png"
        |> Element.image tileWidth tileHeight


{-| Return the image of a box on an end point.
-}
createBoxHome : Element
createBoxHome =
    imgDir
        ++ "box-home.png"
        |> Element.image tileWidth tileHeight


{-| Return the player image.
-}
createPlayer : Element
createPlayer =
    imgDir
        ++ "player.png"
        |> Element.image 37 59
        |> Element.container tileWidth tileHeight Element.middle


{-| Return the empty tile image.
-}
createSpacer : Element
createSpacer =
    Element.spacer tileWidth tileHeight


{-| Return the level container with a positioned element.
-}
createContainer : Model -> Element.Position -> Element -> Element
createContainer model position element =
    let
        width =
            model.levelWidth * tileWidth

        height =
            model.levelHeight * tileHeight
    in
        Element.container width height position element


{-| Return the image directory.

    imgDir == "img/"
-}
imgDir : String
imgDir =
    "img/"


{-| Load and parse the given level.
-}
parseLevel : Int -> Model
parseLevel level =
    let
        ( levelNum, levelMap ) =
            load level

        emptyModel =
            Model [] [] [] ( 0, 0 ) 0 0 0

        model =
            levelMap
                |> List.foldl parseRow ( ( 0, 0 ), emptyModel )
                |> Tuple.second

        levelWidth =
            List.head levelMap
                |> Maybe.withDefault ""
                |> String.length

        levelHeight =
            List.length levelMap
    in
        { model
            | levelWidth = levelWidth
            , levelHeight = levelHeight
            , currentLevel = levelNum
        }


{-| Parse a row of level data.

Helper function for [`parseLevel`](#parseLevel).
-}
parseRow : String -> ( Pos, Model ) -> ( Pos, Model )
parseRow row ( ( x, y ), model ) =
    let
        newModel =
            List.foldl parseElement ( ( x, y ), model ) (String.toList row)
                |> Tuple.second
    in
        ( ( 0, y + 1 ), newModel )


{-| Parse an element of level data.

Helper function for [`parseRow`](#parseRow).
-}
parseElement : Char -> ( Pos, Model ) -> ( Pos, Model )
parseElement obj ( ( x, y ), model ) =
    let
        newModel =
            case obj of
                '#' ->
                    { model | walls = ( x, y ) :: model.walls }

                '.' ->
                    { model | endPoints = ( x, y ) :: model.endPoints }

                '$' ->
                    { model | boxes = ( x, y ) :: model.boxes }

                '@' ->
                    { model | player = ( x, y ) }

                _ ->
                    model
    in
        ( ( x + 1, y ), newModel )


{-| Return the tile width.

    tileWidth == 64
-}
tileWidth : Int
tileWidth =
    64


{-| Return the tile height.

    tileHeight == 64
-}
tileHeight : Int
tileHeight =
    64


{-| Simplify type annotation for [`levels`](#levels).
-}
type alias Level =
    List String


{-| Return the requested level number and data.

The first level is returned if `Int` is less than `0`. The last level is
returned if `Int` is more than `49`.

An empty [`Level`](#Level) will be returned if "the impossible happened" and
Elm failed to retrieve an element from [`levels`](#levels).
-}
load : Int -> ( Int, Level )
load level =
    let
        validLevel =
            if level < 0 then
                0
            else if level > 49 then
                49
            else
                level
    in
        List.getAt validLevel levels
            |> Maybe.withDefault []
            |> (,) validLevel


{-| Return the Sokoban levels.
-}
levels : List Level
levels =
    [ [ "    #####          "
      , "    #   #          "
      , "    #$  #          "
      , "  ###  $##         "
      , "  #  $ $ #         "
      , "### # ## #   ######"
      , "#   # ## #####  ..#"
      , "# $  $          ..#"
      , "##### ### #@##  ..#"
      , "    #     #########"
      , "    #######        "
      ]
    , [ "############  "
      , "#..  #     ###"
      , "#..  # $  $  #"
      , "#..  #$####  #"
      , "#..    @ ##  #"
      , "#..  # #  $ ##"
      , "###### ##$ $ #"
      , "  # $  $ $ $ #"
      , "  #    #     #"
      , "  ############"
      ]
    , [ "        ######## "
      , "        #     @# "
      , "        # $#$ ## "
      , "        # $  $#  "
      , "        ##$ $ #  "
      , "######### $ # ###"
      , "#....  ## $  $  #"
      , "##...    $  $   #"
      , "#....  ##########"
      , "########         "
      ]
    , [ "           ########"
      , "           #  ....#"
      , "############  ....#"
      , "#    #  $ $   ....#"
      , "# $$$#$  $ #  ....#"
      , "#  $     $ #  ....#"
      , "# $$ #$ $ $########"
      , "#  $ #     #       "
      , "## #########       "
      , "#    #    ##       "
      , "#     $   ##       "
      , "#  $$#$$  @#       "
      , "#    #    ##       "
      , "###########        "
      ]
    , [ "        #####    "
      , "        #   #####"
      , "        # #$##  #"
      , "        #     $ #"
      , "######### ###   #"
      , "#....  ## $  $###"
      , "#....    $ $$ ## "
      , "#....  ##$  $ @# "
      , "#########  $  ## "
      , "        # $ $  # "
      , "        ### ## # "
      , "          #    # "
      , "          ###### "
      ]
    , [ "######  ### "
      , "#..  # ##@##"
      , "#..  ###   #"
      , "#..     $$ #"
      , "#..  # # $ #"
      , "#..### # $ #"
      , "#### $ #$  #"
      , "   #  $# $ #"
      , "   # $  $  #"
      , "   #  ##   #"
      , "   #########"
      ]
    , [ "       ##### "
      , " #######   ##"
      , "## # @## $$ #"
      , "#    $      #"
      , "#  $  ###   #"
      , "### #####$###"
      , "# $  ### ..# "
      , "# $ $ $ ...# "
      , "#    ###...# "
      , "# $$ # #...# "
      , "#  ### ##### "
      , "####         "
      ]
    , [ "  ####          "
      , "  #  ###########"
      , "  #    $   $ $ #"
      , "  # $# $ #  $  #"
      , "  #  $ $  #    #"
      , "### $# #  #### #"
      , "#@#$ $ $  ##   #"
      , "#    $ #$#   # #"
      , "#   $    $ $ $ #"
      , "#####  #########"
      , "  #      #      "
      , "  #      #      "
      , "  #......#      "
      , "  #......#      "
      , "  #......#      "
      , "  ########      "
      ]
    , [ "          #######"
      , "          #  ...#"
      , "      #####  ...#"
      , "      #      . .#"
      , "      #  ##  ...#"
      , "      ## ##  ...#"
      , "     ### ########"
      , "     # $$$ ##    "
      , " #####  $ $ #####"
      , "##   #$ $   #   #"
      , "#@ $  $    $  $ #"
      , "###### $$ $ #####"
      , "     #      #    "
      , "     ########    "
      ]
    , [ " ###  #############"
      , "##@####       #   #"
      , "# $$   $$  $ $ ...#"
      , "#  $$$#    $  #...#"
      , "# $   # $$ $$ #...#"
      , "###   #  $    #...#"
      , "#     # $ $ $ #...#"
      , "#    ###### ###...#"
      , "## #  #  $ $  #...#"
      , "#  ## # $$ $ $##..#"
      , "# ..# #  $      #.#"
      , "# ..# # $$$ $$$ #.#"
      , "##### #       # #.#"
      , "    # ######### #.#"
      , "    #           #.#"
      , "    ###############"
      ]
    , [ "          ####     "
      , "     #### #  #     "
      , "   ### @###$ #     "
      , "  ##      $  #     "
      , " ##  $ $$## ##     "
      , " #  #$##     #     "
      , " # # $ $$ # ###    "
      , " #   $ #  # $ #####"
      , "####    #  $$ #   #"
      , "#### ## $         #"
      , "#.    ###  ########"
      , "#.. ..# ####       "
      , "#...#.#            "
      , "#.....#            "
      , "#######            "
      ]
    , [ "################ "
      , "#              # "
      , "# # ######     # "
      , "# #  $ $ $ $#  # "
      , "# #   $@$   ## ##"
      , "# #  $ $ $###...#"
      , "# #   $ $  ##...#"
      , "# ###$$$ $ ##...#"
      , "#     # ## ##...#"
      , "#####   ## ##...#"
      , "    #####     ###"
      , "        #     #  "
      , "        #######  "
      ]
    , [ "   #########       "
      , "  ##   ##  #####   "
      , "###     #  #    ###"
      , "#  $ #$ #  #  ... #"
      , "# # $#@$## # #.#. #"
      , "#  # #$  #    . . #"
      , "# $    $ # # #.#. #"
      , "#   ##  ##$ $ . . #"
      , "# $ #   #  #$#.#. #"
      , "## $  $   $  $... #"
      , " #$ ######    ##  #"
      , " #  #    ##########"
      , " ####              "
      ]
    , [ "       #######    "
      , " #######     #    "
      , " #     # $@$ #    "
      , " #$$ #   #########"
      , " # ###......##   #"
      , " #   $......## # #"
      , " # ###......     #"
      , "##   #### ### #$##"
      , "#  #$   #  $  # # "
      , "#  $ $$$  # $## # "
      , "#   $ $ ###$$ # # "
      , "#####     $   # # "
      , "    ### ###   # # "
      , "      #     #   # "
      , "      ########  # "
      , "             #### "
      ]
    , [ "   ########      "
      , "   #   #  #      "
      , "   #  $   #      "
      , " ### #$   ####   "
      , " #  $  ##$   #   "
      , " #  # @ $ # $#   "
      , " #  #      $ ####"
      , " ## ####$##     #"
      , " # $#.....# #   #"
      , " #  $..**. $# ###"
      , "##  #.....#   #  "
      , "#   ### #######  "
      , "# $$  #  #       "
      , "#  #     #       "
      , "######   #       "
      , "     #####       "
      ]
    , [ "#####         "
      , "#   ##        "
      , "#    #  ####  "
      , "# $  ####  #  "
      , "#  $$ $   $#  "
      , "###@ #$    ## "
      , " #  ##  $ $ ##"
      , " # $  ## ## .#"
      , " #  #$##$  #.#"
      , " ###   $..##.#"
      , "  #    #.*...#"
      , "  # $$ #.....#"
      , "  #  #########"
      , "  #  #        "
      , "  ####        "
      ]
    , [ "   ##########   "
      , "   #..  #   #   "
      , "   #..      #   "
      , "   #..  #  #### "
      , "  #######  #  ##"
      , "  #            #"
      , "  #  #  ##  #  #"
      , "#### ##  #### ##"
      , "#  $  ##### #  #"
      , "# # $  $  # $  #"
      , "# @$  $   #   ##"
      , "#### ## ####### "
      , "   #    #       "
      , "   ######       "
      ]
    , [ "     ###########   "
      , "     #  .  #   #   "
      , "     # #.    @ #   "
      , " ##### ##..# ####  "
      , "##  # ..###     ###"
      , "# $ #...   $ #  $ #"
      , "#    .. ##  ## ## #"
      , "####$##$# $ #   # #"
      , "  ## #    #$ $$ # #"
      , "  #  $ # #  # $## #"
      , "  #               #"
      , "  #  ###########  #"
      , "  ####         ####"
      ]
    , [ "  ######           "
      , "  #   @####        "
      , "##### $   #        "
      , "#   ##    ####     "
      , "# $ #  ##    #     "
      , "# $ #  ##### #     "
      , "## $  $    # #     "
      , "## $ $ ### # #     "
      , "## #  $  # # #     "
      , "## # #$#   # #     "
      , "## ###   # # ######"
      , "#  $  #### # #....#"
      , "#    $    $   ..#.#"
      , "####$  $# $   ....#"
      , "#       #  ## ....#"
      , "###################"
      ]
    , [ "    ##########     "
      , "#####        ####  "
      , "#     #   $  #@ #  "
      , "# #######$####  ###"
      , "# #    ## #  #$ ..#"
      , "# # $     #  #  #.#"
      , "# # $  #     #$ ..#"
      , "# #  ### ##     #.#"
      , "# ###  #  #  #$ ..#"
      , "# #    #  ####  #.#"
      , "# #$   $  $  #$ ..#"
      , "#    $ # $ $ #  #.#"
      , "#### $###    #$ ..#"
      , "   #    $$ ###....#"
      , "   #      ## ######"
      , "   ########        "
      ]
    , [ "#########      "
      , "#       #      "
      , "#       ####   "
      , "## #### #  #   "
      , "## #@##    #   "
      , "# $$$ $  $$#   "
      , "#  # ## $  #   "
      , "#  # ##  $ ####"
      , "####  $$$ $#  #"
      , " #   ##   ....#"
      , " # #   # #.. .#"
      , " #   # # ##...#"
      , " ##### $  #...#"
      , "     ##   #####"
      , "      #####    "
      ]
    , [ "######     ####    "
      , "#    #######  #####"
      , "#   $#  #  $  #   #"
      , "#  $  $  $ # $ $  #"
      , "##$ $   # @# $    #"
      , "#  $ ########### ##"
      , "# #   #.......# $# "
      , "# ##  # ......#  # "
      , "# #   $........$ # "
      , "# # $ #.... ..#  # "
      , "#  $ $####$#### $# "
      , "# $   ### $   $  ##"
      , "# $     $ $  $    #"
      , "## ###### $ ##### #"
      , "#         #       #"
      , "###################"
      ]
    , [ "    #######        "
      , "    #  #  ####     "
      , "##### $#$ #  ##    "
      , "#.. #  #  #   #    "
      , "#.. # $#$ #  $#### "
      , "#.  #     #$  #  # "
      , "#..   $#  # $    # "
      , "#..@#  #$ #$  #  # "
      , "#.. # $#     $#  # "
      , "#.. #  #$$#$  #  ##"
      , "#.. # $#  #  $#$  #"
      , "#.. #  #  #   #   #"
      , "##. ####  #####   #"
      , " ####  ####   #####"
      ]
    , [ "###############    "
      , "#..........  .#### "
      , "#..........$$.#  # "
      , "###########$ #   ##"
      , "#      $  $     $ #"
      , "## ####   #  $ #  #"
      , "#      #   ##  # ##"
      , "#  $#  # ##  ### ##"
      , "# $ #$###    ### ##"
      , "###  $ #  #  ### ##"
      , "###    $ ## #  # ##"
      , " # $  #  $  $ $   #"
      , " #  $  $#$$$  #   #"
      , " #  #  $      #####"
      , " # @##  #  #  #    "
      , " ##############    "
      ]
    , [ "####               "
      , "#  ##############  "
      , "#  #   ..#......#  "
      , "#  # # ##### ...#  "
      , "##$#    ........#  "
      , "#   ##$######  ####"
      , "# $ #     ######@ #"
      , "##$ # $   ######  #"
      , "#  $ #$$$##       #"
      , "#      #    #$#$###"
      , "# #### #$$$$$    # "
      , "# #    $     #   # "
      , "# #   ##        ###"
      , "# ######$###### $ #"
      , "#        #    #   #"
      , "##########    #####"
      ]
    , [ " #######       "
      , " #  #  #####   "
      , "##  #  #...### "
      , "#  $#  #...  # "
      , "# $ #$$ ...  # "
      , "#  $#  #... .# "
      , "#   # $########"
      , "##$       $ $ #"
      , "##  #  $$ #   #"
      , " ######  ##$$@#"
      , "      #      ##"
      , "      ######## "
      ]
    , [ " ################# "
      , " #...   #    #   ##"
      , "##.....  $## # #$ #"
      , "#......#  $  #    #"
      , "#......#  #  # #  #"
      , "######### $  $ $  #"
      , "  #     #$##$ ##$##"
      , " ##   $    # $    #"
      , " #  ## ### #  ##$ #"
      , " # $ $$     $  $  #"
      , " # $    $##$ ######"
      , " #######  @ ##     "
      , "       ######      "
      ]
    , [ "         #####   "
      , "     #####   #   "
      , "    ## $  $  ####"
      , "##### $  $ $ ##.#"
      , "#       $$  ##..#"
      , "#  ###### ###.. #"
      , "## #  #    #... #"
      , "# $   #    #... #"
      , "#@ #$ ## ####...#"
      , "####  $ $$  ##..#"
      , "   ##  $ $  $...#"
      , "    # $$  $ #  .#"
      , "    #   $ $  ####"
      , "    ######   #   "
      , "         #####   "
      ]
    , [ "#####              "
      , "#   ##             "
      , "# $  #########     "
      , "## # #       ######"
      , "## #   $#$#@  #   #"
      , "#  #      $ #   $ #"
      , "#  ### ######### ##"
      , "#  ## ..*..... # ##"
      , "## ## *.*..*.* # ##"
      , "# $########## ##$ #"
      , "#  $   $  $    $  #"
      , "#  #   #   #   #  #"
      , "###################"
      ]
    , [ "       ########### "
      , "       #   #     # "
      , "#####  #     $ $ # "
      , "#   ##### $## # ## "
      , "# $ ##   # ## $  # "
      , "# $  @$$ # ##$$$ # "
      , "## ###   # ##    # "
      , "## #   ### #####$# "
      , "## #     $  #....# "
      , "#  ### ## $ #....##"
      , "# $   $ #   #..$. #"
      , "#  ## $ #  ##.... #"
      , "#####   ######...##"
      , "    #####    ##### "
      ]
    , [ "  ####            "
      , "  #  #########    "
      , " ##  ##  #   #    "
      , " #  $# $@$   #### "
      , " #$  $  # $ $#  ##"
      , "##  $## #$ $     #"
      , "#  #  # #   $$$  #"
      , "# $    $  $## ####"
      , "# $ $ #$#  #  #   "
      , "##  ###  ###$ #   "
      , " #  #....     #   "
      , " ####......####   "
      , "   #....####      "
      , "   #...##         "
      , "   #...#          "
      , "   #####          "
      ]
    , [ "      ####   "
      , "  #####  #   "
      , " ##     $#   "
      , "## $  ## ### "
      , "#@$ $ # $  # "
      , "#### ##   $# "
      , " #....#$ $ # "
      , " #....#   $# "
      , " #....  $$ ##"
      , " #... # $   #"
      , " ######$ $  #"
      , "      #   ###"
      , "      #$ ### "
      , "      #  #   "
      , "      ####   "
      ]
    , [ "############"
      , "##     ##  #"
      , "##   $   $ #"
      , "#### ## $$ #"
      , "#   $ #    #"
      , "# $$$ # ####"
      , "#   # # $ ##"
      , "#  #  #  $ #"
      , "# $# $#    #"
      , "#   ..# ####"
      , "####.. $ #@#"
      , "#.....# $# #"
      , "##....#  $ #"
      , "###..##    #"
      , "############"
      ]
    , [ " #########    "
      , " #....   ##   "
      , " #.#.#  $ ##  "
      , "##....# # @## "
      , "# ....#  #  ##"
      , "#     #$ ##$ #"
      , "## ###  $    #"
      , " #$  $ $ $#  #"
      , " # #  $ $ ## #"
      , " #  ###  ##  #"
      , " #    ## ## ##"
      , " #  $ #  $  # "
      , " ###$ $   ### "
      , "   #  #####   "
      , "   ####       "
      ]
    , [ "############ ######"
      , "#   #    # ###....#"
      , "#   $$#   @  .....#"
      , "#   # ###   # ....#"
      , "## ## ###  #  ....#"
      , " # $ $     # # ####"
      , " #  $ $##  #      #"
      , "#### #  #### # ## #"
      , "#  # #$   ## #    #"
      , "# $  $  # ## #   ##"
      , "# # $ $    # #   # "
      , "#  $ ## ## # ##### "
      , "# $$     $$  #     "
      , "## ## ### $  #     "
      , " #    # #    #     "
      , " ###### ######     "
      ]
    , [ "            #####  "
      , "#####  ######   #  "
      , "#   ####  $ $ $ #  "
      , "# $   ## ## ##  ## "
      , "#   $ $     $  $ # "
      , "### $  ## ##     ##"
      , "  # ##### #####$$ #"
      , " ##$##### @##     #"
      , " # $  ###$### $  ##"
      , " # $  #   ###  ### "
      , " # $$ $ #   $$ #   "
      , " #     #   ##  #   "
      , " #######.. .###    "
      , "    #.........#    "
      , "    #.........#    "
      , "    ###########    "
      ]
    , [ "###########        "
      , "#......   #########"
      , "#......   #  ##   #"
      , "#..### $    $     #"
      , "#... $ $ #   ##   #"
      , "#...#$#####    #  #"
      , "###    #   #$  #$ #"
      , "  #  $$ $ $  $##  #"
      , "  #  $   #$#$ ##$ #"
      , "  ### ## #    ##  #"
      , "   #  $ $ ## ######"
      , "   #    $  $  #    "
      , "   ##   # #   #    "
      , "    #####@#####    "
      , "        ###        "
      ]
    , [ "      #### "
      , "####### @# "
      , "#     $  # "
      , "#   $## $# "
      , "##$#...# # "
      , " # $...  # "
      , " # #. .# ##"
      , " #   # #$ #"
      , " #$  $    #"
      , " #  #######"
      , " ####      "
      ]
    , [ "             ######"
      , " #############....#"
      , "##   ##     ##....#"
      , "#  $$##  $ @##....#"
      , "#      $$ $#  ....#"
      , "#  $ ## $$ # # ...#"
      , "#  $ ## $  #  ....#"
      , "## ##### ### ##.###"
      , "##   $  $ ##   .  #"
      , "# $###  # ##### ###"
      , "#   $   #       #  "
      , "#  $ #$ $ $###  #  "
      , "# $$$# $   # ####  "
      , "#    #  $$ #       "
      , "######   ###       "
      , "     #####         "
      ]
    , [ "    ############ "
      , "    #          ##"
      , "    #  # #$$ $  #"
      , "    #$ #$#  ## @#"
      , "   ## ## # $ # ##"
      , "   #   $ #$  # # "
      , "   #   # $   # # "
      , "   ## $ $   ## # "
      , "   #  #  ##  $ # "
      , "   #    ## $$# # "
      , "######$$   #   # "
      , "#....#  ######## "
      , "#.#... ##        "
      , "#....   #        "
      , "#....   #        "
      , "#########        "
      ]
    , [ "           #####   "
      , "          ##   ##  "
      , "         ##     #  "
      , "        ##  $$  #  "
      , "       ## $$  $ #  "
      , "       # $    $ #  "
      , "####   #   $$ #####"
      , "#  ######## ##    #"
      , "#.            $$$@#"
      , "#.# ####### ##   ##"
      , "#.# #######. #$ $##"
      , "#........... #    #"
      , "##############  $ #"
      , "             ##  ##"
      , "              #### "
      ]
    , [ "     ########     "
      , "  ####      ######"
      , "  #    ## $ $   @#"
      , "  # ## ##$#$ $ $##"
      , "### ......#  $$ ##"
      , "#   ......#  #   #"
      , "# # ......#$  $  #"
      , "# #$...... $$# $ #"
      , "#   ### ###$  $ ##"
      , "###  $  $  $  $ # "
      , "  #  $  $  $  $ # "
      , "  ######   ###### "
      , "       #####      "
      ]
    , [ "        #######    "
      , "    #####  #  #### "
      , "    #   #   $    # "
      , " #### #$$ ## ##  # "
      , "##      # #  ## ###"
      , "#  ### $#$  $  $  #"
      , "#...    # ##  #   #"
      , "#...#    @ # ### ##"
      , "#...#  ###  $  $  #"
      , "######## ##   #   #"
      , "          #########"
      ]
    , [ " #####             "
      , " #   #             "
      , " # # #######       "
      , " #      $@######   "
      , " # $ ##$ ###   #   "
      , " # #### $    $ #   "
      , " # ##### #  #$ ####"
      , "##  #### ##$      #"
      , "#  $#  $  # ## ## #"
      , "#         # #...# #"
      , "######  ###  ...  #"
      , "     #### # #...# #"
      , "          # ### # #"
      , "          #       #"
      , "          #########"
      ]
    , [ "##### ####      "
      , "#...# #  ####   "
      , "#...###  $  #   "
      , "#....## $  $### "
      , "##....##   $  # "
      , "###... ## $ $ # "
      , "# ##    #  $  # "
      , "#  ## # ### ####"
      , "# $ # #$  $    #"
      , "#  $ @ $    $  #"
      , "#   # $ $$ $ ###"
      , "#  ######  ###  "
      , "# ##    ####    "
      , "###             "
      ]
    , [ "##########    "
      , "#        #### "
      , "# ###### #  ##"
      , "# # $ $ $  $ #"
      , "#       #$   #"
      , "###$  $$#  ###"
      , "  #  ## # $## "
      , "  ##$#   $ @# "
      , "   #  $ $ ### "
      , "   # #   $  # "
      , "   # ##   # # "
      , "  ##  ##### # "
      , "  #         # "
      , "  #.......### "
      , "  #.......#   "
      , "  #########   "
      ]
    , [ "         ####     "
      , " #########  ##    "
      , "##  $      $ #####"
      , "#   ## ##   ##...#"
      , "# #$$ $ $$#$##...#"
      , "# #   @   #   ...#"
      , "#  $# ###$$   ...#"
      , "# $  $$  $ ##....#"
      , "###$       #######"
      , "  #  #######      "
      , "  ####            "
      ]
    , [ "  #########  "
      , "  #*.*#*.*#  "
      , "  #.*.*.*.#  "
      , "  #*.*.*.*#  "
      , "  #.*.*.*.#  "
      , "  #*.*.*.*#  "
      , "  ###   ###  "
      , "    #   #    "
      , "###### ######"
      , "#           #"
      , "# $ $ $ $ $ #"
      , "## $ $ $ $ ##"
      , " #$ $ $ $ $# "
      , " #   $@$   # "
      , " #  #####  # "
      , " ####   #### "
      ]
    , [ "       ####     "
      , "       #  ##    "
      , "       #   ##   "
      , "       # $$ ##  "
      , "     ###$  $ ## "
      , "  ####    $   # "
      , "###  # #####  # "
      , "#    # #....$ # "
      , "# #   $ ....# # "
      , "#  $ # #.*..# # "
      , "###  #### ### # "
      , "  #### @$  ##$##"
      , "     ### $     #"
      , "       #  ##   #"
      , "       #########"
      ]
    , [ "      ############ "
      , "     ##..    #   # "
      , "    ##..* $    $ # "
      , "   ##..*.# # # $## "
      , "   #..*.# # # $  # "
      , "####...#  #    # # "
      , "#  ## #          # "
      , "# @$ $ ###  #   ## "
      , "# $   $   # #   #  "
      , "###$$   # # # # #  "
      , "  #   $   # # #####"
      , "  # $# #####      #"
      , "  #$   #   #    # #"
      , "  #  ###   ##     #"
      , "  #  #      #    ##"
      , "  ####      ###### "
      ]
    ]
