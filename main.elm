import Svg            exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events     exposing (..)

import Time  exposing (Time, every, millisecond)

import Color exposing (..)


type Action    = ActNop | ActTick Time | ActReset | ActBlock Tile 
type GameState = GSPlaying | GSWin | GSLose

type alias Model =
    { world  : World
    , cat    : Cat
    , state  : GameState
    , fade   : Float -- 0.0 ~ 1.0
    }

type alias Cat =
    { pos           : Pos
    , walking       : Bool
    , drawPos       : Pos
    , dir           : Int
    , animationStep : Int }

type alias Tile =
    { pos    : Pos
    , block  : Bool }
    
type alias World = List Tile
type alias Pos   = (Int, Int)

initialModel : Model
initialModel =
    let
        world = [-1..11]
             |> List.concatMap ((,) >> flip List.map [-1..11])
             |> List.map (flip Tile False)
        cat   = Cat (5, 5) False (5, 5) 0 0
    in 
        Model world cat GSPlaying 0.0

getScreenPos : (Int, Int) -> (Float, Float)
getScreenPos (x, y) =
    let
        extraX = if y % 2 == 0 then 0.0 else 0.5
    in
        ((toFloat x + extraX) * 32, toFloat <| y * 32)

drawTile : Signal.Address Action -> Tile -> Svg
drawTile address tile =
    let
        (x', y') = getScreenPos tile.pos
        color    = if isPosBorder tile.pos then "pink" else if tile.block then "black" else "yellow"
    in
        svg
          [ x <| toString <| x', y <| toString <| y', transform "rotate(90 0 0)"
          , onClick (Signal.message address (ActBlock tile)) ]
          [ polygon [ points "30,15 22.5,28 7.5,28 0,15 7.5,2 22.5,2", fill color
                    , stroke "black", strokeWidth "1" ] []
          ]

drawCat : Cat -> Svg
drawCat cat =
    let
        (x', y')       = getScreenPos cat.pos
        (x'', y'')     = getScreenPos cat.drawPos
        maxSteps       = Basics.toFloat 4 + 1
        step           = Basics.toFloat cat.animationStep + 1
        (drawX, drawY) = ( x' + (x'' - x') * (1 - (1 / maxSteps) * step)
                         , y' + (y'' - y') * (1 - (1 / maxSteps) * step) )
    in
        svg
          [ x <| toString <| drawX, y <| toString <| drawY ]
          [ -- polygon [ points "30,15 22.5,28 7.5,28 0,15 7.5,2 22.5,2"
            --        , fill "red", stroke "black", strokeWidth "1" ] []
           -- image [ xlinkHref <| "http://cat.com/" ++ toString cat.dir ++ "/" ++ toString cat.animationStep
            image [ xlinkHref "http://icons.iconarchive.com/icons/icons8/windows-8/32/Animals-Cat-icon.png" 
                  , width "32px", height "32px" ] []
          ]

drawModel : Signal.Address Action -> Model -> Svg
drawModel address model =
    let (x', y') = getScreenPos (0, 13) in
    svg [ width "500", height "500", viewBox "-50 -50 500 500", opacity <| toString <| model.fade ]
      <| List.map (drawTile address) model.world
      ++ [ drawCat model.cat ]
      ++ [ text' [ x <| toString x',  y <| toString <| y' + 20, fill "black" 
         , onClick (Signal.message address ActReset) ] [ text "Reset" ] ]
      ++ [ text' [ x <| toString 275, y <| toString <| y' + 20, fill "black" ] [ text "By Gabber" ] ]

isPosBorder : (Int, Int) -> Bool
isPosBorder (x, y) =
    x == -1 || x == 11 || y == -1 || y == 11
    
isCatStucked : Pos -> World -> Bool
isCatStucked pos world =
    not <| List.any (isWalkable pos) world

isWalkable : Pos -> Tile -> Bool
isWalkable (x, y) tile =
    if tile.block || tile.pos == (x, y)
    then False
    else
        let
            (x2, y2) = tile.pos
            (dx, dy) = (abs <| x - x2, abs <| y - y2)
        in dx <= 1 && dy <= 1
        && if rem y 2 == 0
           then y == y2 || x >= x2
           else y == y2 || x <= x2

getDistance : Pos -> Pos -> Int
getDistance (x, y) (x2, y2) =
    let
        (dx, dy) = (abs <| x - x2, abs <| y - y2)
    in
        Basics.max dx dy

getPath : World -> Pos -> Pos -> Maybe (List Pos)
getPath world x2y2 xy =
    let
    getPath' : World -> Pos -> Pos -> List Pos -> Maybe (List Pos)
    getPath' world (x2, y2) (x, y) xs =    
        let
        neighbors = world
                 |> List.filter (isWalkable (x, y)) 
                 |> List.map .pos
        world_without_my_neighbors =
            world
            |> List.filter (.pos >> (==) >> flip List.any ((x, y) :: neighbors) >> not) 
        k = Debug.log "LEN" <| List.length world_without_my_neighbors
        in
        if List.length neighbors == 0
        then Nothing
        else
            if (x, y) == (x2, y2)
            then Just <| (x, y) :: xs
            else
                world_without_my_neighbors
                |> List.map .pos
                |> List.filterMap (\pos -> getPath' world_without_my_neighbors (x2, y2) pos ((x, y) :: xs))
                |> List.head
    in
        getPath' world x2y2 xy []

-- TO - DO usando os 4 pontos do mapa
getBestStepToRunAway : Pos -> World -> Maybe Pos
getBestStepToRunAway pos world =
    let
        world' = world
                 |> List.filter (isWalkable pos)
                 |> List.map .pos
    in world'
       |> List.sortBy (getDistance >> flip List.map (List.filter isPosBorder world')
                   >> List.minimum >> Maybe.withDefault 999)
       |> List.head -- TO-DO: random

update : Action -> Model -> Model
update action model =
    --case Debug.log "Log: " action of
    case action of
        ActNop    -> model
        ActReset  -> initialModel
        ActTick t ->
            let
                cat     = model.cat
                step    = cat.animationStep + if cat.walking then 1 else 0
                walking = cat.walking && step < 5
                cat'    = { cat | animationStep = if step >= 5 then 0 else step
                                , walking = walking
                                , drawPos = if walking then cat.drawPos else cat.pos 
                                }
            in
                
            case model.state of
                GSPlaying -> { model | fade = Basics.min 1.0 (model.fade + 0.1)
                                     , cat  = cat' }
                _ -> -- GSWin e GSLose
                   if model.fade > 0.0
                   then { model | fade  = model.fade - 0.1, cat = cat' }
                   else initialModel
               
        ActBlock tile ->
          if model.state /= GSPlaying  || tile.block || model.cat.walking
          || tile.pos == model.cat.pos || isPosBorder tile.pos
          then model
          else
              let
                  world =
                      model.world
                      |> List.map (\t -> if tile.pos == t.pos then Tile tile.pos True else t)
                  cat    = model.cat
                  newpos =
                      Maybe.withDefault cat.pos <| getBestStepToRunAway model.cat.pos world
                      
                      {- S T A C K     O V E R F L O W
                      case getPath world (-1, -1) model.cat.pos of
                          Just xs -> Maybe.withDefault model.cat.pos <| List.head xs
                          Nothing -> model.cat.pos -- -}
              in
              { model
              | cat =
                  { cat
                  | pos     = newpos
                  , walking = True
                  }
              , world = world
              , state =
                  if isPosBorder newpos
                  then GSLose
                  else if isCatStucked newpos world
                       then GSWin
                       else GSPlaying
              }
  
main : Signal Svg
main =
  let
      mailbox = Signal.mailbox ActNop
  in
  mailbox.signal
  |> Signal.merge (Signal.map ActTick <| every <| 100 * millisecond)
  |> Signal.foldp update initialModel 
  |> Signal.map (drawModel mailbox.address)
