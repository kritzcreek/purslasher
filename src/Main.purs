module Main where

import Prelude
import Graphics.Canvas as Canvas
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (error, CONSOLE)
import Control.Monad.Eff.Ref (REF, Ref, modifyRef, readRef, newRef)
import Data.Maybe (Maybe(..))
import Player (Direction(..), moveRight, moveLeft, startP2, startP1, PlayerPosition(PlayerPosition))

type GameState =
  { player1 :: PlayerPosition
  , player2 :: PlayerPosition
  , sprite1 :: Canvas.CanvasImageSource
  , sprite2 :: Canvas.CanvasImageSource
  }

main :: forall e. Eff ( console :: CONSOLE
                      , canvas :: Canvas.CANVAS
                      , ref :: REF
                      | e) Unit
main = do
  drawBackground
  attachKeyListeners
  mcvas <- Canvas.getCanvasElementById "canvas_game"
  case mcvas of
    Nothing -> error "Whoops"
    Just cvas -> do
      ctx <- Canvas.getContext2D cvas
      gameLoop ctx

gameLoop
  :: forall e
  . Canvas.Context2D
  -> Eff ( ref :: REF, canvas :: Canvas.CANVAS | e) Unit
gameLoop ctx = do
  Canvas.withImage "assets/Ichigo_Idle.png" \i1 -> do
    Canvas.withImage "assets/villain.png" \i2 -> do
      gameState <- newRef { player1: startP1
                          , player2: startP2
                          , sprite1: i1
                          , sprite2: i2
                          }
      requestAnimationFrame (step ctx gameState)

step
  :: forall e
   . Canvas.Context2D
   -> Ref GameState
   -> Number
   -> Eff (canvas :: Canvas.CANVAS, ref :: REF | e) Unit
step ctx ref delta = do
  cleanCanvas ctx
  gs <- readRef ref
  {left, up, right} <- getKeys
  when left
    (modifyRef ref (\gsP -> gsP {player1 = moveLeft gs.player1}))
  when right
    (modifyRef ref (\gsP -> gsP {player1 = moveRight gs.player1}))
  drawPlayer ctx gs.sprite1 gs.player1
  drawPlayer ctx gs.sprite2 gs.player2
  requestAnimationFrame (step ctx ref)

cleanCanvas
  :: forall e. Canvas.Context2D -> Eff ( canvas :: Canvas.CANVAS | e) Unit
cleanCanvas ctx =
  Canvas.clearRect ctx {x: 0.0, y: 0.0, w: 1280.0, h: 720.0} $> unit

drawPlayer
  :: forall e
  . Canvas.Context2D
  -> Canvas.CanvasImageSource
  -> PlayerPosition
  -> Eff ( canvas :: Canvas.CANVAS | e) Unit
drawPlayer ctx img (PlayerPosition x y direction) = do
  Canvas.withContext ctx do
    Canvas.translate {translateX: x, translateY: y} ctx
    Canvas.scale {scaleX: directionToScale direction, scaleY: 1.0} ctx
    Canvas.drawImage ctx img 0.0 0.0
  pure unit
  where
    directionToScale = case _ of
      FacingRight -> 1.0
      FacingLeft -> -1.0

drawBackground :: forall e. Eff ( canvas :: Canvas.CANVAS
                                , console :: CONSOLE | e) Unit
drawBackground = do
  mcvas <- Canvas.getCanvasElementById "canvas_background"
  case mcvas of
    Nothing -> error "Whoops"
    Just cvas -> do
      ctx <- Canvas.getContext2D cvas
      Canvas.withImage "assets/Background.png" \i -> do
        Canvas.withImage "assets/Background2.png" \i2 -> do
          Canvas.withImage "assets/Graseffekt.png" \i3 -> do
            Canvas.drawImage ctx i 0.0 0.0
            Canvas.drawImage ctx i2 0.0 0.0
            Canvas.drawImage ctx i3 0.0 0.0
            pure unit

foreign import attachKeyListeners :: forall e. Eff (ref :: REF | e) Unit
foreign import isKeyPressed :: forall e. String -> Eff (ref :: REF | e) Boolean
foreign import requestAnimationFrame
  :: forall e . (Number -> Eff e Unit) -> Eff e Unit

getKeys
  :: forall e
  . Eff (ref :: REF | e) {left :: Boolean, up :: Boolean, right :: Boolean }
getKeys = do
  left <- isKeyPressed "37"
  up <- isKeyPressed "38"
  right <- isKeyPressed "39"
  pure {left, up, right}
