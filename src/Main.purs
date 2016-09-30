module Main where

import Prelude
import Graphics.Canvas as Canvas
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (error, CONSOLE, log)
import Data.Maybe (Maybe(..))
import Graphics.Canvas (CanvasImageSource)

main :: forall e. Eff ( console :: CONSOLE
                      , canvas :: Canvas.CANVAS
                      | e) Unit
main = do
  drawBackground
  mcvas <- Canvas.getCanvasElementById "canvas_game"
  case mcvas of
    Nothing -> error "Whoops"
    Just cvas -> do
      ctx <- Canvas.getContext2D cvas
      Canvas.withImage "assets/Ichigo_Idle.png" \i ->
        drawPlayer ctx i startP1
      Canvas.withImage "assets/villain.png" \i ->
        drawPlayer ctx i startP2
      log "Hello sailor!"

startP1 :: PlayerPosition
startP1 = PlayerPosition 100.0 470.0 FacingRight

startP2 :: PlayerPosition
startP2 = PlayerPosition 1170.0 470.0 FacingLeft

data Direction = FacingLeft | FacingRight
data PlayerPosition = PlayerPosition Number Number Direction

drawPlayer
  :: forall e
  . Canvas.Context2D
  -> CanvasImageSource
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
