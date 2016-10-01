module Player where

import Prelude

data Direction = FacingLeft | FacingRight
data PlayerPosition = PlayerPosition Number Number Direction

playerSpeed :: Number
playerSpeed = 4.0

startP1 :: PlayerPosition
startP1 = PlayerPosition 200.0 470.0 FacingRight

startP2 :: PlayerPosition
startP2 = PlayerPosition 1170.0 470.0 FacingLeft

moveLeft :: PlayerPosition -> PlayerPosition
moveLeft (PlayerPosition x y d) = PlayerPosition (x - playerSpeed) y FacingLeft

moveRight :: PlayerPosition -> PlayerPosition
moveRight (PlayerPosition x y d) = PlayerPosition (x + playerSpeed) y FacingRight
