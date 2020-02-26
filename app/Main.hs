{-# LANGUAGE Templatehaskell #-}
module Main where

import qualified Graphics.Vty as Vty
import Brick
import Brick.BChan
import Control.Concurrent (forkIO)
import Control.Monad (forever)
import Control.Lens (makeLenses)
import FaceDataServer.Types (Radian, Percent)
import FaceDataServer.Connection
import Network.Multicast (multicastSender)

data Name = NoName
data AppState = AppState { _sock     :: Socket
                         , _addr     :: SockAddr
                         , _faceData :: FaceData
                         }
makeLenses ''AppState

mkInitialState sock addr = AppState sock addr defaultFaceData

hostName = "226.0.0.1"
portNum = 5032

ui s = [vBox [str "Face-Data-Server easy controller"]]

-- Event handler. Temporary code. NO COMMIT {{{
update f l s = do
    let s' = over f l s
    sendFaceData (s^.sock) (s^.addr) s'
    return s'

eHandler s (VtyEvent (Vty.EvKey (Vty.KEsc) [])) = halt s
eHandler s (VtyEvent (Vty.EvKey (Vty.KChar 'a') [])) = update (subtract 1/pi) (faceData&face_y_radian) s >>= continue
eHandler s (VtyEvent (Vty.EvKey (Vty.KChar 's') [])) = update (subtract 1/pi) (faceData&face_x_radian) s >>= continue
eHandler s (VtyEvent (Vty.EvKey (Vty.KChar 'w') [])) = update (+ 1/pi) (faceData&face_x_radian) s >>= continue
eHandler s (VtyEvent (Vty.EvKey (Vty.KChar 'd') [])) = update (+ 1/pi) (faceData&face_y_radian) s >>= continue
eHandler s (VtyEvent (Vty.EvKey (Vty.KChar 'q') [])) = update (subtract 1/pi) (faceData&face_z_radian) s >>= continue
eHandler s (VtyEvent (Vty.EvKey (Vty.KChar 'e') [])) = update (+ 1/pi) (faceData&face_z_radian) s >>= continue
eHandler s (VtyEvent (Vty.EvKey (Vty.KChar 'd') [])) = undefined
-- }}}

app :: App AppState e Name
app = App { appDraw         = ui
          , appHandleEvent  = eHandler
          , appStartEvent   = return
          , appChooseCursor = neverShowCursor
          , appAttrMap      = const $ attrMap Vty.defAttr []
          }

main :: IO ()
main = do
    (s, adr) <- multicastSender hostName portNum
    void $ defaultMain app $ mkInitalState s adr
