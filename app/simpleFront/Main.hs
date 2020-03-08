{-# LANGUAGE TemplateHaskell #-}
module Main where

import qualified Graphics.Vty as Vty
import Brick
import Brick.BChan
import Brick.Widgets.Center (hCenter)
import Brick.Widgets.Border (border)
import Control.Concurrent (forkIO)
import Control.Monad (forever, void)
import Control.Monad.IO.Class (liftIO)
import Control.Lens
import qualified FaceDataServer as FDS
import FaceDataServer.Types
import FaceDataServer.Connection
import Network.Socket (Socket, SockAddr)
import Network.Multicast (multicastReceiver)



data CustomEvent = GetData FaceData
data Name = NoName deriving (Eq, Ord)


uiFaceRadians :: FaceData -> Widget Name
uiFaceRadians s = border $ vBox [ str "Face Rotations"
                                , str $ "X: " ++ show (s^.face_x_radian)
                                , str $ "Y: " ++ show (s^.face_y_radian)
                                , str $ "Z: " ++ show (s^.face_z_radian)
                                ]

uiMouthPercents s = border $ vBox [ str "Mouth size percentage"
                                  , str $ "height: " ++ show (s^.mouth_height_percent)
                                  , str $ "width: "  ++ show (s^.mouth_width_percent)
                                  ]

uiEyePercents s = border $ vBox [ str "Eyes size percentage"
                                , str $ "Left eye: " ++ show (s^.left_eye_percent)
                                , str $ "Right eye: " ++ show (s^.right_eye_percent)
                                ]



ui s = [vBox [ hCenter $ str "Face-Data-Server simple frontend viewer"
             , uiFaceRadians s
             , uiMouthPercents s
             , uiEyePercents s
             ]
       ]

eHandler s (VtyEvent (Vty.EvKey (Vty.KChar 'q') [])) = halt s
eHandler s (AppEvent (GetData d)) = continue d
eHandler s _ = continue s

app :: App FaceData CustomEvent Name
app = App { appDraw         = ui
          , appHandleEvent  = eHandler
          , appStartEvent   = return
          , appChooseCursor = neverShowCursor
          , appAttrMap      = const $ attrMap Vty.defAttr []
          }


main = do
    s <- multicastReceiver FDS.defaultGroupAddr FDS.defaultPortNumber

    -- Create thread to receive FaceData
    -- & convert into Event
    chan <- newBChan 10
    forkIO $ forever $ do
        facedata <- getFaceData s
        writeBChan chan $ GetData facedata

    -- Create Vty
    let buildVty = Vty.mkVty Vty.defaultConfig
    initialVty <- buildVty

    void $ customMain initialVty buildVty (Just chan) app defaultFaceData
