module Main where

import Brick
import Brick.Widgets.Border
import Graphics.Vty.Attributes

type AppState = Int

app :: App AppState () String
app =
  App
  { appDraw = appDraw'
  , appChooseCursor =
      const . const Nothing                     :: AppState -> [CursorLocation String] -> Maybe (CursorLocation String)
  , appHandleEvent =
      (\appState _ -> Brick.halt appState)  :: AppState -> BrickEvent String () -> EventM String (Next AppState)
  , appStartEvent = return                      :: AppState -> EventM String AppState
  , appAttrMap = const (attrMap (defAttr) ([])) :: AppState -> AttrMap
  }

appDraw' :: AppState -> [Widget String]
appDraw' _ = [
    (str "Hello," <=> str "World!" <=> hBorder)
  , hBox [str "                                              Woohoo"]
  ]


main :: IO ()
main = do
  let initialState = 5
  defaultMain app initialState >>= print
