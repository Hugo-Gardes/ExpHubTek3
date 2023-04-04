import Graphics.Gloss
import Graphics.Gloss.Juicy
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Data.Picture
import System.Environment
import Data.Maybe

takeExtension :: FilePath -> String
takeExtension filename = reverse $ takeWhile (/= '.') $ reverse filename
takeExtension _ = "error"

loadPicture :: FilePath -> IO Picture
loadPicture filename = do
    let ext = takeExtension filename
    case ext of
        "bmp" -> loadBMP filename
        "jpg" -> fromMaybe (error "Could not load image") <$> loadJuicyJPG filename
        "png" -> fromMaybe (error "Could not load image") <$> loadJuicyPNG filename
        _ -> error $ "Unknown file extension: " ++ ext
loadPicture _ = error "No file name"

main :: IO ()
main = do
  args <- getArgs
  case args of
    [filename] -> do
      image <- loadPicture filename
      let world = World { image = image }
      play window background fps world renderWorld handleInput updateWorld
    _ -> putStrLn "Usage: image_viewer <filename>"
    where
      width = 800
      height = 600
      window = InWindow "Image Viewer" (width, height) (10, 10)
      background = white
      fps = 60

data World = World { image :: Picture }

renderWorld :: World -> Picture
renderWorld world = image world

handleInput :: Event -> World -> World
handleInput _ world = world

updateWorld :: Float -> World -> World
updateWorld _ = id
