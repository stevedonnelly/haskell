module IO.SDL where
import Data.List as List
import Data.Word as Word
import Foreign.Marshal.Array as Array
import Foreign.Ptr as Ptr
import Foreign.Storable as Storable
import Graphics.UI.SDL as SDL
import Prelude.Extensions as PreludeExt

splitBlocks = \size list -> let
    splitBlock = \x (blocks, length) -> let
        append = ((:) ((:) x (head blocks)) (tail blocks), (+) length 1)
        new_block = ((:) [x] blocks, 1)
        in (ifElse ((==) length size) new_block append)
    result = (fst (List.foldr splitBlock ([[]], 0) list))
    in (ifElse (List.null list) [] result)

pixelRowLists :: Surface -> IO [[[Word8]]]
pixelRowLists = \surface -> do
    let (width, height) = (surfaceGetWidth surface, surfaceGetHeight surface)
    let format = (surfaceGetPixelFormat surface)
    bytes_per_pixel_word <- (pixelFormatGetBytesPerPixel format)
    let bytes_per_pixel = (fromIntegral bytes_per_pixel_word)
    image_buffer <- (surfaceGetPixels surface)
    let pixel_pointer = (Ptr.castPtr image_buffer) :: (Ptr Word8)
    pixel_data <- (Array.peekArray (product [height, width, bytes_per_pixel]) pixel_pointer)
    (return (splitBlocks width (splitBlocks bytes_per_pixel pixel_data)))


