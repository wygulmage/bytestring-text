{-# LANGUAGE OverloadedStrings
           , BangPatterns
  #-}


import Data.ByteString.Text.Short.Internal.Search

import Test.Tasty.Bench

import qualified Data.List as List

main :: IO ()
main = defaultMain $
    (let
       !needle = "/"
       !haystack = "/ascii ByteString"
       !both = (needle, haystack)
    in bgroup "first index" $
        (bench "elemIndices"
            $ nf (List.null . elemIndicesBS 47) haystack) :
        (bench "elemIndices (unprocessed)"
            $ nf (\ (pat, txt) -> List.null (elemIndicesBS pat txt)) (47, haystack)) :
        (bench "indicesBrutal"
            $ nf (List.null . indicesBrutalBS needle) haystack) :
        (bench "first index indicesBrutal (unprocessed)"
            $ nf (\ (pat, txt) -> List.null (indicesBrutalBS pat txt)) both) :
        (bench "first index indicesTwoWay"
            $ nf (List.null . indicesTwoWayBS needle) haystack) :
        (bench "first index indicesTwoWay (unprocessed)"
            $ nf (\ (pat, txt) -> List.null (indicesTwoWayBS pat txt)) both) :
        (bench "first index indices"
            $ nf (List.null . indicesBS needle) haystack) :
        (bench "first index indices (unprocessed)"
            $ nf (\ (pat, txt) -> List.null (indicesBS pat txt)) both) :
        []) :

    (let
      !needle = ":"
      !haystack = "some.long.url.co.uk/webstore/foo.asp.net.php#:~:nothingHerePal"
      !both = (needle, haystack)
    in bgroup "index farther out" $
      (bench "elemIndices"
          $ nf (List.null . elemIndicesBS 58) haystack) :
      (bench "indicesBrutal"
          $ nf (List.null . indicesBrutalBS needle) haystack) :
      (bench "indicesBrutal (unprocessed)"
          $ nf (\ (pat, txt) -> List.null (indicesBrutalBS pat txt)) both) :
      (bench "indicesTwoWay"
          $ nf (List.null . indicesTwoWayBS needle) haystack) :
      (bench "indicesTwoWay (unprocessed)"
          $ nf (\ (pat, txt) -> List.null (indicesTwoWayBS pat txt)) both) :
      (bench "index indices"
          $ nf (List.null . indicesBS needle) haystack) :
      (bench "index indices (unprocessed)"
          $ nf (\ (pat, txt) -> List.null (indicesBS pat txt)) both) :
      []) :

    (let
        !needle = "cccpcpc"
        !haystack = "pcppccpucdceedecccpcpccpuputpccccp"
    in bgroup "short repetitive needle" $
      (bench "indicesBrutal"
          $ nf (List.null . indicesBrutalBS needle) haystack) :
      (bench "indicesTwoWay"
          $ nf (List.null . indicesTwoWayBS needle) haystack) :
      (bench "indicesTwoWay (unprocesed)"
          $ nf (\ (pat, txt) -> List.null (indicesTwoWayBS pat txt)) (needle, haystack)) :
      (bench "indices"
          $ nf (List.null . indicesBS needle) haystack) :
      (bench "indices (unprocesed)"
          $ nf (\ (pat, txt) -> List.null (indicesBS pat txt)) (needle, haystack)) :
      []) :

    []
