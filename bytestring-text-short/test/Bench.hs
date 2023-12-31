{-# LANGUAGE NoImplicitPrelude
           , OverloadedStrings
           , BangPatterns
  #-}


import Data.ByteString.Text.Short.Internal
import Data.ByteString.Text.Short.Internal.Search
import Data.ByteString.Text.Builder.Internal.Prelude

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
            $ nf (List.null . uncurry elemIndicesBS) (47, haystack)) :
        (bench "indicesBrutal"
            $ nf (List.null . indicesBrutalBS needle) haystack) :
        (bench "indicesBrutal (unprocessed)"
            $ nf (List.null . uncurry indicesBrutalBS) both) :
        (bench "indicesTwoWay"
            $ nf (List.null . indicesTwoWayBS needle) haystack) :
        (bench "indicesTwoWay (unprocessed)"
            $ nf (List.null . uncurry indicesTwoWayBS) both) :
        (bench "indices"
            $ nf (List.null . indicesBS needle) haystack) :
        (bench "indices (unprocessed)"
            $ nf (List.null . uncurry indicesBS) both) :
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
          $ nf (List.null . uncurry indicesBrutalBS) both) :
      (bench "indicesTwoWay"
          $ nf (List.null . indicesTwoWayBS needle) haystack) :
      (bench "indicesTwoWay (unprocessed)"
          $ nf (List.null . uncurry indicesTwoWayBS) both) :
      (bench "indices"
          $ nf (List.null . indicesBS needle) haystack) :
      (bench "indices (unprocessed)"
          $ nf (List.null . uncurry indicesBS) both) :
      []) :

    (let
      !needle = "cccpcpc"
      !haystack = "pcppccpucdceedecccpcpccpuputpccccp"
      !both = (needle, haystack)
    in bgroup "first index of short repetitive needle" $
      (bench "indicesBrutal"
          $ nf (List.null . indicesBrutalBS needle) haystack) :
      (bench "indicesBrutal (unprocesed)"
          $ nf (List.null . uncurry indicesBrutalBS) both) :
      (bench "indicesTwoWay"
          $ nf (List.null . indicesTwoWayBS needle) haystack) :
      (bench "indicesTwoWay (unprocesed)"
          $ nf (List.null . uncurry indicesTwoWayBS) both) :
      (bench "indices"
          $ nf (List.null . indicesBS needle) haystack) :
      (bench "indices (unprocesed)"
          $ nf (List.null . uncurry indicesBS) both) :
      []) :

    (let
      !needle = "baa"
      !haystack = case replicate 100 "aaaabacaadaabeaaaaabaacaad" of SBS bs -> bs
      !both = (needle, haystack)
    in bgroup "count short needle;long repetitive haystack" $
      (bench "indicesBrutal"
          $ nf (List.length . indicesBrutalBS needle) haystack) :
      (bench "indicesBrutal (unprocesed)"
          $ nf (List.length . uncurry indicesBrutalBS) both) :
      (bench "indicesTwoWay"
          $ nf (List.length . indicesTwoWayBS needle) haystack) :
      (bench "indicesTwoWay (unprocesed)"
          $ nf (List.length . uncurry indicesTwoWayBS) both) :
      (bench "indices"
          $ nf (List.length . indicesBS needle) haystack) :
      (bench "indices (unprocesed)"
          $ nf (List.length . uncurry indicesBS) both) :
    []) :

    (let
      !needle = case replicate 2 "aaaabacaadaabeaa" of SBS bs -> bs
      !haystack = case replicate 100 "aaaabacaadaabeaaaaabaacaaaaaaabacaadaabeaaaaabaacaad" of SBS bs -> bs
      !both = (needle, haystack)
    in bgroup "count 32 byte repetitive needle;long repetitive haystack" $
      (bench "indicesBrutal"
          $ nf (List.length . indicesBrutalBS needle) haystack) :
      (bench "indicesBrutal (unprocesed)"
          $ nf (List.length . uncurry indicesBrutalBS) both) :
      (bench "indicesTwoWay"
          $ nf (List.length . indicesTwoWayBS needle) haystack) :
      (bench "indicesTwoWay (unprocesed)"
          $ nf (List.length . uncurry indicesTwoWayBS) both) :
      (bench "indices"
          $ nf (List.length . indicesBS needle) haystack) :
      (bench "indices (unprocesed)"
          $ nf (List.length . uncurry indicesBS) both) :
    []) :

    -- (let
    --   !needle = case replicate 2 "aaaabacaadaabeaaaaabaacaad" of SBS bs -> bs
    --   !haystack = case replicate 100 "aaaabacaadaabeaaaaabaacaaaaaaabacaadaabeaaaaabaacaad" of SBS bs -> bs
    --   !both = (needle, haystack)
    -- in bgroup "count long repetitive needle;long repetitive haystack" $
    --   (bench "indicesBrutal"
    --       $ nf (List.length . indicesBrutalBS needle) haystack) :
    --   (bench "indicesBrutal (unprocesed)"
    --       $ nf (List.length . uncurry indicesBrutalBS) both) :
    --   (bench "indicesTwoWay"
    --       $ nf (List.length . indicesTwoWayBS needle) haystack) :
    --   (bench "indicesTwoWay (unprocesed)"
    --       $ nf (List.length . uncurry indicesTwoWayBS) both) :
    --   (bench "indices"
    --       $ nf (List.length . indicesBS needle) haystack) :
    --   (bench "indices (unprocesed)"
    --       $ nf (List.length . uncurry indicesBS) both) :
    -- []) :

    []
