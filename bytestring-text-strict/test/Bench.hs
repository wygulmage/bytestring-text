{-# LANGUAGE NoImplicitPrelude
           , OverloadedStrings
           , BangPatterns
  #-}


import Data.ByteString.Text.Strict
import Data.ByteString.Text.Strict.Internal
import Data.ByteString.Text.Builder.Internal.Prelude

import qualified Data.ByteString as BS

import Test.Tasty.Bench

import qualified Data.List as List

main :: IO ()
main = defaultMain $
    (let
       !needle = "/"
       !haystack = "/ascii ByteString"
       !both = (needle, haystack)
    in bgroup "first index" $
        -- (bench "elemIndices"
        --     $ nf (List.null . elemIndicesBS 47) haystack) :
        -- (bench "elemIndices (unprocessed)"
        --     $ nf (\ (pat, txt) -> List.null (elemIndicesBS pat txt)) (47, haystack)) :
        (bench "BS.isInfixOf"
            $ nf (BS.isInfixOf needle) haystack) :
        (bench "BS.isInfixOf (unprocessed)"
            $ nf (uncurry BS.isInfixOf) both) :
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
      -- (bench "elemIndices"
      --     $ nf (List.null . elemIndicesBS 58) haystack) :
      (bench "BS.isInfixOf"
          $ nf (BS.isInfixOf needle) haystack) :
      (bench "BS.isInfixOf (unprocessed)"
          $ nf (uncurry BS.isInfixOf) both) :
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
      (bench "BS.isInfixOf"
          $ nf (BS.isInfixOf needle) haystack) :
      (bench "BS.isInfixOf (unprocesed)"
          $ nf (uncurry BS.isInfixOf) both) :
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
      !haystack = case replicate 100 "aaaabacaadaabeaaaaabaacaad" of TextBS bs -> bs
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
      !needle = case replicate 2 "aaaabacaadaabeaa" of TextBS bs -> bs
      !haystack = case replicate 100 "aaaabacaadaabeaaaaabaacaaaaaaabacaadaabeaaaaabaacaad" of TextBS bs -> bs
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
    --   !needle = case replicate 2 "aaaabacaadaabeaaaaabaacaad" of TextBS bs -> bs
    --   !haystack = case replicate 100 "aaaabacaadaabeaaaaabaacaaaaaaabacaadaabeaaaaabaacaad" of TextBS bs -> bs
    --   !both = (needle, haystack)
    -- in bgroup "count long repetitive needle;long repetitive haystack" $
    --   (bench "indicesBrutal"
    --       $ nf (List.length . indicesBrutalBS needle) haystack) :
    --   (bench "indicesBrutal (unprocesed)"
    --       $ nf (\ (pat, txt) -> List.length (indicesBrutalBS pat txt)) both) :
    --   (bench "indicesTwoWay"
    --       $ nf (List.length . indicesTwoWayBS needle) haystack) :
    --   (bench "indicesTwoWay (unprocesed)"
    --       $ nf (\ (pat, txt) -> List.length (indicesTwoWayBS pat txt)) both) :
    --   (bench "indices"
    --       $ nf (List.length . indicesBS needle) haystack) :
    --   (bench "indices (unprocesed)"
    --       $ nf (\ (pat, txt) -> List.length (indicesBS pat txt)) both) :
    -- []) :

    []
