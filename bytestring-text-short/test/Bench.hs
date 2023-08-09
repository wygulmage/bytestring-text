{-# LANGUAGE OverloadedStrings
  #-}


import Data.ByteString.Text.Short.Internal.Search

import Test.Tasty.Bench

import qualified Data.List as List

main :: IO ()
main = defaultMain $
    (bgroup "search" $
        (bench "first index elemIndices"
            $ nf (List.null . elemIndicesBS 47) "/ascii ByteString") :
        (bench "first index indicesBrutal"
            $ nf (List.null . indicesBrutalBS "/") "/ascii ByteString") :
        (bench "first index indicesTwoWay"
            $ nf (List.null . indicesTwoWayBS "/") "/ascii ByteString") :

        (bench "index elemIndices"
            $ nf (List.null . elemIndicesBS 58) "some.long.url.co.uk/webstore/foo.asp.net.php#:~:nothingHerePal") :
        (bench "index indicesBrutal"
            $ nf (List.null . indicesBrutalBS ":") "some.long.url.co.uk/webstore/foo.asp.net.php#:~:nothingHerePal") :
        (bench "index indicesTwoWay"
            $ nf (List.null . indicesTwoWayBS ":") "some.long.url.co.uk/webstore/foo.asp.net.php#:~:nothingHerePal") :

        (bench "short repetitive needle indicesBrutal"
            $ nf (List.null . indicesBrutalBS "cccpcpc") "pcppccpucdceedecccpcpccpuputpccccp") :
        (bench "short repetitive needle indicesTwoWay"
            $ nf (List.null . indicesTwoWayBS "cccpcpc") "pcppccpucdceedecccpcpccpuputpccccp") :

        []) :
    []
