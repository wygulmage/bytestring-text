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
        (bench "first index elemIndices (unprocessed)"
            $ nf (\ (pat, txt) -> List.null (elemIndicesBS pat txt)) (47, "/ascii ByteString")) :
        (bench "first index indicesBrutal"
            $ nf (List.null . indicesBrutalBS "/") "/ascii ByteString") :
        (bench "first index indicesBrutal (unprocessed)"
            $ nf (\ (pat, txt) -> List.null (indicesBrutalBS pat txt)) ("/", "/ascii ByteString")) :
        (bench "first index indicesTwoWay"
            $ nf (List.null . indicesTwoWayBS "/") "/ascii ByteString") :
        (bench "first index indicesTwoWay (unprocessed)"
            $ nf (\ (pat, txt) -> List.null (indicesTwoWayBS pat txt)) ("/", "/ascii ByteString")) :
        (bench "first index indices"
            $ nf (List.null . indicesBS "/") "/ascii ByteString") :
        (bench "first index indices (unprocessed)"
            $ nf (\ (pat, txt) -> List.null (indicesBS pat txt)) ("/", "/ascii ByteString")) :

        (bench "index elemIndices"
            $ nf (List.null . elemIndicesBS 58) "some.long.url.co.uk/webstore/foo.asp.net.php#:~:nothingHerePal") :
        (bench "index indicesBrutal"
            $ nf (List.null . indicesBrutalBS ":") "some.long.url.co.uk/webstore/foo.asp.net.php#:~:nothingHerePal") :
        (bench "index indicesBrutal (unprocessed)"
            $ nf (\ (pat, txt) -> List.null (indicesBrutalBS pat txt)) (":", "some.long.url.co.uk/webstore/foo.asp.net.php#:~:nothingHerePal")) :
        (bench "index indicesTwoWay"
            $ nf (List.null . indicesTwoWayBS ":") "some.long.url.co.uk/webstore/foo.asp.net.php#:~:nothingHerePal") :
        (bench "index indicesTwoWay (unprocessed)"
            $ nf (\ (pat, txt) -> List.null (indicesTwoWayBS pat txt)) (":", "some.long.url.co.uk/webstore/foo.asp.net.php#:~:nothingHerePal")) :
        (bench "index indices"
            $ nf (List.null . indicesBS ":") "some.long.url.co.uk/webstore/foo.asp.net.php#:~:nothingHerePal") :
        (bench "index indices (unprocessed)"
            $ nf (\ (pat, txt) -> List.null (indicesBS pat txt)) (":", "some.long.url.co.uk/webstore/foo.asp.net.php#:~:nothingHerePal")) :


        (bench "short repetitive needle indicesBrutal"
            $ nf (List.null . indicesBrutalBS "cccpcpc") "pcppccpucdceedecccpcpccpuputpccccp") :
        (bench "short repetitive needle indicesTwoWay"
            $ nf (List.null . indicesTwoWayBS "cccpcpc") "pcppccpucdceedecccpcpccpuputpccccp") :
        (bench "short repetitive needle indicesTwoWay (unprocesed)"
            $ nf (\ (pat, txt) -> List.null (indicesTwoWayBS pat txt)) ("cccpcpc", "pcppccpucdceedecccpcpccpuputpccccp")) :
        (bench "short repetitive needle indices"
            $ nf (List.null . indicesBS "cccpcpc") "pcppccpucdceedecccpcpccpuputpccccp") :
        (bench "short repetitive needle indices (unprocesed)"
            $ nf (\ (pat, txt) -> List.null (indicesBS pat txt)) ("cccpcpc", "pcppccpucdceedecccpcpccpuputpccccp")) :


        []) :
    []
