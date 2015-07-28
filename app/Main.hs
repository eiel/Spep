module Main where

import Control.Arrow
import Data.Either
import Data.List
import Data.Maybe
import Data.Ord
import Github.Repos
import System.Environment

main :: IO ()
main = getArgs >>= return . head >>= printSpep

printSpep u = spep u >>= mapM_ spepFormat

spepFormat (lang, count) = putStr lang >> putStr " " >> print count

spep :: String -> IO [(String, Int)]
spep = flip userRepos Owner >>> fmap (programmingLanguages >>> rank)

rank :: [String] -> [(String, Int)]
rank = sort >>> group >>> map keyAndSize >>> rankSort
  where
    rankSort = sortBy (flip $ comparing snd)
    keyAndSize = head &&& length

programmingLanguages :: Either Error [Repo] -> [String]
programmingLanguages = either (const []) id >>>
                       map repoLanguage >>>
                       filter isJust >>>
                       map fromJust
