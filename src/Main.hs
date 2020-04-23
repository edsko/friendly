{-# LANGUAGE TemplateHaskell #-}
module Main where

import Control.Lens
import Data.Char
import Data.Bifunctor
import Data.Monoid
import Options.Applicative

{-------------------------------------------------------------------------------
  Command line options
-------------------------------------------------------------------------------}

data Options = Options {
      maxWidth :: Int
    }

parseOptions :: Parser Options
parseOptions = Options
    <$> (option auto $ mconcat [
            short 'w'
          , long "width"
          , value 80
          , showDefault
          , help "Don't split across multiple lines unless it would be longer than given width"
          ])

getOptions :: IO Options
getOptions = execParser opts
  where
    opts = info (parseOptions <**> helper) $ mconcat [
          fullDesc
        , progDesc "Attempt to pretty-print the input"
        ]

{-------------------------------------------------------------------------------
  Stuff we want to keep track of in all contexts
-------------------------------------------------------------------------------}

class FriendlyContext c where
  initContext :: c
  indentation :: Lens' c Int

incIndentation :: FriendlyContext c => c -> c
incIndentation c = c & indentation %~ (+ 2)

decIndentation :: FriendlyContext c => c -> c
decIndentation c = c & indentation %~ (\x -> x - 2)

indent :: FriendlyContext c => c -> String
indent c = "\n" ++ replicate (c ^. indentation) ' '

remainingWidth :: FriendlyContext c => Options -> c -> Int
remainingWidth Options{..} c = maxWidth - c ^. indentation

{-------------------------------------------------------------------------------
  Pretty-print JSON-like input.

  (Things are set up so that we can have different sorts of inputs; but
  right now this is the only one we actually support.)
-------------------------------------------------------------------------------}

data SemiJsonContext = SJC {
      _sjcIndent :: Int
    }

makeLenses ''SemiJsonContext

instance FriendlyContext SemiJsonContext where
  initContext = SJC {
                    _sjcIndent = 0
                  }
  indentation = sjcIndent

semiJson :: Options -> String -> String
semiJson opts = go initContext
  where
    go :: SemiJsonContext -> String -> String
    go _   []     = []
    go sjc (c:cs)
      | c `elem` scopeOpen
      , Just (closed, rest) <- closeWithin 1 (remainingWidth opts sjc) cs =
          (c : closed) ++ go sjc rest
      | c `elem` scopeOpen =
          let sjc' = incIndentation sjc
          in [c] ++ indent (incIndentation sjc') ++ go sjc' (trimLeft cs)
      | c `elem` scopeClose =
          let sjc' = decIndentation sjc
          in indent sjc' ++ [c] ++ go sjc' cs
      | c == ',' =
          indent sjc ++ [c] ++ go sjc cs
      | otherwise =
          [c] ++ go sjc cs

    scopeOpen, scopeClose :: [Char]
    scopeOpen  = "{(["
    scopeClose = "})]"

    -- @closeWithin n m@ checks that @n@ scopes close within @m@ chars
    closeWithin :: Int -> Int -> String -> Maybe (String, String)
    closeWithin _ _ []     = Just ([], [])
    closeWithin 0 _ xs     = Just ([], xs)
    closeWithin _ 0 (_:_)  = Nothing
    closeWithin n m (c:cs) = first (c :) <$> closeWithin n' (m - 1) cs
      where
        n' | c `elem` scopeOpen  = n + 1
           | c `elem` scopeClose = n - 1
           | otherwise           = n

{-------------------------------------------------------------------------------
  Util
-------------------------------------------------------------------------------}

trimLeft :: String -> String
trimLeft = dropWhile isSpace

{-------------------------------------------------------------------------------
  Main application
-------------------------------------------------------------------------------}

main :: IO ()
main = do
    opts <- getOptions
    interact $ semiJson opts
