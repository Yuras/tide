{-# LANGUAGE OverloadedStrings #-}

module Main
( main
)
where

import qualified TextBuffer
import qualified TokenBuffer
import qualified HaskellLex
import qualified Lex
import Span

import Prelude hiding (lex, span)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Data.IORef
import Control.Monad
import Control.Exception
import Control.Concurrent
import System.Timeout (timeout)
import Graphics.UI.Gtk

main :: IO ()
main = do
  void $ initGUI

  window <- windowNew
  windowSetDefaultSize window 600 480
  void $ on window objectDestroy
    mainQuit

  scrolledWindow <- scrolledWindowNew Nothing Nothing
  textView <- textViewNew

  fontDescriptionFromString ("monospace 12" :: Text)
    >>= widgetModifyFont textView . Just

  set scrolledWindow [containerChild := textView]
  set window [containerChild := scrolledWindow]

  lexRef <- newIORef (Lex.empty HaskellLex.initialState HaskellLex.next)

  buffer <- textViewGetBuffer textView

  do
    tagTable <- textBufferGetTagTable buffer

    reservedIdTag <- textTagNew (Just "reservedid")
    set reservedIdTag [textTagForeground := ("green" :: Text)]
    textTagTableAdd tagTable reservedIdTag

    reservedOpTag <- textTagNew (Just "reservedop")
    set reservedOpTag [textTagForeground := ("#FFCC00" :: Text)]
    textTagTableAdd tagTable reservedOpTag

    specialTag <- textTagNew (Just "special")
    set specialTag [textTagForeground := ("#FFCC00" :: Text)]
    textTagTableAdd tagTable specialTag

    varSymTag <- textTagNew (Just "varsym")
    set varSymTag [textTagForeground := ("#FFCC00" :: Text)]
    textTagTableAdd tagTable varSymTag

    litTag <- textTagNew (Just "lit")
    set litTag [textTagForeground := ("red" :: Text)]
    textTagTableAdd tagTable litTag

    commentTag <- textTagNew (Just "comment")
    set commentTag [textTagForeground := ("gray" :: Text)]
    textTagTableAdd tagTable commentTag

  updateVar <- newEmptyMVar

  let dumpLex = do
        do
          lex <- readIORef lexRef
          let ltxt = Text.length (TextBuffer.toText (Lex.textBuffer lex))
          let ltok = TokenBuffer.chars (TokenBuffer.measure (Lex.tokenBuffer lex))
          print (ltxt, ltok, Lex.isDirty lex)

        when False $ do
          lex <- readIORef lexRef
          Text.putStrLn (TextBuffer.toText $ Lex.textBuffer lex)
          print (TokenBuffer.toList $ Lex.tokenBuffer lex)
          print (map (fmap Lex.token) $ TokenBuffer.toList $ Lex.tokenBuffer lex)

      edit span txt = do
        lex <- readIORef lexRef
        let (lex', span') = Lex.replace lex span txt
        writeIORef lexRef lex'

        clear span'

      step = do 
        lex <- readIORef lexRef
        when (Lex.isDirty lex) $ do
          let (lex', span) = Lex.step lex
          writeIORef lexRef lex'
          mask_ $ highlight span
          step

      update = do
        void $ timeout (1 * 1000) step
        l <- readIORef lexRef
        when (Lex.isDirty l) $ do
          void $ tryPutMVar updateVar ()

      updateThread = forever $ do
        threadDelay (10 * 1000)
        takeMVar updateVar
        postGUIAsync update

      clear span = do
        iter1 <- textBufferGetIterAtOffset buffer (start span)
        iter2 <- textBufferGetIterAtOffset buffer (start span + count span)
        textBufferRemoveAllTags buffer iter1 iter2

      highlight span = do
        do
          lex <- readIORef lexRef
          print ("HL" :: String, span, Lex.isDirty lex)

        clear span
        lex <- readIORef lexRef
        let (startPos, tokens) = Lex.tokens lex span
        go startPos tokens
        where
        go _ [] = return ()
        go pos (item : rest) = do
          hl pos item
          let pos' = case item of
                Left l -> pos + l
                Right i -> pos + Lex.consumedLength i
          go pos' rest
        hl _ Left{} = return ()
        hl pos (Right i) = do
          let maybe_tag = case Lex.token i of
                HaskellLex.ReservedId{} -> Just ("reservedid" :: Text)
                HaskellLex.ReservedOp{} -> Just "reservedop"
                HaskellLex.Special{} -> Just "special"
                HaskellLex.VarSym{} -> Just "varsym"
                HaskellLex.Lit{} -> Just "lit"
                HaskellLex.White (HaskellLex.Comment _) -> Just "comment"
                HaskellLex.White (HaskellLex.CommentStart _) -> Just "comment"
                HaskellLex.White (HaskellLex.CommentEnd _) -> Just "comment"
                _ -> Nothing
          case maybe_tag of
            Nothing -> return ()
            Just tag -> do
              let pos' = pos + Lex.consumedLength i
              iter1 <- textBufferGetIterAtOffset buffer pos
              iter2 <- textBufferGetIterAtOffset buffer pos'
              textBufferApplyTagByName buffer tag iter1 iter2

  void $ on buffer bufferInsertText $ \iter txt -> do
    off <- textIterGetOffset iter
    edit (Span off 0) txt

  void $ after buffer bufferInsertText $ \_ txt -> do
    let _ = txt :: Text
    update
    dumpLex

  void $ on buffer deleteRange $ \iter1 iter2 -> do
    off1 <- textIterGetOffset iter1
    off2 <- textIterGetOffset iter2
    edit (Span off1 (off2 - off1)) Text.empty

  void $ after buffer deleteRange $ \_ _ -> do
    update
    dumpLex

  void $ forkIO updateThread

  widgetShowAll window
  mainGUI
