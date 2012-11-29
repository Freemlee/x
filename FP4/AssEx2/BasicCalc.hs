-- BasicCalc.hs
-- John O'Donnell

{- A basic calculator.  To compiler and run, enter these commands:
      ghc --make BasicCalc
     ./BasicCalc
 -}

module Main where

import Graphics.UI.Gtk
import Graphics.UI.Gtk.Glade
import Control.Monad
import Control.Concurrent
import Data.IORef
import Data.Maybe
import System.IO.Unsafe

{- This data structure holds the state of the system. -}

data CalcState = CalcState
  { displayString :: String  -- what's displayed at the top
  , stack :: [Double]        -- stack of numbers
  , store :: Double	     -- storage cell
  , dispEntry :: Entry       -- the gtk Entry for the display
  , stackBuf :: TextBuffer   -- the gtk TextBuffer for the stack
  }

{- A single state reference sr :: SR is created, which always points
to the current state of the system. -}

type SR = IORef CalcState

{- Initial state of the calculator.  The dispEntry and stackBuf fields
need to be updated with the actual gtk values, which are created
dynamically when the program is initialising itself.  The error values
given in the initState enable the program to crash with an informative
message if these fields are not initialised properly before they are
used. -}

initState :: CalcState
initState = CalcState
  { displayString = ""
  , stack = []
  , store = 0
  , dispEntry = error "Display entry not set"
  , stackBuf = error "Stack text buffer not set"
  }

{- The main program initialises the widgets and then starts the
GUI. -}

usd2gbp :: Double
usd2gbp = 0.6252
usd2cny :: Double
usd2cny = 6.2274
usd2sgd :: Double
usd2sgd = 1.2228
usd2eur :: Double
usd2eur = 0.7738
usd2usd :: Double
usd2usd = 1.0000

main :: IO ()
main =
  do initGUI
     timeoutAddFull
       (yield >> return True)
       priorityDefaultIdle 50

-- Read in the glade file

     let gladeFilePath = "glade/calculator.glade"
     maybe_xml <- xmlNew gladeFilePath
     let xml = case maybe_xml of
           Just xml_text -> xml_text
           Nothing -> error "cannot open glade xml file"

-- Set up the main window

     mainWindow <- xmlGetWidget xml castToWindow "MainWindow"
     onDestroy mainWindow mainQuit

-- Initialise the state reference

     sr <- newIORef initState

-- Activate Menu: File: Quit

     quitMenuAction <- xmlGetWidget xml castToMenuItem "menuQuit"
     onActivateLeaf quitMenuAction $ do mainQuit

-- Activate Help: About

     aboutDialogue <- xmlGetWidget xml castToAboutDialog "aboutDialogue"
     aboutButton <- xmlGetWidget xml castToMenuItem "menuAbout"
     onActivateLeaf aboutButton $ do widgetShow aboutDialogue

-- Initialise the display entry (the top field for entering numbers)

     displayEntry <- xmlGetWidget xml castToEntry "DisplayEntry"
     s <- readIORef sr
     writeIORef sr (s {dispEntry = displayEntry})

-- Initialise the stack view (the text view at the bottom)

     stackView <- xmlGetWidget xml castToTextView "StackTextView"
     textBufTagTable <- textTagTableNew
     stackTextBuf <- textBufferNew (Just textBufTagTable)
     textViewSetBuffer stackView stackTextBuf
     textBufferSetText stackTextBuf ""
     s <- readIORef sr
     writeIORef sr (s {stackBuf = stackTextBuf})

-- Set up the digit and decimal point buttons

     forM
       [("b0",'0'), ("b1",'1'), ("b2",'2'), ("b3",'3'), ("b4",'4'),
        ("b5",'5'), ("b6",'6'), ("b7",'7'), ("b8",'8'), ("b9",'9'),
        ("bpoint",'.')]
       (\(x,y) -> prepareNumButton sr xml x y)

-- Set up converter button
	{-
	GBP
	CNY
	USD
	EUR
	SGD
	-}

     bConv <- xmlGetWidget xml castToButton "bConv"
     onClicked bConv $ do
	converterWindow <- xmlGetWidget xml castToWindow "convertWindow"
	widgetShow converterWindow

     bConverter <- xmlGetWidget xml castToButton "bConvert"
     onClicked bConverter $ do
	currencyFrom <- xmlGetWidget xml castToComboBox "currency1"
	currencyTo <- xmlGetWidget xml castToComboBox "currency2"
	currencyVal <- xmlGetWidget xml castToEntry "entry1"
	convVal <- xmlGetWidget xml castToEntry "entry2"
	let cur1 = unsafePerformIO $ comboBoxGetActive currencyFrom
	let cur2 = unsafePerformIO $ comboBoxGetActive currencyTo
	let curVal = unsafePerformIO $ entryGetText currencyVal
	let convertedVal = fromUSD (toUSD (read (curVal) :: Double) cur1) cur2
	--putStrLn $ (show convertedVal)
	entrySetText convVal $ show convertedVal

-- Int/Double toggle listeners

     rInt <- xmlGetWidget xml castToRadioButton "rInt"
     rDoub <- xmlGetWidget xml castToRadioButton "rDoub"
     onToggled rInt $ do
	setStack sr []
	return ()
     onToggled rDoub $ do
	setStack sr []
	return ()

-- Set up RND and TRN buttons

     bRound <- xmlGetWidget xml castToButton "bRound"
     onClicked bRound $ do
	s <- readIORef sr
	let (x:xs) = stack s
	setStack sr $ ((read (show (round x))::Double):xs)

     bTrunc <- xmlGetWidget xml castToButton "bTrunc"
     onClicked bTrunc $ do
	s <- readIORef sr
	let (x:xs) = stack s
	setStack sr $ ((read (show (floor x))::Double):xs)

-- Set up the EXCH button

     bExch <- xmlGetWidget xml castToButton "bExch"
     onClicked bExch $ do
	s <- readIORef sr
	let newStack = exchFunc $ stack s
	setStack sr newStack

-- Set up the STO and FET button

     bFet <- xmlGetWidget xml castToButton "bFet"
     onClicked bFet $ do
	s <- readIORef sr
	let x = store s
	setStack sr $ x : (stack s)

     bSto <- xmlGetWidget xml castToButton "bSto"
     onClicked bSto $ do
	s <- readIORef sr
	let (x:_) = stack s
	setStorage sr x

-- Set up the Sin, Cos and Sqrt buttons
     bSin <- xmlGetWidget xml castToButton "bSin"
     bCos <- xmlGetWidget xml castToButton "bCos"
     bSqrt <- xmlGetWidget xml castToButton "bSqrt"

     onClicked bSin $ do
	s <- readIORef sr
	let (x:xs) = stack s
	setStack sr $ (sin x):xs

     onClicked bCos $ do
	s <- readIORef sr
	let (x:xs) = stack s
	setStack sr $ (cos x):xs

     onClicked bSqrt $ do
	s <- readIORef sr
	let (x:xs) = stack s
	setStack sr $ (sqrt x):xs

-- Set up the Enter button

     benter <- xmlGetWidget xml castToButton "benter"
     onClicked benter $ do
       s <- readIORef sr
       setStack sr  ((read (displayString s) :: Double) : stack s)
       setDisplay sr ""

-- Set up +/- button

     bChangeSign <- xmlGetWidget xml castToButton "bChangeSign"
     onClicked bChangeSign $ do
	s <- readIORef sr
	setStack sr $ swapSign(stack s) 

-- Set up the CLR button

     bCLR <- xmlGetWidget xml castToButton "bCLR"
     onClicked bCLR $ do
	setStack sr []
	setDisplay sr ""
	
       
-- Set up the CE button

     bCE <- xmlGetWidget xml castToButton "bCE"
     onClicked bCE $ do
       setDisplay sr ""



-- Set up the operator buttons

     prepareBinopButton sr xml "bAdd" (+)
     prepareBinopButton sr xml "bSub" (-)
     prepareBinopButton sr xml "bMul" (*)
     prepareBinopButton sr xml "bDiv" (/)
     prepareUnopButton sr xml "bReciprocal" (1/)

-- Start up the GUI
     
     widgetShowAll mainWindow
     mainGUI

{- Set the stack to xs.  The new stack is shown in the text view on
the GUI, and is also printed to the console. -}

setStack :: SR -> [Double] -> IO ()
setStack sr xs =
  do s <- readIORef sr
     let str = show xs
     textBufferSetText (stackBuf s) str
     putStrLn ("Stack: " ++ str)
     writeIORef sr (s {stack = xs})

{- Set Storage Cell -}

setStorage :: SR -> Double -> IO ()
setStorage sr x =
  do s <- readIORef sr
     writeIORef sr (s {store = x})

{- Set the display to xs.  This is set in the GUI, and also printed on
the console. -}

setDisplay :: SR -> String -> IO ()
setDisplay sr xs =
  do s <- readIORef sr
     entrySetText (dispEntry s) xs
     writeIORef sr (s {displayString = xs})
     putStrLn xs

{- This function takes several parameters needed to describe an
operator with two operands, such as + or *, and it sets up the
button. -}

prepareBinopButton
  :: SR -> GladeXML -> String -> (Double -> Double -> Double) -> IO ()
prepareBinopButton sr xml bname f =
  do button <- xmlGetWidget xml castToButton bname
     onClicked button $ do
       s <- readIORef sr
       case stack s of
         x:y:stack' ->
           do let r = f x y
              setStack sr (r:stack')
              setDisplay sr (show r)
         _ -> return ()
     return ()

{- This function is similar to prepareBinopButton, but it's for
operators that take only one argument. -}

prepareUnopButton
  :: SR -> GladeXML -> String -> (Double -> Double) -> IO ()
prepareUnopButton sr xml bname f =
  do button <- xmlGetWidget xml castToButton bname
     onClicked button $ do
       s <- readIORef sr
       case stack s of
         x:stack' ->
           do let r = f x
              setStack sr (r:stack')
              setDisplay sr (show r)
         _ -> return ()
     return ()

{- This function sets up a button that is used to enter data into the
display, in particular digits and the decimal point. -}

prepareNumButton :: SR -> GladeXML -> String -> Char -> IO ()
prepareNumButton sr xml bname bchar =
  do button <- xmlGetWidget xml castToButton bname
     onClicked button $ do
       s <- readIORef sr
       let newstr = displayString s ++ [bchar]
       setDisplay sr newstr
     return ()

swapSign :: [Double] -> [Double]
swapSign (x:xs) =
	(-x:xs)

exchFunc :: [Double] -> [Double]
exchFunc [] = []
exchFunc (x:[]) =
	(x:[])
exchFunc (x:y:ys) =
	(y:x:ys)

toUSD :: Double -> Int -> Double
toUSD x 0 = (1/usd2gbp)*x 							
toUSD x 1 = (1/usd2cny)*x
toUSD x 2 = (1/usd2usd)*x
toUSD x 3 = (1/usd2eur)*x
toUSD x 4 = (1/usd2sgd)*x

fromUSD :: Double -> Int -> Double
fromUSD x 0 = (usd2gbp)*x 							
fromUSD x 1 = (usd2cny)*x
fromUSD x 2 = (usd2usd)*x
fromUSD x 3 = (usd2eur)*x
fromUSD x 4 = (usd2sgd)*x
