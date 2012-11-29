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
import Text.Printf

{- This data structure holds the state of the system. -}

data CalcState = CalcState
  { displayString :: String  		-- what's displayed at the top
  , displayLabel :: String
  , stack :: [(Double,String)]        -- stack of numbers
  , store :: (Double, String)  		-- storage cell
  , dispEntry :: Entry       		-- the gtk Entry for the display
  , dispLabel :: Entry
  , stackBuf :: TextBuffer   		-- the gtk TextBuffer for the stack
  , isInt :: Bool
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
  , displayLabel = ""
  , stack = [(0,"")]
  , store = (0,"")
  , dispEntry = error "Display entry not set"
  , dispLabel = error "Display label not set"
  , stackBuf = error "Stack text buffer not set"
  , isInt = True
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
     widgetModifyBg mainWindow StateNormal $ Color 15000 15000 15000
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
     onDestroy aboutDialogue $ do
	widgetHide aboutDialogue

-- Initialise the display entry (the top field for entering numbers)

     displayEntry <- xmlGetWidget xml castToEntry "DisplayEntry"
     displayLabel <- xmlGetWidget xml castToEntry "DisplayLabel"
     s <- readIORef sr
     writeIORef sr (s {dispEntry = displayEntry, dispLabel = displayLabel})

-- Initialise the stack view (the text view at the bottom)

     stackView <- xmlGetWidget xml castToTextView "StackTextView"
     textBufTagTable <- textTagTableNew
     stackTextBuf <- textBufferNew (Just textBufTagTable)
     textViewSetBuffer stackView stackTextBuf
     textBufferSetText stackTextBuf "\n"
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
     widgetModifyBg bConv StateNormal $ Color 40000 65535 40000
     converterWindow <- xmlGetWidget xml castToWindow "convertWindow"
     onClicked bConv $ do
	widgetShowAll converterWindow
	--Just newXML <- xmlNew "glade/calculator.glade"
	--nw <- xmlGetWidget newXML castToWindow "convertWindow"
	--nmw <- xmlGetWidget newXML castToWindow "MainWindow"
	--widgetShowAll nw
	--widgetHideAll nmw
     onDestroy converterWindow $ do
	widgetHideAll converterWindow
	{-
	set converterWindow [ widgetVisible := True ]
	windowPresent converterWindow
	set converterWindow [ widgetVisible := False ] -}

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
	entrySetText convVal $ printf "%.2f" convertedVal

-- Int/Double toggle listeners

     rInt <- xmlGetWidget xml castToRadioButton "rInt"
     rDoub <- xmlGetWidget xml castToRadioButton "rDoub"
     widgetModifyBase rInt StateNormal $ Color 65000 65000 65000
     widgetModifyBase rDoub StateNormal $ Color 65000 65000 65000
     onToggled rInt $ do
	setStack sr []
	swapMode sr
	bSin <- xmlGetWidget xml castToButton "bSin"
	bCos <- xmlGetWidget xml castToButton "bCos"
	bReciprocal <- xmlGetWidget xml castToButton "bReciprocal"
	bTrunc <- xmlGetWidget xml castToButton "bTrunc"
	bRound <- xmlGetWidget xml castToButton "bRound"
	bpoint <- xmlGetWidget xml castToButton "bpoint"

	widgetHide bSin
	widgetHide bCos
	widgetHide bReciprocal
	widgetHide bTrunc
	widgetHide bRound
	widgetHide bpoint
	s <- readIORef sr
	putStrLn $ show $ isInt s

     onToggled rDoub $ do
	bSin <- xmlGetWidget xml castToButton "bSin"
	bCos <- xmlGetWidget xml castToButton "bCos"
	bReciprocal <- xmlGetWidget xml castToButton "bReciprocal"
	bTrunc <- xmlGetWidget xml castToButton "bTrunc"
	bRound <- xmlGetWidget xml castToButton "bRound"
	bpoint <- xmlGetWidget xml castToButton "bpoint"

	widgetShow bSin
	widgetShow bCos
	widgetShow bReciprocal
	widgetShow bTrunc
	widgetShow bRound
	widgetShow bpoint
     toggleButtonSetActive rDoub True

-- Set up RND and TRN buttons

     bRound <- xmlGetWidget xml castToButton "bRound"
     widgetModifyBg bRound StateNormal $ Color 40000 65535 40000
     onClicked bRound $ do
	s <- readIORef sr
	let ((x,y):xs) = stack s
	setStack sr $ (((read (show (round x))::Double),y):xs)

     bTrunc <- xmlGetWidget xml castToButton "bTrunc"
     widgetModifyBg bTrunc StateNormal $ Color 40000 65535 40000
     onClicked bTrunc $ do
	s <- readIORef sr
	let ((x,y):xs) = stack s
	setStack sr $ (((read (show (floor x))::Double),y):xs)

-- Set up the EXCH button

     bExch <- xmlGetWidget xml castToButton "bExch"
     widgetModifyBg bExch StateNormal $ Color 40000 65535 40000
     onClicked bExch $ do
	s <- readIORef sr
	let newStack = exchFunc $ stack s
	setStack sr newStack

-- Set up the STO and FET button

     bFet <- xmlGetWidget xml castToButton "bFet"
     widgetModifyBg bFet StateNormal $ Color 40000 65535 40000
     onClicked bFet $ do
	s <- readIORef sr
	let x = store s
	setStack sr $ x : (stack s)

     bSto <- xmlGetWidget xml castToButton "bSto"
     widgetModifyBg bSto StateNormal $ Color 40000 65535 40000
     onClicked bSto $ do
	s <- readIORef sr
	let (x:_) = stack s
	setStorage sr x

-- Set up the Sin, Cos and Sqrt buttons
     bSin <- xmlGetWidget xml castToButton "bSin"
     bCos <- xmlGetWidget xml castToButton "bCos"
     bSqrt <- xmlGetWidget xml castToButton "bSqrt"
     widgetModifyBg bSin StateNormal $ Color 40000 65535 40000
     widgetModifyBg bCos StateNormal $ Color 40000 65535 40000
     widgetModifyBg bSqrt StateNormal $ Color 40000 65535 40000

     onClicked bSin $ do
	s <- readIORef sr
	let ((x,y):xs) = stack s
	setStack sr $ ((sin x),y):xs

     onClicked bCos $ do
	s <- readIORef sr
	let ((x,y):xs) = stack s
	setStack sr $ ((cos x),y):xs

     onClicked bSqrt $ do
	s <- readIORef sr
	let ((x,y):xs) = stack s
	setStack sr $ ((sqrt x),y):xs

-- Set up the Enter button

     benter <- xmlGetWidget xml castToButton "benter"
     widgetModifyBg benter StateNormal $ Color 40000 65535 40000
     onClicked benter $ do
	extraText <- xmlGetWidget xml castToEntry "DisplayLabel"
     	let labelText = unsafePerformIO $ entryGetText extraText
        s <- readIORef sr
        setStack sr  (((read (displayString s) :: Double),labelText) : stack s)  --Edit "" for actual message
        setDisplay sr "" ""
       

-- Set up +/- button

     bChangeSign <- xmlGetWidget xml castToButton "bChangeSign"
     widgetModifyBg bChangeSign StateNormal $ Color 65535 40000 40000
     onClicked bChangeSign $ do
	s <- readIORef sr
	setStack sr $ swapSign(stack s) 

-- Set up the CLR button

     bCLR <- xmlGetWidget xml castToButton "bCLR"
     widgetModifyBg bCLR StateNormal $ Color 40000 65535 40000
     onClicked bCLR $ do
	setStack sr []
	setDisplay sr "" ""
	-- setLabel sr ""
	
       
-- Set up the CE button

     bCE <- xmlGetWidget xml castToButton "bCE"
     widgetModifyBg bCE StateNormal $ Color 40000 65535 40000
     onClicked bCE $ do
       setDisplay sr "" ""
       -- setLabel sr ""



-- Set up the operator buttons
     
     prepareBinopButton sr xml "bAdd" (+) "Add"
     prepareBinopButton sr xml "bSub" (-) "Sub"
     prepareBinopButton sr xml "bMul" (*) "Mul"
     prepareBinopButton sr xml "bDiv" (/) "Div"
     prepareUnopButton sr xml "bReciprocal" (1/)

-- Start up the GUI
     
     widgetShowAll mainWindow
     mainGUI

swapMode :: SR -> IO()
swapMode sr =
	do s <- readIORef sr
	   let x = isInt s
	   writeIORef sr (s {isInt = (not x)}) 
	   putStrLn $ show $ not x

{- Set the stack to xs.  The new stack is shown in the text view on
the GUI, and is also printed to the console. -}

setStack :: SR -> [(Double,String)] -> IO ()
setStack sr xs =
  do s <- readIORef sr
     let str = show xs
     textBufferSetText (stackBuf s) (formatBufferText str)
     putStrLn ("Stack: " ++ str)
     writeIORef sr (s {stack = xs})

formatBufferText :: String -> String
formatBufferText (']':_) = []
formatBufferText ('[':xs) = formatBufferText xs
formatBufferText ('(':xs) = formatBufferText xs
formatBufferText (',':'(':xs) = formatBufferText ("\n" ++ xs)
formatBufferText (',':xs) = formatBufferText ("\t" ++ xs)
formatBufferText (x:xs) = x : formatBufferText xs


{- Set Storage Cell -}

setStorage :: SR -> (Double,String) -> IO ()
setStorage sr x =
  do s <- readIORef sr
     writeIORef sr (s {store = x})

{- Set the display to xs.  This is set in the GUI, and also printed on
the console. -}

setDisplay :: SR -> String -> String -> IO ()
setDisplay sr xs lab =
  do s <- readIORef sr
     entrySetText (dispEntry s) xs
     writeIORef sr (s {displayString = xs, displayLabel = lab})
     putStrLn $ xs ++ "\t" ++ lab

setLabel :: SR -> String -> IO ()
setLabel sr xs =
  do s <- readIORef sr
     entrySetText (dispLabel s) xs
     writeIORef sr (s {displayLabel = xs})

{- This function takes several parameters needed to describe an
operator with two operands, such as + or *, and it sets up the
button. -}

prepareBinopButton
  :: SR -> GladeXML -> String -> (Double -> Double -> Double) -> String -> IO ()
prepareBinopButton sr xml bname f op =
  do button <- xmlGetWidget xml castToButton bname
     widgetModifyBg button StateNormal $ Color 40000 40000 65535
     onClicked button $ do
       s <- readIORef sr
       case stack s of
         (x,x'):(y,y'):stack' ->
		if (isInt s)
		    then do let r = read (show (floor (f x y)))::Double
			    setStack sr (((r,"Auto Gen from " ++ op) ):stack')  -- Edit ... later
			    setDisplay sr (intFormat $ show r) "Auto Gen"
			    -- setLabel sr "Auto Gen ..."
		    else do let r = f x y
			    setStack sr (((r,"Auto Gen from " ++ op) ):stack')
			    setDisplay sr (show r) $ "Auto Gen"
			    -- setLabel sr "Auto Gen ..."
         _ -> return ()
     return ()

intFormat :: String -> String
intFormat ('.':_) =
	[]
intFormat (x:xs) =
	x : intFormat xs

{- This function is similar to prepareBinopButton, but it's for
operators that take only one argument. -}

prepareUnopButton
  :: SR -> GladeXML -> String -> (Double -> Double) -> IO ()
prepareUnopButton sr xml bname f =
  do button <- xmlGetWidget xml castToButton bname
     widgetModifyBg button StateNormal $ Color 40000 40000 65535
     onClicked button $ do
       s <- readIORef sr
       case stack s of
         (x,x'):stack' ->
           do let r = f x
	      if (isInt s)
		then do
			setStack sr (((read (show (round r)) :: Double),"Auto Gen"):stack')
			setDisplay sr (show r) "Auto Gen"
			-- setLabel sr "Auto Gen"
		else do
              		setStack sr ((r,"Auto Gen"):stack')
              		setDisplay sr (show (round r)) "Auto Gen"
			-- setLabel sr "Auto Gen"
         _ -> return ()
     return ()

{- This function sets up a button that is used to enter data into the
display, in particular digits and the decimal point. -}

prepareNumButton :: SR -> GladeXML -> String -> Char -> IO ()
prepareNumButton sr xml bname bchar =
  do button <- xmlGetWidget xml castToButton bname
     widgetModifyBg button StateNormal $ Color 65535 40000 40000
     onClicked button $ do
       s <- readIORef sr
       let newstr = displayString s ++ [bchar]
       let labelStr = displayLabel s
       setDisplay sr newstr labelStr						-- MAYBE CHANGE??
       ---- setLabel sr labelStr
     return ()

swapSign :: [(Double,String)] -> [(Double,String)]
swapSign ((x,y):xs) =
	((-x,y):xs)

exchFunc :: [(Double,String)] -> [(Double,String)]
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
	
