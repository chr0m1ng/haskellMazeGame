-- Maze generator and solver in Haskell
-- Joe Wingbermuehle
-- 20070602 <> 20140815

module Main where
    
    import Control.Monad
    import Control.Monad.State
    import Control.Monad.Reader
    import Data.Map
    import System.Random
    import Graphics.Gloss
    import Graphics.Gloss.Interface.IO.Game
    import Graphics.Gloss.Juicy
    import Data.Maybe

    import Codec.Picture.Repa (readImageRGBA, toByteString, reverseColorChannel)
    import Graphics.Gloss
    
    readPng :: FilePath -> Int -> Int -> IO Picture
    readPng path w h = do
      (Right img) <- readImageRGBA path
      let bs = toByteString $ reverseColorChannel img
      return $ bitmapOfByteString w h (BitmapFormat TopToBottom PxRGBA) bs True

    window :: Display
    window = InWindow "Labirinto" (800, 600) (10, 10)
    
    background :: Color
    background = black

    drawElement :: ElementType -> Float -> Float -> Picture
    drawElement e x y
        | e == Space || e == Door || e == Visited = translate ((x * 20) -390) ((y * (-20)) + 230) $ color black $ rectangleSolid 20 20
        | e == Wall = translate ((x * 20) -390) ((y * (-20)) + 230) $ color green $ rectangleSolid 20 20
        | e == Person = translate ((x * 20) -390) ((y * (-20)) + 230) $ color white $ circle 8 
        | e == Food = translate ((x * 20) -390) ((y * (-20)) + 230) $ color white $ thickCircle 8 8
        | otherwise = translate ((x * 20) -390) ((y * (-20)) + 230) $ color blue $ rectangleSolid 20 20    

    ioPicToPic :: Picture -> IO()
    ioPicToPic p@(Bitmap width height _ _) = 
        display (InWindow "Image Viewer" (width, height) (10, 10))
                white
                p

    drawing :: Int -> Int -> MazeData -> Picture
    drawing x y md
        | (x, y) == ((playerX md), (playerY md)) = pictures [drawElement Person (fromIntegral x) (fromIntegral y), drawing (x + 1) y md] 
        | (x, y) == ((targetX md), (targetY md)) = pictures [drawElement Food (fromIntegral x) (fromIntegral y), drawing (x + 1) y md] 
        | x < 38 = pictures [drawElement (findWithDefault Wall (x, y) (maze md)) (fromIntegral x) (fromIntegral y), drawing (x + 1) y md]
        | x == 38 && y < 22 = pictures [drawElement (findWithDefault Wall (x, y) (maze md)) (fromIntegral x) (fromIntegral y), drawing 0 (y + 1) md]
        | otherwise = Blank

    instance Show MazeData where
        show = showMaze 0 0
            where
                showMaze x y md =
                    if x == (width md) then
                        if y + 1 == (height md) then ""
                        else "\n" ++ (showMaze 0 (y + 1) md)
                    else
                        let e = findWithDefault Wall (x, y) (maze md) in
                        (show e) ++ (showMaze (x + 1) y md)

    data ElementType = Space | Wall | Marked | Visited | Door | Person | Food
        deriving (Eq)
    
    data Direction = DLeft | DRight | DUp | DDown
        deriving (Enum)
    
    data MazeData = MazeData {
        width :: Int,
        height :: Int,
        gen :: StdGen,
        playerX :: Int,
        playerY :: Int,
        targetX :: Int,
        targetY :: Int,
        maze :: Map (Int, Int) ElementType
    }
    
    instance Show ElementType where
        show Space = "  "
        show Wall = "[]"
        show Marked = "++"
        show Visited = show Space
        show Door = show Space
    
    type MazeState a = ReaderT (Int, Int) (State MazeData) a
    
    -- Initialize an uncarved maze.
    initMaze :: Int -> Int -> Int -> MazeData
    initMaze w h s =
        let xs = [0 .. w - 1] in
        let ys = [0 .. h - 1] in
        let top = [(x, 0) | x <- xs] in
        let bottom = [(x, h - 1) | x <- xs] in
        let left = [(0, y) | y <- ys] in
        let right = [(w - 1, y) | y <- ys] in
        let start = [(2, 2)] in
        let spaces = Prelude.foldl (++) [] [top, bottom, left, right, start] in
        let m1 = fromList [(p, Space) | p <- spaces] in
        let doors = [(2, 2), (2, 1), (w - 3, h - 2)] in
        let m2 = fromList [(p, Door) | p <- doors] in
        MazeData {
            width = w, height = h, gen = mkStdGen s, playerX = 2, playerY = 1, targetX = w - 3, targetY = h - 2,
            maze = union m1 m2
        }
    
    -- Get the next position in the specified direction.
    getPosition :: (Int, Int) -> Direction -> (Int, Int)
    getPosition (x, y) DLeft = (x - 1, y)
    getPosition (x, y) DRight = (x + 1, y)
    getPosition (x, y) DUp = (x, y - 1)
    getPosition (x, y) DDown = (x, y + 1)
    
    -- Move in the specified direction.
    move :: ElementType -> Direction -> MazeState (Int, Int)
    move fill d = do
        pos <- ask; st <- get
        let mid = getPosition pos d
        let m = insert mid fill $ maze st
        let next = getPosition mid d
        put $ st { maze = insert next fill m }
        return next
    
    -- Determine if we can carve in the specified direction.
    canMove :: ElementType -> Direction -> MazeState Bool
    canMove match d = do
        pos <- ask; st <- get
        let mid = getPosition pos d
        let e1 = findWithDefault Wall mid $ maze st
        let next = getPosition mid d
        let e2 = findWithDefault Wall next $ maze st
        return $ e1 == match && e2 == match
    
    -- Carve the maze starting at the speciified position.
    carve :: MazeState ()
    carve = do
        st <- get
        let (start, g') = randomR (0, 3) $ gen st
        put $ st { gen = g' }
        mapM_ tryCarve [toEnum ((start + i) `mod` 4) | i <- [0 .. 3]]
        where
            tryCarve d = do
                cm <- canMove Wall d
                when cm (move Space d >>= (\n -> local (const n) carve))
    
    -- Find a path from the current position to the end position.
    solveTo :: (Int, Int) -> MazeState Bool
    solveTo stop = do
        pos <- ask
        if pos == stop then
            return True
        else do
            st <- get; pos <- ask
            put $ st { maze = insert pos Marked (maze st) }
            foldM trySolve False [toEnum i | i <- [0 .. 3]]
        where
            trySolve result d
                | result = return True
                | otherwise = do
                    cm <- canMove Space d
                    if cm then do
                        next <- move Marked d
                        found <- local (const next) $ solveTo stop
                        if found then return True
                        else (move Visited d >> return False)
                    else return False
    
    -- Generate a random maze.
    generate :: Int -> Int -> Int -> MazeData
    generate w h s = execState (runReaderT carve (2, 2)) (initMaze w h s)
    
    -- Solve a maze.
    solve :: MazeData -> MazeData
    solve md =
        let start = (2, 2) in
        let stop = ((width md) - 3, (height md - 3)) in
        execState (runReaderT (solveTo stop) start) md

    canWalk :: Int -> Int -> MazeData -> Bool
    canWalk x y md 
            | (findWithDefault Wall (x, y) (maze md)) == Space && x < 38 && y < 22 && y > 0 = True
            | (findWithDefault Wall (x, y) (maze md)) == Door && x < 38 && y < 22 && y > 0 = True
            | otherwise = False
        
    handleKeys :: Event -> MazeData -> MazeData
    handleKeys (EventKey (Char 'w') _ _ _) md = do
        let newY = (playerY md) - 1
        if(canWalk (playerX md) newY md) then
            md {playerY = newY}
        else
            md
    handleKeys (EventKey (Char 's') _ _ _) md = do
        let newY = (playerY md) + 1
        if(canWalk (playerX md) newY md) then
            md {playerY = newY}
        else
            md
    
    handleKeys (EventKey (Char 'd') _ _ _) md = do
        let newX = (playerX md) + 1
        if(canWalk newX (playerY md) md) then
            md {playerX = newX}
        else
            md

    handleKeys (EventKey (Char 'a') _ _ _) md = do
        let newX = (playerX md) - 1
        if(canWalk newX (playerY md) md) then
            md {playerX = newX}
        else
            md
    
    handleKeys _ md = md

    fps :: Int
    fps = 60
    
    myUpdate :: Float -> MazeData -> MazeData
    myUpdate _ md = md

    render :: MazeData -> Picture
    render md = drawing 0 0 md

    -- Generate and display a solved random maze.
    main :: IO ()
    main = do
        let maze = generate 39 23 5
        let solved = solve maze
        putStrLn $ show maze
        putStrLn $ show solved
        play FullScreen background fps maze render handleKeys myUpdate