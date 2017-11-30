-- Maze generator in Haskell original created by Joe Wingbermuehle and Modified by Gabriel Santos
module Main where
    
    import Control.Monad
    import Control.Monad.State
    import Control.Monad.Reader
    import Data.Map
    import System.Random
    import Graphics.Gloss.Interface.Pure.Game
    
    background :: Color
    background = white

    fps :: Int
    fps = 60

    data ElementType = Space | Wall | Marked | Visited | Door | Person | Food | Hall
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
    
    -- Generate a random maze.
    generate :: Int -> Int -> Int -> MazeData
    generate w h s = execState (runReaderT carve (2, 2)) (initMaze w h s)

    canWalk :: Int -> Int -> MazeData -> Bool
    canWalk x y md 
            | (findWithDefault Wall (x, y) (maze md)) == Space && x < 38 && y < 22 && y > 0 = True
            | (findWithDefault Wall (x, y) (maze md)) == Door && x < 38 && y < 22 && y > 0 = True
            | otherwise = False

    finishedMaze :: MazeData -> Bool
    finishedMaze md
            | ((playerX md) == (targetX md)) && ((playerY md) == (targetY md)) = True
            | otherwise = False
        

    handleKeys :: Event -> MazeData -> MazeData
    handleKeys (EventKey (Char 'w') keystate _ _) md = do
        let newY = (playerY md) - 1
        if((keystate == Down) && (canWalk (playerX md) newY md)) then
            md {playerY = newY}
        else
            md
    
    handleKeys (EventKey (Char 's') keystate _ _) md
        | keystate == Down && (finishedMaze md) = generate 39 23 start
        | keystate == Down && (canWalk (playerX md) newY md) = md{playerY = newY, gen = g'}
        | otherwise = md
            where newY = (playerY md) + 1
                  (start, g') = randomR (0, 100) $ gen md
        
    
    handleKeys (EventKey (Char 'd') keystate _ _) md = do
        let newX = (playerX md) + 1
        if((keystate == Down) && (canWalk newX (playerY md) md)) then
            md {playerX = newX}
        else
            md

    handleKeys (EventKey (Char 'a') keystate _ _) md = do
        let newX = (playerX md) - 1
        if((keystate == Down) && (canWalk newX (playerY md) md)) then
            md {playerX = newX}
        else
            md
    
    handleKeys (EventKey (SpecialKey KeyUp) keystate _ _) md = do
        let newY = (playerY md) - 1
        if((keystate == Down) && (canWalk (playerX md) newY md)) then
            md {playerY = newY}
        else
            md
    
    handleKeys (EventKey (SpecialKey KeyDown) keystate _ _) md
        | keystate == Down && (finishedMaze md) = generate 39 23 start
        | keystate == Down && (canWalk (playerX md) newY md) = md{playerY = newY, gen = g'}
        | otherwise = md
            where newY = (playerY md) + 1
                  (start, g') = randomR (0, 100) $ gen md
        
    
    handleKeys (EventKey (SpecialKey KeyRight) keystate _ _) md = do
        let newX = (playerX md) + 1
        if((keystate == Down) && (canWalk newX (playerY md) md)) then
            md {playerX = newX}
        else
            md

    handleKeys (EventKey (SpecialKey KeyLeft) keystate _ _) md = do
        let newX = (playerX md) - 1
        if((keystate == Down) && (canWalk newX (playerY md) md)) then
            md {playerX = newX}
        else
            md

    handleKeys _ md = md
    
    myUpdate :: Float -> MazeData -> MazeData
    myUpdate _ md = md

    render :: MazeData -> Picture
    render md = drawing 0 0 md

    drawElement :: ElementType -> Float -> Float -> Picture
    drawElement e x y
        | e == Space || e == Door || e == Visited = translate ((x * 20) -390) ((y * (-20)) + 230) $ color background $ rectangleSolid 20 20
        | e == Wall = translate ((x * 20) -390) ((y * (-20)) + 230) $ color black $ rectangleSolid 20 20
        | e == Person = translate ((x * 20) -390) ((y * (-20)) + 230) $ color blue $ circleSolid 8 
        | e == Food = translate ((x * 20) -390) ((y * (-20)) + 230) $ color red $ thickCircle 8 8
        | e == Hall = pictures [translate 310 (-350) $ color red $ rectangleSolid 20 300, translate 350 (-350) $ color red $ rectangleSolid 20 300]
        | otherwise = translate ((x * 20) -390) ((y * (-20)) + 230) $ color blue $ rectangleSolid 20 20    

    drawing :: Int -> Int -> MazeData -> Picture
    drawing x y md
        | (x, y) == ((playerX md), (playerY md)) = pictures [drawElement Person (fromIntegral x) (fromIntegral y), drawing (x + 1) y md] 
        | (x, y) == ((targetX md), (targetY md)) = pictures [drawElement Food (fromIntegral x) (fromIntegral y), drawing (x + 1) y md] 
        | x < 38 = pictures [drawElement (findWithDefault Wall (x, y) (maze md)) (fromIntegral x) (fromIntegral y), drawing (x + 1) y md]
        | x == 38 && y < 22 = pictures [drawElement (findWithDefault Wall (x, y) (maze md)) (fromIntegral x) (fromIntegral y), drawing 0 (y + 1) md]
        | finishedMaze md = pictures [drawElement Hall 0 0]
        | otherwise = Blank
        
    main :: IO ()
    main = do
        let maze = generate 39 23 5
        play FullScreen background fps maze render handleKeys myUpdate