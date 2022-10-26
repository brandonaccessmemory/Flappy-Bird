module Main where 
import Graphics.Gloss
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Interface.IO.Game
import System.Random
import Control.Monad.State (State, evalState, get, put, modify, runState, execState)

main :: IO ()
main = do
    bluebird  <- loadBMP "assets/bluebird.bmp"
    wallpaper <- loadBMP "assets/landscape.bmp"
    playBut   <- loadBMP "assets/play.bmp"
    pauseBut  <- loadBMP "assets/pause.bmp"
    gameOver  <- loadBMP "assets/gameover.bmp"
    gameIcon  <- loadBMP "assets/flappybird.bmp"
    pipes     <- loadBMP "assets/pipe.bmp"
    ground    <- loadBMP "assets/ground.bmp"
    zero      <- loadBMP "assets/0.bmp"
    one       <- loadBMP "assets/1.bmp"
    two       <- loadBMP "assets/2.bmp"
    three     <- loadBMP "assets/3.bmp"
    four      <- loadBMP "assets/4.bmp"
    five      <- loadBMP "assets/5.bmp"
    six       <- loadBMP "assets/6.bmp"
    seven     <- loadBMP "assets/7.bmp"
    eight     <- loadBMP "assets/8.bmp"
    nine      <- loadBMP "assets/9.bmp"
    aryabird  <- loadBMP "assets/aryabird.bmp"
    pridebird <- loadBMP "assets/pridebird.bmp"
    nyan      <- loadBMP "assets/nya.bmp"
    tapPic    <- loadBMP "assets/tap.bmp"
    random    <- getStdGen 
    let initialState =  GameState {
            birdPos = 10,
            birdVel = birdSpeed,
            birdRotation = 0,
            birdSkin = 0,
            pipes  = (300, 300),
            pipePos = 600,
            pipeStatus = 0,
            wallpaperPos = 0,
            generator = random,
            score = 0,
            gameMode = Start
        }
    play 
        window 
        background 
        fps 
        initialState 
        (`render` [bluebird, wallpaper, playBut, pauseBut, gameOver, gameIcon, pipes, ground, zero, one, two, three, four, five, six, seven, eight, nine, aryabird, pridebird, nyan, tapPic] )
        inputHandler
        update

-- execState takes in update' ( which updates the game state ) and seconds and returns the final game state
update :: Float -> GameState -> GameState
update seconds = execState (update' seconds)

-- If the game state is not 'InGame', no function will be called hence the game state is not updated which makes the game look like its at a pause
-- If the game state is 'InGame' then execute all the functions that made up the game ( e.g. detecting collision and moving the objects )
update' :: Float -> State GameState ()
update' seconds = do 
    game <- get
    if gameMode game /= InGame 
        then put game
        else do 
            randomPipe
            crashLanding
            pipeCollision
            moveBird seconds 
            movePipe seconds 
            moveWallpaper seconds 

background :: Color
background = white

window :: Display 
window = InWindow "Flappy Bird" (600 , 800) (100, 100)

-- Defines all the variables that a game state will consist of
data GameState = GameState {
    birdPos      :: Float,
    -- y position of the bird
    birdVel      :: Float,
    -- y velocity of the bird
    birdRotation :: Float,
    -- rotation of the bird
    birdSkin     :: Int,
    -- skin of the bird
    pipes       :: (Float,Float),
    -- length of the pipes
    pipePos      :: Float,
    -- x position of the bird
    pipeStatus   :: Int,
    -- 0 if pipe is in front of the bird, 1 if it is behind
    wallpaperPos :: Float,
    -- x position of the background
    generator    :: StdGen, 
    -- the seed of the random
    score        :: Int,
    -- score of the game
    gameMode     :: Mode } deriving Show
    -- state of the game
-- deriving Show for error checkings

-- The type of game modes that our game can be in, deriving Eq so that we can perform comparisons on this data type
data Mode = Start | InGame | Skins | Pause | GameOver deriving (Show, Eq)

-- Defines the initial state of the game
initialState :: GameState
initialState = GameState {
    birdPos = 10,
    birdVel = birdSpeed,
    birdRotation = 0,
    birdSkin = 0,
    pipePos = 600,
    pipes = (300, 300),
    pipeStatus = 0,
    wallpaperPos = 0,
    generator = mkStdGen 1,
    score = 0,
    gameMode = Start
}

fps :: Int
fps = 60

birdSpeed :: Float
birdSpeed = 400

{- handles input keys like the space key, the letter 'p' , 'r' , 's' and the left mouse button to set the game mode. 
   For example, pressing the space key in any game mode will set the gameMode as 'InGame' as well as the velocity of the bird. 
   I want to keep the same bird skin that was chosen as well as the seed of the random generator so that the skin of the bird is not reset when we restart the game. 
   In the case of the random generator, If we use the same seed in each initial state then the pipes will be generated in the same way as previous runs of the game.
-}
inputHandler :: Event -> GameState -> GameState
inputHandler (EventKey (SpecialKey KeySpace) Down _  _) game
    | gameMode game == Start    = game { birdVel = birdSpeed, gameMode = InGame }
    | gameMode game == InGame   = game { birdVel = birdSpeed, gameMode = InGame }
    | gameMode game == Pause    = game { birdVel = birdSpeed, gameMode = InGame }
    | gameMode game == Skins    = game { gameMode = Start }
    | gameMode game == GameOver = initialState { birdSkin = birdSkin game, generator = generator game }

inputHandler (EventKey (Char 'p') Down _ _) game
    | gameMode game == Pause    = game { gameMode = InGame }
    | gameMode game == GameOver = initialState { birdSkin = birdSkin game, generator = generator game }
    | otherwise                 = game { gameMode = Pause  }

inputHandler (EventKey (MouseButton LeftButton) Down _ (x,y)) game
    | gameMode game == Skins    = findSkin game (x, y)
    | otherwise                 = game 

inputHandler (EventKey (Char 'r') Down _ _ ) game = initialState { birdSkin = birdSkin game, generator = generator game }

inputHandler (EventKey (Char 's') Down _ _) game
    | gameMode game == Start    = game { gameMode = Skins }
    | gameMode game == Skins    = game { gameMode = Start }
    | gameMode game == GameOver = initialState { birdSkin = birdSkin game, generator = generator game }

inputHandler _ game = game 

-- a function that determines the skin of the bird depending on the x and y position of the mouse, the number assigned to the variable birdSkin is the same as the index of the picture list in render
findSkin :: GameState -> (Float, Float) -> GameState
findSkin game (x,y)
    | x > (-34) && x < 34 && y < 135   && y > 35     = game { birdSkin = 18, gameMode = Start }
    | x > (-34) && x < 34 && y < 35    && y > (-35)  = game { birdSkin =  0, gameMode = Start }
    | x > (-34) && x < 34 && y < (-65) && y > (-135) = game { birdSkin = 19, gameMode = Start }
    | x > (-34) && x < 34 && y < (-165)&& y > (-235) = game { birdSkin = 20, gameMode = Start }
    | otherwise = game

-- Render takes in the current game state and a list of bmps which we loaded in and renders the pictures for our game depending on the game state
render :: GameState -> [Picture] -> Picture
render game imgs =
    case gameMode game of 
        Start    -> pictures [ wallpaper1, wallpaper2, ground1, ground2, playBut , gameIcon, chosenbird, tapPic ]
        InGame   -> pictures [ wallpaper1, wallpaper2, toppipe, botpipe, ground1, ground2, chosenbird, score1, score2 ]
        Pause    -> pictures [ wallpaper1, wallpaper2, chosenbird, toppipe, botpipe, pause, ground1, ground2, score1, score2 ]
        Skins    -> pictures [ wallpaper1, wallpaper2, ground1, ground2, bird, bird1, bird2, bird3, skinIcon, tapPic]
        GameOver -> pictures [ wallpaper1, wallpaper2, toppipe, botpipe , ground1, ground2, chosenbird, gameOver ]
    
    where
        chosenbird = translate 0 (birdPos game) $ rotate (birdRotation game) $ scale 0.08 0.08 $ imgs !! birdSkin game
        bird       = scale 0.08 0.08 $ head imgs
        bird1      = translate 0 100 $ scale 0.08 0.08 $ imgs !! 18
        bird2      = translate 0 (-100) $ scale 0.08 0.08 $ imgs !! 19 
        bird3      = translate 0 (-200) $ scale 0.08 0.08 $ imgs !! 20

        gameIcon   = translate 20 100 $ scale 2 2 $ imgs !! 5
        skinIcon   = translate 20 250 $ scale 2 2 $ imgs !! 5
        playBut    = translate 0 (-70 ) $ scale 0.25 0.25 $ imgs !! 2
        tapPic     = translate 100 0 $ scale 2 2 $ imgs !! 21
        pause      = translate 0 200 $ scale 0.4 0.4 $ imgs !! 3
        gameOver   = imgs !! 4

        wallpaper1 = translate (212 -  wallpaperPos game) 80 $ scale 1 1.25 $ imgs !! 1
        wallpaper2 = translate (1236 - wallpaperPos game) 80 $ scale 1 1.25 $ imgs !! 1

        ground1    = translate ( 212 - wallpaperPos game) (-361) $ scale 1 1.25 $ imgs !! 7
        ground2    = translate (1236 - wallpaperPos game) (-361) $ scale 1 1.25 $ imgs !! 7

        -- translates the position of the top and bottom pipe base on a number that is randomly generated in the randomPipe function
        toppipe    = translate (pipePos game) ( 650 - fst (pipes game)) $ rotate 180  $ scale 0.4 1 $ imgs !! 6
        botpipe    = translate (pipePos game) (-650 + snd (pipes game)) $ scale 0.4 1 $ imgs !! 6

        -- Using `mod` 10 results in the last digit of the current score and using `div` 10 resuls in the first digit of the current score
        score1 = translate (-260) 320 $ scale 4 4 $ imgs !! ( 8 + score game `div` 10)
        score2 = translate (-230) 320 $ scale 4 4 $ imgs !! ( 8 + score game `mod` 10)

-- This function takes in seconds ( representing dt ) as an argument which is the change of time between each frame, we then use this variable to apply gravity to the bird while ensuring the movement of the bird is rather smooth
-- To add a little movement to the bird, if the velocity of the bird is greater than 0 then we rotate the bird upwards, otherwise downwards
moveBird :: Float -> State GameState ()
moveBird seconds = do 
    game <- get 
    let y  = birdPos game + birdVel game * seconds 
        vy = birdVel game - 1300 * seconds 
        x  = if vy > 0 then (-350) * seconds else 550 * seconds
    put game {birdPos = y, birdVel = vy, birdRotation = x }

-- This function is similar to moveBird where we move the pipes base on seconds passed, most of the numbers used for the position of objects in the game were considered after performing some pixel calculations
-- When the pipe passes a certain position then we increase the score of the game by 1
movePipe:: Float -> State GameState ()
movePipe seconds = do
    game <- get
    let x = if pipePos game < (-40) then 1              else 0
        y = if pipePos game ==  24  then score game + 1 else score game
    put game { pipePos = pipePos game - (180 + speedUpPipe game) * seconds, pipeStatus = x , score = y }

-- This function is similar to the above functions where we move the background of the game base on seconds passed
moveWallpaper :: Float -> State GameState ()
moveWallpaper seconds = do  
    game <- get 
    let x' = if wallpaperPos game == 1024 then 0 else wallpaperPos game + 2
    put game { wallpaperPos = x'}

-- The main way to detect pipe collisions is to consider the height of the bird ( denoted by the variable birdPos ) and comparing this with the length of the pipe
-- Pipe status = 0 means the pipe is in front of the bird, this is to prevent the game detecting collisions despite that the pipe has already passed the bird
pipeCollision :: State GameState()
pipeCollision = do
    game <- get 
    if (pipePos game < 83 && pipeStatus game == 0) && (birdPos game < snd (pipes game) - 350 || birdPos game > 350 - fst (pipes game)) 
        then put game { gameMode = GameOver } 
        else put game

-- Takes in the game state and returns a float, this float is added to the movement speed of the pipe which essentially speeds up the pipe to increase difficulty 
speedUpPipe :: GameState -> Float
speedUpPipe game = if score game > 10 then 75 else 0    

-- A random generator that generates the length of both pipes, only generates a new number after the previous pipe has passed the screen  
randomPipe :: State GameState ()
randomPipe = do 
    game <- get 
    let ( i, gen' ) = if pipePos game < (-325) then randomR (100,550) (generator game) else (fst (pipes game), generator game) 
        x           = if pipePos game < (-325) then 330                                else pipePos game
    put game { generator = gen', pipes = ( i , 550 - i ), pipePos = x }

-- Detects if the bird has touch the floor or reach the top of the screen which results to game over
crashLanding :: State GameState ()
crashLanding = do 
    game <- get 
    if birdPos game < (-289) || birdPos game > 375 then put game { gameMode = GameOver } else put game
