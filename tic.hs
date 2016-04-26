import Data.List
import Data.Ord

data Symbol = O | X | Empty deriving (Show, Eq)

show' :: Symbol -> String
show' Empty = " "
show' other = show other

type Board = [Symbol]

board :: Board
board = replicate 9 Empty 

print' :: Board -> IO ()
print' [] = putStrLn "+-+-+-+"
print' board = do
                putStrLn "+-+-+-+"
                line $ take 3 board
                print' $ drop 3 board
               where line b = do
                               sequence_ . map putStr $ map ("|" ++) (map show' b) ++ ["|\n"]

insertAt :: Symbol -> Board -> Int -> Board
insertAt s b p = as ++ (s:(tail bs))
                 where (as,bs) = splitAt p b 

checkLines :: Symbol -> Board -> Bool
checkLines _ [] = False
checkLines s b  = (and $ map (== s) (take 3 b)) || checkLines s (drop 3 b)

every :: Int -> [a] -> [a]
every n [] = []
every n b = head b : every n (drop n b)

checkColumns :: Symbol -> Board -> Bool 
checkColumns s b = let column = (every 3 b)
                   in if (length column) < 3 then False else 
                     (and $ map (== s) column) || checkColumns s (tail b)

checkDiags :: Symbol -> Board -> Bool 
checkDiags s [x1,_,x2,_,y,_,z2,_,z1] = check [x1,y,z1] || check [x2,y,z2]
                                           where check = and . map (== s)

wonBy :: Symbol -> Board -> Bool 
wonBy s b = checkLines s b || checkDiags s b || checkColumns s b 

isEmpty :: Board -> Int -> Bool
isEmpty b i = (b !! i) == Empty 

possibleBoards :: Symbol -> Board -> [Board]
possibleBoards s b = map (insertAt s b) $ filter (isEmpty b) [0..8]

opponent :: Symbol -> Symbol
opponent X = O
opponent O = X

miniMax :: Symbol -> Symbol -> Board -> (Integer, Board)
miniMax player turn board
                          | wonBy player board = (10,board)
                          | wonBy (opponent player) board = (-10,board)
                          | otherwise = let boards = possibleBoards turn board
                                        in if null boards then (0,board) else
                                             let scores = map fst $ map (miniMax player (opponent turn)) boards
                                                 minOrMaxBy = if player == turn then maximumBy else minimumBy
                                                 bestOf = minOrMaxBy (comparing fst) . zip scores 
                                             in bestOf boards
                                                
bestMove :: Symbol -> Board -> Board
bestMove s b = snd (miniMax s s b)

play 0 _ b = putStrLn ""
play n s b = let nextBoard = bestMove s b
                 in
                   do
                     print' nextBoard
                     putStrLn ""
                     play (n-1) (opponent s) nextBoard
            
main = play 9 X board

