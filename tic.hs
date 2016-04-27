import Data.List
import Data.Ord

data Symbol = X | O | Empty deriving (Show, Eq)

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
checkLines s [a0,a1,a2,b0,b1,b2,c0,c1,c2] = let test = all (==s)
                                            in (test [a0,a1,a2]) || (test [b0,b1,b2]) || (test [c0,c1,c2])

checkColumns :: Symbol -> Board -> Bool 
checkColumns s [a0,b0,c0,a1,b1,c1,a2,b2,c2] = let test = all (==s)
                                              in (test [a0,a1,a2]) || (test [b0,b1,b2]) || (test [c0,c1,c2])

checkDiags :: Symbol -> Board -> Bool 
checkDiags s [x1,_,x2,_,y,_,z2,_,z1] = check [x1,y,z1] || check [x2,y,z2]
                                           where check = all (== s)

wonBy :: Symbol -> Board -> Bool 
wonBy s b = checkLines s b || checkDiags s b || checkColumns s b 

isEmpty :: Board -> Int -> Bool
isEmpty b i = (b !! i) == Empty 

possibleBoards :: Symbol -> Board -> [Board]
possibleBoards s b = map (insertAt s b) $ filter (isEmpty b) [0..8]

opponent :: Symbol -> Symbol
opponent X = O
opponent O = X

miniMax :: Symbol -> Symbol -> Board -> (Int, Board)
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

play 0 _ _ = putStrLn ""
play n s b = let nextBoard = bestMove s b
                 in
                   do
                     print' nextBoard
                     putStrLn ""
                     play (n-1) (opponent s) nextBoard
            
main = play 9 X board
