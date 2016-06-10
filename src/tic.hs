import Data.List (maximumBy, minimumBy)
import Data.Ord (comparing)

data Symbol = X | O | Empty deriving (Show, Eq)

show' :: Symbol -> String
show' Empty = " "
show' other = show other

type Board = [Symbol]

board :: Board
board = replicate 9 Empty 

show'' :: Board -> String
show'' [] = "+-+-+-+\n"
show'' b = show''[] ++ (concat $ map (("|" ++) . show') $ take 3 b) ++ "|\n" ++ (show'' $ drop 3 b)

insertAt :: Symbol -> Board -> Int -> Board
insertAt s b 0 = s : (tail b)
insertAt s (x:xs) p = x : (insertAt s xs (p-1))

wonBy :: Symbol -> Board -> Bool 
wonBy s [a0,a1,a2,b0,b1,b2,c0,c1,c2] = checkLines || checkColumns  || checkDiags
                                       where checkLines = (test [a0,a1,a2]) || (test [b0,b1,b2]) || (test [c0,c1,c2])
                                             checkColumns = (test [a0,b0,c0]) || (test [a1,b1,c1]) || (test [a2,b2,c2])
                                             checkDiags = (test [a0,b1,c2]) || (test [a2,b1,c0])  
                                             test = all (==s)

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
                          | otherwise = if null boards then (0,board) else bestOf boards
                                        where 
                                          boards = possibleBoards turn board
                                          scores = map (fst . (miniMax player (opponent turn))) boards
                                          minOrMaxBy = if player == turn then maximumBy else minimumBy
                                          bestOf = minOrMaxBy (comparing fst) . zip scores 
                                                
bestMove :: Symbol -> Board -> Board
bestMove s b = snd (miniMax s s b)

play 0 _ _ = putStrLn ""
play n s b = do
               putStrLn $ show'' nextBoard
               play (n-1) (opponent s) nextBoard
             where nextBoard = bestMove s b

main = play 9 X board
