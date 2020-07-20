import Data.Char
import System.IO.Error
import Control.Exception

data Color = Red | Black deriving (Eq, Show)
data Suit = Clubs | Diamonds | Hearts | Spades deriving (Eq, Show)
data Rank = Num Int | Jack | Queen | King | Ace deriving (Eq, Show)
data Card = Card { suit :: Suit, rank :: Rank } deriving (Eq, Show)
data Move = Draw | Discard Card deriving (Eq, Show)

{-Q1: This function returns color of the card according to suit of the card. I have designed
using guards. If suit of the card is Clubs or Spades it returns Black. If suit the card is Diamonds or Hearts it returns red.-}
cardColor :: Card -> Color
cardColor crd 
    | ((suit crd == Clubs) || (suit crd == Spades)) = Black
    | ((suit crd == Diamonds) || (suit crd == Hearts)) = Red

{-Q2: This function returns value of the card according to rank of the card. I have designed
using guards. If rank of the card is Ace it returns 11 otherwise it returns 10. If rank
is number it returns numbers itself.
-}
cardValue :: Card -> Int
cardValue crd 
    | (rank crd == Ace) = 11
    | (rank crd == King) = 10
    | (rank crd == Queen) = 10
    | (rank crd == Jack) = 10
    | otherwise = cardValue' (rank crd)
    where
        cardValue' :: Rank -> Int
        cardValue' (Num i) = i

{-Q3: This function removes the given card from the given list. Then returns new list. I have designed
using guards. If the element is in the list(First case -> Exp: initial list not equals new list) it returns
updated list. Otherwise it returns empty list to show error end of the program.
-}
removeCard :: [Card] -> Card -> [Card]
removeCard cs c
    -- If the element is in the list
    | ((filter (\x -> x/=c) cs) /= cs) = (filter (\x -> x/=c) cs)
    -- If the element is not in the list
    | otherwise = []

{-Q4: This function returns whether cards of the given card list have same color. I have designed
using guards. If list has less than 1 element it returns True, otherwise recursively call function to
control colors of all elements.-}
allSameColor :: [Card] -> Bool
allSameColor (c:cs) 
    | length(c:cs) <= 1 = True -- Final Iteration
    | otherwise = (cardColor c == cardColor (head cs)) && allSameColor cs

{-Q5: This function returns sum of values of the given card list. I have create one helper
function 'sumOfRanks'. 'sumOfRanks' function calculates total value using tail recursive approach.
-}    
sumCards::[Card] -> Int
sumCards cs = sumOfRanks cs 0
    where
        sumOfRanks :: [Card] -> Int -> Int
        sumOfRanks [] total = total
        sumOfRanks cardList total
            | length cardList == 0 = total -- Last Iteration
            | otherwise = sumOfRanks (tail cardList) ( total + (cardValue (head cardList))) 

{-Q6: This function returns score of the game according to goal and values of held-cards.
I have used guards and two helper functions. If there is no held_card it returns -1.
Why there is no held cards because one of the moves of player asked to remove the card she/he does not have.
Then removecard function returns empty list and score returns -1 to notify main program.
 
'calculateRealScore' helper function controls whether held cards have same colors. If they
have same colors it divides preliminary score by 2 else it returns preliminary score. This is real score.

'sumOfTheValues'' helper function add values of the cards. It iterates using tail recursive approach.

Totalscore is calculated according to given cases. 
-}
score :: [Card] -> Int -> Int
score [] _ = -1
score crdList goal
    | sumOfTheValues > goal = -- If sum greater than goal
        let preliminary_score = 3*(sumOfTheValues-goal)
        in calculateRealScore crdList preliminary_score
    | otherwise = 
        let preliminary_score = (goal - sumOfTheValues)
        in calculateRealScore crdList preliminary_score
    where
        sumOfTheValues = sumCards crdList 
        calculateRealScore :: [Card] -> Int -> Int
        calculateRealScore crdList preliminary_score
            | not (allSameColor crdList) = preliminary_score
            -- If all the cards have same color, divide prelimnary score by 2.
            | otherwise = preliminary_score `div` 2

{-Q7: This types represents state of the game.
If the state of the game is 'Start' -> Game continues.
else if the state of the game is 'End' -> Game overs.
else if the state of the game is 'WrongCard' -> Player asked to remove the card he/she does not have.
-}           
data State = Start | End | WrongCard deriving (Eq, Show)

{-Q8: This function gets heldcardLists, movelist and goal. Then returns score of the game.
I have used one helper function. 'runGame'' function gets state, cardList, heldcardLists and movelist. It returns heldCardList.
If the state of the game is wrongCard it returns empty list to notify main program about error.
If the state of the game is End it returns heldCards to calculate score of the game.
If the current move is Draw and there is no card in the card list, the function overs the game to calling itself with 'End' status.
If the current move is Draw and there are cards in the card list, the head of the card list is added to held card and the function calls itself with updatedcardlists and updated movelists.
If the game are not okey for above conditions, it means the next move of the player is Discard.
If the card that will be removed is not in the card list, the function calls itself with 'wrongCard' status. And game overs.
If the card that will be removed is in the card list, the function removes card and, calls itself with updatedcard lists and updated movelists.
-}
runGame :: [Card] -> [Move] -> Int -> Int
runGame cardList moveList goal =
    let heldCards = []
    in score (runGame' Start heldCards cardList moveList) goal
    where
        runGame':: State -> [Card] -> [Card] -> [Move] -> [Card]
        runGame' WrongCard _ _ _ = [] -- Wrongcard error
        runGame' End held_cards _ _ = held_cards  -- End
        runGame' _ held_cards _ [] = held_cards -- There is no move
        runGame' Start held_cards cardList moveList
            -- End Condition 
            | ((sumCards held_cards) > goal) = runGame' End held_cards cardList moveList
            -- If Move is Draw and there is no enough card.
            | (head moveList == Draw && (length cardList) == 0) = runGame' End held_cards cardList moveList -- If 
            -- If Move is Draw, add first card of cardList to held cards
            | (head moveList == Draw) =
                let l = head cardList
                in runGame' Start (held_cards++[l]) (tail cardList) (tail moveList)
            -- One of two conditions below are for case of Discard move.
            | length (removeCard held_cards (discardCard (head moveList))) == 0 =
                let l = discardCard (head moveList)
                in runGame' (WrongCard) (removeCard held_cards l) (cardList) (tail moveList)
            | otherwise =
                let l = discardCard (head moveList)
                in runGame' (Start) (removeCard held_cards l) (cardList) (tail moveList)
            where
                discardCard :: Move -> Card
                discardCard (Discard crd) = crd
                    

{-Q9: This function gets a char and returns Suit according to that char.
The char should be first letter of one of the suits. Otherwise it raises error.
-}
convertSuit :: Char -> Suit
convertSuit c
    | c == 'd' || c == 'D' = Diamonds
    | c == 'h' || c == 'H' = Hearts
    | c == 'c' || c == 'C' = Clubs
    | c == 's' || c == 'S' = Spades
    | otherwise = error "Invalid Suit"

{-Q10: This function gets a char and returns Rank according to that char.
If the char is '1' it returns Ace, if the car is 't' or 'T' it returns 10.
If the char is 'j','J' or 'q','Q' or 'k','K', it returns 'Jack','Queen','King' according to first letter of ranks.
It the char is digit it returns as Int. Otherwise it raises error.
-}
convertRank :: Char -> Rank
convertRank c
    | c == '1' = Ace
    | c == 't' || c == 'T' = Num 10
    | c == 'J' || c == 'j' = Jack
    | c == 'Q' || c == 'q' = Queen
    | c == 'K' || c == 'k' = King
    | isDigit c = Num (digitToInt c)
    | otherwise = error "Invalid Rank"

{-Q11: This function gets two char(suitName,rankName) and returns new card.
This cards is created converting rankName to rank and converting suitName to suit. 
-}
convertCard :: Char -> Char -> Card
convertCard suitName rankName = Card {rank = convertRank rankName, suit = convertSuit suitName}

{-Q12: This function read cards from the player and saves them to list. 
I have used one helper function. 'readCards'' helper function getline from the user.
If length of the line is 2, that means player will enter a information about card.
First letter - suitName, second letter - rankName.
Card is created according to these. The card is added to list and the function makes call to the functions itself to get new input from the player.
If length of the line is 1, the function controls whether it is '.'.
If it is '.' the function returns cardList. Otherwise the function asked to enter again-}

readCards :: IO([Card])
readCards = readCards' []
    where
        readCards':: [Card]-> IO([Card])
        readCards' cardList = do
            input <- getLine
            case (length input) == 2 of
                True -> do
                    let s = head input -- Suit
                        r = head (tail input) -- Rank
                        new_card = (convertCard s r) 
                    readCards' (cardList ++ [new_card]) -- Add Card to CardList
                False -> do
                    case (input == ".") of
                        True -> do
                            return(cardList) -- Finish reading cards
                        False -> do
                            putStrLn("Invalid Card, Enter Again")
                            readCards' cardList

{-Q13: This function gets moveName,suitName and rankName as chars. Then returns move.
if the type of move(moveName) is 'd' or 'D' it returns Draw.
if the type of move(moveName) is 'r' or 'R' it returns Discard with creating new card.
-}
convertMove :: Char -> Char -> Char -> Move
convertMove moveName suitName rankName 
    | (moveName == 'd') || (moveName == 'D') = Draw
    | (moveName == 'r') || (moveName == 'R') = Discard (convertCard suitName rankName)

{-Q14: This function read moves from the player and saves them to list. 
I have used one helper function. 'readMoves'' helper function getline from the user.
If length of the line is 3, that means player will enter a information about card.
First letter - moveName, second letter - suitName, third letter rankName.
Move is created according to these. The move is added to list and the function makes call to the functions itself to get new input from the player.
If length of the line is 1, the function controls whether it is . or 'd','D'.
If it is '.' the function returns moveList. If it is 'd' or 'D' it returns new Move(Draw) and make call to the functions itself to get new input from the player.  -}
readMoves :: IO([Move])
readMoves = readMoves' []
    where
        readMoves':: [Move]-> IO([Move])
        readMoves' moveList = do
            input <- getLine
            case (length input) == 3 of
                True -> do
                    let m = head input -- Move
                        s = head(tail input) -- Suit
                        r = head(tail (tail input)) -- Rank
                        new_move = (convertMove m s r)
                    readMoves' (moveList ++ [new_move]) -- Add Move to MoveList
                False -> do
                    case ((input == "d") || (input == "D")) of
                        True -> do
                            let new_move = convertMove 'd' 'd' 'd' -- Add Draw
                            readMoves'(moveList ++ [new_move]) -- Add Move to MoveList
                        False -> do
                            case (input == ".") of
                                True -> do
                                    return(moveList) -- Finish reading cards
                                False -> do
                                    putStrLn("Invalid Card, Enter Again")
                                    readMoves' moveList
{-Q15: This function main function, readCards,moveCards, functions are called. Then
Goal is gotten as an input. Then game starts, the result of the runGame function is score.
If score is -1, player asked to remove tha card that she/he does not have. So one of the moves are wrong.
If score is another thing, it returns the score of the play.
-}
main = do 
    putStrLn "Enter cards:"
    cards <- readCards
    --putStrLn (show cards)

    putStrLn "Enter moves:"
    moves <- readMoves
    --putStrLn (show moves)

    putStrLn "Enter goal:"
    line <- getLine

    let goal = read line :: Int
    let score  = runGame cards moves goal 
    case score == -1 of
        True -> error ("part2: card not in list")
        False -> putStrLn ("Score: " ++ show score)  
