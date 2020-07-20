{- README
Until the eighth question, all functions work as desired. My computer was freezing while creating dictionary for these words because the number of words in the given text file was too high. Despite my efforts to try and fix everything, I could not find a solution. And finally I had to give up. Functional programming has been great, but this question has aged me a lot.
-}   
import Data.Char
import System.IO
import System.Environment
{- In this question three custom types are created. 'CharacterCount' type is mapping from characters to integers.
'DictionaryWordCharacterCount' type is mapping from word to CharacterCount.
'DictionaryCharacterCountWord' type is mapping from CharacterCount to word.
Empty lists are created for three of them for future use.
-}
type CharacterCount = (Char,Int)
type DictionaryWordCharacterCount = ([Char],[CharacterCount])
type DictionaryCharacterCountWord = ([CharacterCount],[[Char]])

character_count_list :: [CharacterCount]
character_count_list = []

dict_word_char_counts :: [DictionaryWordCharacterCount]
dict_word_char_counts = []

dict_char_counts_word :: [DictionaryCharacterCountWord]
dict_char_counts_word = []

{-
This function inserts CharacterCount(Char,Int) to CharacterCountList. It uses
tail recursive approach. If the character is not in the CharacterCountList it adds
directly. But if character is in the CharacterCountList the function should be update count only.
So firstly, finds current count and adds it new element's count. Deletes old element's from list and adds new element to the list.
-}
insertCharacterCountList :: (CharacterCount) -> [CharacterCount] -> [CharacterCount]
insertCharacterCountList character_count character_count_list
    -- If character is in the charactCount list increment number of count.
    | length (filter (\x -> (fst x) == fst character_count) character_count_list) /= 0 =
        let currenct_count = snd (head(filter (\x -> (fst x) == fst character_count) character_count_list))
            new_count = (snd character_count) + currenct_count
            list_without_that_char = filter (\x -> (fst x) /= (fst character_count)) character_count_list
            new_list = list_without_that_char ++ [(fst character_count,new_count)]
        in new_list
    -- Otherwise add directly new charactercount to charactercount list
    | otherwise = character_count_list ++ [character_count]

{-
This function adds the first list to the second list and finally returns list2.
In every iteration, inserts head of list1 to list2 using insertCharacterCountList function.
-}
unionTwoCharacterCountList :: [CharacterCount] -> [CharacterCount] -> [CharacterCount]
unionTwoCharacterCountList list1 list2
    | length list1 == 0 = list2 -- Final Iteration
    -- Add first element of list1 to list2.
    | otherwise = unionTwoCharacterCountList (tail list1) (insertCharacterCountList (head list1) list2)

{-Q1: This function gets a string and returns its character counts. I have created
2 helper functions. 'wordCharCounts'' helper function takes word and empty list, it iterates
using tail recursive approach and returns character counts. If it is final iteration,
the function returns sorted list to future use.
'calculateAndRemoveChar'' helper function takes word and character count list. Controls first letter of word.
It finds out how many times repeated first letter and deletes all repeats and itself. Then add char and number of repeats to chacter count list
It returns reduced word and new character count list.
-}
wordCharCounts :: [Char] -> [CharacterCount]
wordCharCounts w = wordCharCounts' w character_count_list
    where
        wordCharCounts' :: [Char] -> [CharacterCount] -> [CharacterCount]
        wordCharCounts' word character_count 
            | length word == 0 = quicksort character_count -- Final Iteration
            | otherwise = 
                let (reduced_word,new_character_list) = calculateAndRemoveChar' (map toLower word) character_count
                in wordCharCounts' reduced_word new_character_list
        -- Remove first letter of word and insert it to charactercount list.
        calculateAndRemoveChar' :: [Char] -> [CharacterCount] -> DictionaryWordCharacterCount
        calculateAndRemoveChar' word character_count_list =
            let hd = head word -- First character
                num_of_c = length (filter (==hd) word) -- number of repeat in string
            in  ((filter (/=hd) word), insertCharacterCountList (hd,num_of_c) character_count_list)

{-Q2: This function takes sentence(set of words) and returns CharacterCount list. 
'sentenceCharCounts'' helper function takes sentence and empty charactercount list then returns charactercount list.
It uses tail recursive approach. If it is final iteration, the function returns sorted list to future use.
This function calls unionTwoCharacterCountList and merges new character list and total character list. 
-}
sentenceCharCounts :: [[Char]] -> [CharacterCount]
sentenceCharCounts all_sentences = sentenceCharCounts' all_sentences character_count_list
    where
        sentenceCharCounts' :: [[Char]] -> [CharacterCount] -> [CharacterCount]
        sentenceCharCounts' word_list character_list
            | length(word_list) == 0 = quicksort character_list -- Final Iteration
            -- Add characterCount list of first word of wordlist to charactercountlist.
            | otherwise = sentenceCharCounts' (tail word_list) (unionTwoCharacterCountList character_list (wordCharCounts (head word_list)))

{-Q3: This function takes (set of words) and returns DictionaryWordCharacterCount list.
dictCharCounts' helper function takes set of words and empty list. It iterates using tail recursive approach.
Except final iteration, it looks first word of word list(every iteration it reduces) and find character counts of that word.
'insertCharacterCountListAndWordToDict' helper function inserts new element to dictionary and returns new dictionary.
 -}
dictCharCounts :: [[Char]] ->  [DictionaryWordCharacterCount]
dictCharCounts word_list = dictCharCounts' word_list dict_word_char_counts
    where
        dictCharCounts' :: [[Char]] ->  [DictionaryWordCharacterCount] ->  [DictionaryWordCharacterCount]
        dictCharCounts' word_list dict_character_list
            | length word_list == 0 = quicksort dict_character_list -- Final Iteration
            -- Add first word of word list and its charactercount to dictionary.
            | otherwise = 
                let word = head word_list
                    character_list = wordCharCounts word
                in dictCharCounts' (tail word_list) (insertCharacterCountListAndWordToDict word character_list (dict_character_list))
        -- Inserting new element to dictionary
        insertCharacterCountListAndWordToDict :: [Char] -> [CharacterCount] -> [DictionaryWordCharacterCount]  -> [DictionaryWordCharacterCount]
        insertCharacterCountListAndWordToDict word new_character_count_list dictionary =
            let new_element = (word,new_character_count_list)
                new_dictionary = dictionary ++ [new_element] 
            in new_dictionary

{-
This function takes one DictionaryWordCharacterCount and returns one DictionaryCharacterCountWord
changing place of first element and place of second element.
-}
convertCtoWDict :: DictionaryWordCharacterCount -> DictionaryCharacterCountWord
convertCtoWDict element = (snd element,[fst element])

{-
This function takes a dictionary(CharacterCount to Word) and returns only Word.
-}
convertWords :: DictionaryCharacterCountWord -> [Char]
convertWords element = head (snd element)

{-
This function takes dictionary(Word to characterCount) and converts it new dictionary(CharacterCount to Word)
dictWordsByCharCounts' function firstly changes order of word and character count using convertCtoWDict function.
Then make calls to mergeSameThings helper function to merge words that have same character counts.
Until this time we done other parts as sorted because to do this part easily.
MergeSameThings function uses tail recursive approach. In every iteration
it finds words that have same character counts, then removes them from list and merges that word.
Then it adds merged elements to list.
-}
dictWordsByCharCounts :: [DictionaryWordCharacterCount] -> [DictionaryCharacterCountWord]
dictWordsByCharCounts dictWordToChar = dictWordsByCharCounts' dictWordToChar
    where 
        dictWordsByCharCounts' :: [DictionaryWordCharacterCount] -> [DictionaryCharacterCountWord]
        dictWordsByCharCounts' dictWordToChar = mergeSameThings (map convertCtoWDict dictWordToChar) (map convertCtoWDict dictWordToChar)
        -- Merging same elements(have same characterCount) on the dictionary.
        mergeSameThings :: [DictionaryCharacterCountWord] -> [DictionaryCharacterCountWord]->[DictionaryCharacterCountWord]
        mergeSameThings dictCharToWord dictUnique
            | length dictCharToWord == 0 = dictUnique -- Final Iteration
            -- If there is more than one word for one countList in the dictionary.
            | length (filter (\x -> (fst x) == (fst (head dictCharToWord))) dictCharToWord) /= 1 = 
                let element = head dictCharToWord
                    hs = filter (\x -> (fst x) == (fst element)) dictCharToWord
                    list_without_that_elements = filter (\x -> (fst x) /= (fst element)) dictUnique
                    new_word_list = (map convertWords hs)
                    new_unique_dict = list_without_that_elements ++ [(fst element,new_word_list)]
                in mergeSameThings (list_without_that_elements) new_unique_dict
            | otherwise = mergeSameThings (tail dictCharToWord) dictUnique

{-
This function takes word and charater count to word dictionary, then returns anagrams of that word in dictionary.
This function first finds characterCounts of given word then look dictionary to find words that have same character counts.
And it returns words that have same character counts
-}
wordAnagrams :: [Char] -> [DictionaryCharacterCountWord] -> [[Char]]
wordAnagrams word dictionary = 
    let wordDictionary = wordCharCounts word
        -- All words that has given wordCharCounts in the dictionary.
        samePatternWords = filter (\x -> (fst x) == wordDictionary) dictionary
        all_words = snd (head samePatternWords)
    in all_words

{-
This function takes 2 characterCounts(Second one set of first one) and subtracts second one from first one.
'subtractCounts'' helper function iterates until length of first one is not 0. Finally it returns resultmapping. 
This function uses tail recursive approach. It iterates all characters of mapping1.
If mapping2 also have this character, the function subtracts character counts of second one from first one.
Then make calls to itself with reduced mapping1 and updated resultmapping.
If mapping1 has character but mapping2 has not that character, the function adds directly character and character counts to resultmapping.
-}
subtractCounts :: [CharacterCount]->[CharacterCount]->[CharacterCount]
subtractCounts mapping1 mapping2 = subtractCounts' mapping1 mapping2 []
    where
        subtractCounts':: [CharacterCount]->[CharacterCount]->[CharacterCount]->[CharacterCount]
        subtractCounts' mapping1 mapping2 resultmapping
            | length mapping1 == 0 = resultmapping -- Final Iteration
            -- If first mapping and second mapping have same character
            | length (filter (\x -> (fst x) == (fst (head mapping1))) mapping2) == 1 = 
                let element = head mapping1 
                    decrease = snd (head(filter (\x -> (fst x) == (fst (head mapping1))) mapping2))
                    new_list = filter (\x -> (snd x) /= 0) (resultmapping ++ [(fst element,(snd element)-decrease)])
                in subtractCounts' (tail mapping1) mapping2 new_list
            -- First one has but second one does not have.
            | otherwise = 
                let element = head mapping1
                    new_list = resultmapping ++ [(fst element,snd element)]
                in subtractCounts' (tail mapping1) mapping2 new_list

{-
This function creates subset according to given charactercount list.
Firstly, this function converts charactercountlist to a set of chars 
using 'convertToWord' and 'createString' helper functions. Then the function
creates all set of words using 'subsets' helper function. These set have some same elements
because of design but with 'removeDuplicates' helper function we are deleting duplicate elements.
Then all subsets are being converted to characterCount lists using 'convertToCharacterCountList' and 
'convertToCharacterCountList'' helper functions.
-}
charCountsSubsets :: [CharacterCount] -> [[CharacterCount]]
charCountsSubsets cc =
    --Convert given characterCount to a word.
    let word = convertToWord ([],cc) 
        -- Find all subsets of that list of chars. And remove duplicates from list.
        all_subsets = removeDuplicates (subsets word)
        -- Convert all subsets on the list to characterCount list.
        all_subsets_list = map (\x -> convertToCharacterCountList x []) all_subsets
    in all_subsets_list
    where
        convertToWord :: DictionaryWordCharacterCount -> [Char] 
        convertToWord (current_word,ch_list)
            | length ch_list == 0 = current_word -- Final Iteration
            | otherwise = 
                let element = head ch_list
                    ch = fst element
                    numberOfRepeat = snd element
                    -- Duplicate given char
                    str = createString (ch:[]) numberOfRepeat 
                    -- Add new string to list and call functions itself.
                in convertToWord ((current_word ++ str),(tail ch_list)) 
        -- Duplicate given char(converted string) to multiple char(according to n).
        -- if string "a" and n = 4, the function returns aaaa. 
        createString :: String -> Int -> String
        createString string n = concat $ replicate n string
        -- Find subsets of list
        subsets :: [a] -> [[a]]
        subsets [] = [[]]
        subsets (x:xs) = [zs | ys <- subsets xs, zs <- [ys, (x:ys)]]
        -- Remove duplicats of the list.
        removeDuplicates :: (Ord a, Eq a) => [a] -> [a]
        removeDuplicates xs = remove $ quicksort xs
            where
                remove []  = []
                remove [x] = [x]
                remove (x1:x2:xs)
                    | x1 == x2  = remove (x1:xs)
                    | otherwise = x1 : remove (x2:xs)
        -- Convert given string to characterCount list
        convertToCharacterCountList :: [Char] -> [CharacterCount] -> [CharacterCount]
        convertToCharacterCountList word character_count_list
            | length word == 0 = character_count_list -- Final Iteration
            -- Add convert first character of word to characterlist and it to list.
            | otherwise = convertToCharacterCountList (tail word) (convertToCharacterCountList' character_count_list (head word))
        -- Add given char to characterCount to characterCount list.
        convertToCharacterCountList' :: [CharacterCount] -> Char -> [CharacterCount]
        convertToCharacterCountList' character_count_list ch =
            insertCharacterCountList (head (wordCharCounts (ch:[]))) character_count_list

{-
Until the eighth question, all functions work as desired. My computer was freezing while creating dictionary for these words because the number of words in the given text file was too high. Despite my efforts to try and fix everything, I could not find a solution. And finally I had to give up. Functional programming has been great, but this question has aged me a lot.
-}   
sentenceAnagrams :: [Char] -> [DictionaryCharacterCountWord] ->[[Char]]
sentenceAnagrams sentence dictionary = ["I","couldnt","do","it"]
    where 
        l = charCountsSubsets (wordCharCounts (parseSentence sentence []))
        -- sentenceAnagrams' ::[Char]-> [Char] -> [[Char]] -> [[Char]]
        -- sentenceAnagrams' remaining_characters new_sentence all_sentences =
        --     | length remaining_characters == 0 = 
        --     |  
        parseSentence :: [Char] -> [Char] -> [Char]
        parseSentence sentence new_string
            | length sentence == 0 = new_string
            | head sentence /= ' ' = parseSentence (tail sentence) (new_string ++ [head sentence])
            | otherwise = parseSentence (tail sentence) (new_string)
{-
Until the eighth question, all functions work as desired. My computer was freezing while creating dictionary for these words because the number of words in the given text file was too high. Despite my efforts to try and fix everything, I could not find a solution. And finally I had to give up. Functional programming has been great, but this question has aged me a lot.
-}             
main = do
    handle <- readFile "words.txt"
    let allWords = lines handle
    args <- getArgs
    print(args)
    let dictionary = dictWordsByCharCounts (dictCharCounts allWords)
    let allAnagrams = sentenceAnagrams (head args) dictionary
    print(allAnagrams)

-- This function sorts given list using quicksort algorithm.
quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
-- Call quicksort lesser + [x] + quicksort bigger  
quicksort (x:xs) = (quicksort [a|a<-xs, a<=x]) ++ [x] ++ (quicksort [a|a<-xs, a>x])


