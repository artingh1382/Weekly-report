import           Data.List
import qualified Data.Map           as M
import           Data.Monoid
import           Data.Semigroup
import           System.Environment

-- using type synonyms to make type signatures help understand the transitions accuring in the function
type Lesson = String
type Time = Int
type Tests = Int

data Topic = Topic Lesson Time Tests

-- a function that takes a String and two Lists of Integer then returns a Topic
lesson :: Lesson -> [Time] -> [Tests] -> Topic
lesson topic time tests = (Topic topic aTime aTests)
  where aTime= sum time
        aTests = sum tests


-- gets the Lesson (String)
getTopic :: Topic -> Lesson
getTopic (Topic topic _ _) = topic


-- gets the Time (Int)
getTime :: Topic -> Time
getTime (Topic _ time _) = time

-- gets the Tests(Int)
getTests :: Topic -> Tests
getTests (Topic _ _ tests) = tests


-- takes a list and a function on the list then maps that function and sums the values
allQuant :: Num a => [Topic] -> (Topic -> a) -> a
allQuant topicList f = sum $ map f topicList


-- takes either one of the getTime or getTests function and then returns a nice visual list
-- of Strings
topicAndQuant :: [Topic] -> (Topic -> Int) -> [String]
topicAndQuant topicList f = map together zipped
  where names = map getTopic topicList
        quant = map f topicList
        zipped = zip names quant
        together = \pair -> fst pair ++ " : " ++ show (snd pair)

-- takes the nubmer of unsolved, wrong and all of the tests then computes the accurate score
percent :: Float -> Float -> Float -> Float
percent e w a = ((c - (w/3)) / a) * 100
  where c = a - (e+w)

--add comments and refactor duplicate code with new functions that take functions as arguments
--
toFloat :: String -> Float
toFloat x = (read x) :: Float

main :: IO ()
main = do
    putStrLn "number of blank questions: "
    blanks <- getLine
    putStrLn "number of wrong questions: "
    wrongs <- getLine
    putStrLn "number of all the questions: "
    all <- getLine
    --let values = map toFloat [blanks, wrongs,  wrongs]
    --let e = values !! 0
    --let w = values !! 1
    --let a = values !! 2
    --let result = percent e w a
    --print result


