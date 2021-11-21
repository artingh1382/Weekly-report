import           Data.List
import           Data.List.Split
import qualified Data.Map           as M
import           Data.Maybe
import           Data.Monoid
import           Data.Semigroup
import           System.Environment

-- using type synonyms to make type signatures help understand the transitions accuring in the function
type Time = Int
type Tests = Int

-- new data type that represents all the topics I study in school
data Lesson = Dis
           | Phy
           | Cal
           | Geo
           | Che
           | The
           | Ara
           | Lit
           deriving (Read,Eq)

-- new data type consisting of all the days in the week
data Day = Sat
          | Sun
          | Mon
          | Tue
          | Wed
          | Thu
          | Fri
          deriving (Read,Eq)


data Topic = Topic Lesson Time Tests | Empty

class Lex a where
  parse :: String -> a
  convert :: a -> String


instance Show Lesson where
  show Dis = "Discrete"
  show Phy = "Physics"
  show Cal = "Calculus"
  show Geo = "Geometry"
  show Che = "Chemistry"
  show The = "Theology"
  show Ara = "Arabic"
  show Lit = "Literature"


instance Show Day where
  show Sat = "Saturday"
  show Sun = "Sunday"
  show Mon = "Monday"
  show Tue = "Tuesday"
  show Wed = "Wednesday"
  show Thu = "Thursday"
  show Fri = "Friday"


instance Lex Day where
  parse day = read day :: Day
  convert day = showLex day


instance Lex Lesson where
  parse lesson = read lesson:: Lesson
  convert lesson = showLex lesson


showLex :: Show a => a -> String
showLex lex = take 3 $ show lex


justString :: Maybe String -> String
justString (Just string) = string
justString Nothing       = ""

-- takes a Lesson and a list of Time,Tests tuple and then converts
-- them into a Topic
lesson :: Lesson -> [(Time,Tests)] -> Topic
lesson topic list = Topic topic times tests
  where times = sum $ map (\pair -> fst pair) list
        tests = sum $ map (\pair -> snd pair) list

-- gets the Lesson (String)
getTopic :: Topic -> String
getTopic (Topic topic _ _) = show topic


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
-- TODO rewrite the functoin to print out output like this: Calculus| 180 Minutes == 3 Hours
topicAndQuant :: [Topic] -> (Topic -> Int) -> [String]
topicAndQuant topicList f = map together zipped
  where names = map getTopic topicList
        quant = map f topicList
        zipped = zip names quant
        together pair = fst pair ++ " : " ++ show (snd pair)

-- this function is an attempt to print and go to the next line
--topicAndQuant :: [Topic] -> (Topic -> Int) -> String
--topicAndQuant topicList f = mconcat $ map together zipped
--  where names = map getTopic topicList
--        quant = map f topicList
--        zipped = zip names quant
--        together = \pair -> fst pair ++ "| " ++ show (snd pair) ++ "\n"

-- takes the nubmer of unsolved, wrong and all of the tests then computes the accurate score
percent :: Float -> Float -> Float -> Float
percent e w a = ((c - (w/3)) / a) * 100
  where c = a - (e+w)

--add comments and refactor duplicate code with new functions that take functions as arguments
--
toFloatIO :: String -> IO Float
toFloatIO x = return float
  where float = read x :: Float


toFloat :: String -> Float
toFloat x = read x :: Float


topicsInDays :: String -> [[String]]
topicsInDays day = map (splitOn "&") $ lines day

stListToSt :: Foldable t => t [a] -> [a]
stListToSt xs = foldr (++) [] xs

--testList = [["My", "name", "is"], ["Barry", "Allen"], ["And", "I'm", "The"], ["Fastest", "man", "alive"]]

parseFile' :: String -> Maybe String
parseFile' stream
    | "Sat : " `isPrefixOf` stream = stripPrefix "Sat : " stream
    | "Sun : " `isPrefixOf` stream = stripPrefix "Sun : " stream
    | "Mon : " `isPrefixOf` stream = stripPrefix "Mon : " stream
    | "Tue : " `isPrefixOf` stream = stripPrefix "Tue : " stream
    | "Wed : " `isPrefixOf` stream = stripPrefix "Wed : " stream
    | "Thu : " `isPrefixOf` stream = stripPrefix "Thu : " stream
    | "Fri : " `isPrefixOf` stream = stripPrefix "Fri : " stream
    | " " `isPrefixOf` stream = stripPrefix " " stream
    | otherwise = Just stream


getNumbers' :: String -> String
getNumbers' stream
    | "(" `isPrefixOf` stream = justString $ stripPrefix "(" newStream
    | otherwise = stream

  where newStream = reverse $ justString $ stripPrefix ")" $ reverse stream

getNumbers :: [[String]] -> [[String]]
getNumbers stream = undefined


parseFile :: [String] -> [[String]]
parseFile stream = map words cleanStream
  where cleanStream = map justString $ map parseFile' stream


main :: IO ()
main = do
    file <- readFile "sample.txt"
    let stream = stListToSt $ topicsInDays file
    let parsed = parseFile stream
    mapM_ mapm' parsed

  where mapm' = mapM_ putStrLn
