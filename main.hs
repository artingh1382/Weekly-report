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
           deriving (Read,Eq,Ord)

-- new data type consisting of all the days in the week
data Day = Sat
          | Sun
          | Mon
          | Tue
          | Wed
          | Thu
          | Fri
          deriving (Read,Eq)

-- the Topic data type consists of a Lesson, Time (synonym of Int) and a "Tests" (synonym of Int)
-- TODO update the Topic Type to be an instance of the Either type class
data Topic = Topic Lesson Time Tests | Empty deriving Eq

-- gets a maybe topic and returns a normal topic instead of Just Topic
justTopic :: Maybe Topic -> Topic
justTopic (Just (Topic l ti te)) = Topic l ti te
justTopic Nothing                = Empty
justTopic (Just Empty)           = Empty


sameTopic :: Topic -> Topic -> Bool
sameTopic _ Empty = False
sameTopic Empty _ = False
sameTopic (Topic l1 ti1 te1) (Topic l2 ti2 te2)
    | l1 == l2 = True
    | otherwise = False

-- gets two Topics time and combines them if they have the same Lesson type
combineTopics' :: Topic -> Topic -> Maybe Topic
combineTopics' Empty (Topic l ti te) = Just (Topic l ti te)
combineTopics' (Topic l ti te) Empty = Just (Topic l ti te)
combineTopics' Empty Empty = Nothing
combineTopics' (Topic l1 ti1 te1) (Topic l2 ti2 te2)
    | l1 == l2 = Just (Topic l1 newTi newTe)
    | otherwise = Nothing

  where newTi = ti1 + ti2
        newTe = te1 + te2


combineTopics :: Topic -> Topic -> Topic
combineTopics (Topic l1 ti1 te1) (Topic l2 ti2 te2)
    | result == Nothing = Empty
    | otherwise = justTopic result

  where result = combineTopics' (Topic l1 ti1 te1) (Topic l2 ti2 te2)



instance Show Topic where
  show (Topic lesson time tests) = mconcat [show lesson, ": ", "\n"
                                           , " ", "Time: ", show time, "\n"
                                           , " ", "Tests: ", show tests, "\n"]
  show Empty = "Empty"


instance Semigroup Topic where
  (<>) topic Empty   = topic
  (<>) Empty topic   = topic
  (<>) topic1 topic2 = combineTopics topic1 topic2


instance Monoid Topic where
  mempty = Empty
  mappend = (<>)

-- the Lex type class represents all a way to parse and convert all
-- of the possible lexicons in a file
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


-- the lexicon type class
instance Lex Day where
  parse day = read day :: Day
  convert day = showLex day


instance Lex Lesson where
  parse lesson = read lesson:: Lesson
  convert lesson = showLex lesson

-- takes the first 3 letter of the string version of our Lexicons
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


getLesson :: Topic -> Lesson
getLesson (Topic l _ _) = l

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
--topicAndQuant2 :: [Topic] -> (Topic -> Int) -> String
--topicAndQuant2 topicList f = map show
--  where names = map getTopic topicList
--        quant = map f topicList
--        string = mconcat [names, ": ", quant, "\n"]

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

-- gets strings that end with new line charecter and then turnes splits them whenever it sees the new line charecter and then gets rid of all the seperators
topicsInDays :: String -> [[String]]
topicsInDays day = map (splitOn "&") $ lines day

-- concatenates all the strings in the list together
stListToSt :: Foldable t => t [a] -> [a]
stListToSt = concat
--stListToSt = foldr (++) []

-- gets a single string of topics, tuples and seperator then returnes
-- a list of strings without the seperators
cleanStream :: String -> [String]
cleanStream = stListToSt . topicsInDays


-- gets a string and gets rid of the day data and useless spaces
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

-- gets a normal stream o
parseFile :: [String] -> [[String]]
parseFile stream = map words cleanStream
  where cleanStream = map (justString . parseFile') stream

-- sees if the string starts with "(" and if it does, we know that it's a string of tuple.
-- for example "(2,24)" so it reverses the tuple, gets rid of the closing parenthesis, reverses it again
-- and then gets rid off the opening parenthesis.
getNumbers' :: String -> String
getNumbers' stream
    | "(" `isPrefixOf` stream = justString $ stripPrefix "(" newStream
    | otherwise = stream

  where newStream = reverse $ justString $ stripPrefix ")" $ reverse stream

-- goes through the elements in a list and makes a tuple out of them
-- WARNING: this function can be used only and only for lists containing lists that have 2 elements in them
recTuple :: [[a]] -> [(a,a)]
recTuple []     = []
recTuple [[]]   = []
recTuple (x:xs) = (head x, last x) : recTuple xs


-- gets a parsed stream for example [["Dis","(90,23)"], ..]
-- gets rid of the parenthesis in "(90,23)" and returns "90,23"
-- converts the lists into lists of Lessons and lists of (Time,Tests)
-- then zips them together and returns them as a list
lessonsAndData' :: [[String]] -> [(Lesson,(Time,Tests))]
lessonsAndData' streams = zip lessons ts
  where helperGet = map getNumbers'
        cleaned = map helperGet streams
        sToInt = read :: String -> Int
        tuples = recTuple cleaned
        stringLessons = map fst tuples
        lessons = map parse stringLessons :: [Lesson]
        stringT = map snd tuples
        intT =  map (map sToInt . splitOn ",") stringT
        ts = recTuple intT


-- takes a list of list of Strings, maily the filtered stream without
-- the Days and seperators, an then returns a list of topic
lessonsAndData :: [[String]] -> [Topic]
lessonsAndData streams = map makeStream' sortedStreams
  where cleanedStreams = lessonsAndData' streams
        sortedStreams = sortOn fst cleanedStreams
        makeStreamOfT list = map makeStream' list
        makeStream' (x,(y1,y2)) = Topic x y1 y2

splitByTopic :: [Topic] -> [Topic]
splitByTopic = undefined

-- WARNING: this function can only work on a sorted list of Topics
combineAllLessons :: [Topic] -> [Topic]
combineAllLessons [] = []
combineAllLessons (x:xs)
    | null xs = [x]
    | sameTopic x (head xs) = x <> head xs : combineAllLessons (tail xs)
    | otherwise = x : combineAllLessons xs


parsedToTopics :: [[String]] -> [Topic]
parsedToTopics = combineAllLessons . lessonsAndData

pMain :: String -> [Topic]
pMain stream = parsedToTopics $ parseFile $ cleanStream stream

main :: IO ()
main = undefined
