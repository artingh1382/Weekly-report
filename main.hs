type Lesson = String
type Time = Int
type Tests = Int

data Topic = Topic Lesson Time Tests

lesson :: Lesson -> [Time] -> [Tests] -> Topic
lesson topic time tests = (Topic topic aTime aTests)
  where aTime= sum time
        aTests = sum tests


getTopic :: Topic -> Lesson
getTopic (Topic topic _ _) = topic


getTime :: Topic -> Time
getTime (Topic _ time _) = time


getTests :: Topic -> Tests
getTests (Topic _ _ tests) = tests


allTests :: [Topic] -> Tests
allTests topicList = sum $ map getTests topicList


allTimes :: [Topic] -> Time
allTimes topicList = sum $ map getTime topicList


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
