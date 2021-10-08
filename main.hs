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


topicPlusTime :: [Topic] -> [String]
topicPlusTime topicList = map readable zipped
  where names = map getTopic topicList
        times = map getTime topicList
        zipped = zip names times
        readable = \pair -> show (fst pair) ++ " : " ++ show (snd pair)


topicPlusTest :: [Topic] -> [String]
topicPlusTest topicList = map readable zipped
  where names = map getTopic topicList
        tests = map getTests topicList
        zipped = zip names tests
        readable = \pair -> show (fst pair) ++ " : " ++ show (snd pair)


percent :: Float -> Float -> Float
percent c a = (((3*c) - wrong) / (3*a)) * 100
  where wrong = a-c

--add comments and refactor duplicate code with new functions that take functions as arguments
