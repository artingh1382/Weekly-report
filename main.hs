--function to compute tests and times
--
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

--allTests :: [Topic] -> Tests


--allTimes :: [Topic] -> Time
percent :: Float -> Float -> Float
percent c a = (((3*c) - wrong)/ (3*a)) * 100
  where wrong = a-c
{-# LANGUAGE MultiParamTypeClasses #-}

type Minute = Int
type Hour = Int

-- a new data type named Time that has has two parts: Hour and minte
type Time = (Hour,Minute)


-- the getTime fucntion taks a Time and returns a String in return
getTime :: Time -> String
getTime (h,m) = show h ++ "h" ++ show m ++ "m"

-- a function which takes numeric value and converts it into Time
convertToTime :: Int -> Time
convertToTime 0 = (0,0)
convertToTime n = (hour,minute)
  where hour = n `div` 60
        minute = n - (hour * 60)


-- the convewrtTimeToMinutes is the inverse of our convertToTime function and gives us the summed minuts as a result
convertTimeToMinutes :: Time -> Int
convertTimeToMinutes (h,m) = (h * 60) + m

-- new data type representing days

type Months = Int
type Days = Int

type Date = (Months,Days)

getMonth :: Date-> String
getMonth (m,d) = show m ++ " Months" ++ " " ++ show d ++ " Days"

convertToDays :: Int -> Date
convertToDays 0 = (0,0)
convertToDays n = (months,days)
  where months = n `div` 30
        days = n - (months * 30)

convertDateToDays :: Date -> Int
convertDateToDays (m,d) = (m * 30) + d

class (Num a, Integral a) => ModArith a where
  convertIntToType :: a -> (a,a)
  convertIntToType 0 = (0,0)
  convertIntToType n = (months,days)
    where months = n `div` 30
          days = n - (months * 30)

  convertTypeToInt :: a -> a
  --convertTypeToInt (h,m) =
  showType :: a -> String

--instance ModArith Date where
--  convertIntToType 0 = Date 0 0
--  convertIntToType n = Date months days
--    where months = n `div` 30
--          days = n - (months * 30)


