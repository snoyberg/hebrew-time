{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE CPP #-}
---------------------------------------------------------
--
-- Module        : Data.Time.Calendar.Hebrew
-- Copyright     : Michael Snoyman
-- License       : BSD3
--
-- Maintainer    : Michael Snoyman <michael@snoyman.com>
-- Stability     : Unstable
-- Portability   : portable
--
-- Conversion to and from Hebrew dates.
--
---------------------------------------------------------
module Data.Time.Calendar.Hebrew
    ( -- * Data types
      HebrewDate (..)
    , Month (..)
      -- * Conversions
    , fromHebrew
    , toHebrew
    , monthHebrew
      -- * Anniversaries
    , anniversaryInYear
    , nextAnniversary
#if TEST
      -- * Testing
    , testSuite
#endif
    ) where

import Data.Typeable (Typeable)
import Data.Data (Data)
import Control.Arrow
import Data.Time.Calendar (Day (..))

#if TEST
import Control.Applicative ((<$>))
import Data.Time.Calendar.WeekDate (toWeekDate)
import Data.Time.Calendar (fromGregorian)
import Test.Framework (testGroup, Test)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.HUnit hiding (Test)
import Test.QuickCheck
#endif

------ data definitions
data Month = Tishrei | Cheshvan | Kislev | Tevet | Shevat
           | Adar | Adar1 | Adar2
           | Nissan | Iyar | Sivan | Tammuz | Av | Elul
    deriving (Eq, Ord, Show, Enum, Read, Data, Typeable)
data YearType = Chaser | Ksidran | Shlema
    deriving (Eq, Ord, Show, Enum)
data YearLeap = Leap | NonLeap
    deriving (Eq, Ord, Show, Enum)

monthHebrew :: Month -> String
monthHebrew Tishrei = "תשרי"
monthHebrew Cheshvan = "חשון"
monthHebrew Kislev = "כסלו"
monthHebrew Tevet = "טבת"
monthHebrew Shevat = "שבט"
monthHebrew Adar = "אדר"
monthHebrew Adar1 = "אדר א"
monthHebrew Adar2 = "אדר ב"
monthHebrew Nissan = "ניסן"
monthHebrew Iyar = "אייר"
monthHebrew Sivan = "סיון"
monthHebrew Tammuz = "תמוז"
monthHebrew Av = "אב"
monthHebrew Elul = "אלול"

------ newtypes
newtype Chalakim = Chalakim Integer
    deriving (Eq, Ord, Show, Enum, Num, Real, Integral)
type TotalChalakim = Chalakim

newtype Shaot = Shaot Integer
    deriving (Eq, Ord, Show, Enum, Num, Real, Integral)

newtype Days = Days Integer
    deriving (Eq, Ord, Show, Enum, Num, Real, Integral)
type Weekday = Days
type Julian = Days
type TotalDays = Days
type Date = Days

newtype Weeks = Weeks Integer
    deriving (Eq, Ord, Show, Enum, Num, Real, Integral)

newtype Months = Months Integer
    deriving (Eq, Ord, Show, Enum, Num, Real, Integral)

newtype Years = Years Integer
    deriving (Eq, Ord, Show, Enum, Num, Real, Integral)

------ simple conversions
daysFromWeeks :: Weeks -> Days
daysFromWeeks (Weeks w) = Days (w * 7)

weeksFromDays :: Days -> (Weeks, Days)
weeksFromDays (Days d) = (Weeks *** Days) (d `divMod` 7)

shaotFromDays :: Days -> Shaot
shaotFromDays (Days d) = Shaot (d * 24)

daysFromShaot :: Shaot -> (Days, Shaot)
daysFromShaot (Shaot s) = (Days *** Shaot) (s `divMod` 24)

chalakimFromShaot :: Shaot -> Chalakim
chalakimFromShaot (Shaot s) = Chalakim (s * 1080)

shaotFromChalakim :: Chalakim -> (Shaot, Chalakim)
shaotFromChalakim (Chalakim c) = (Shaot *** Chalakim) (c `divMod` 1080)

chalakimFromMonths :: Months -> Chalakim
chalakimFromMonths (Months m) = Chalakim m * lunarMonth

------ constants
lunarMonth :: TotalChalakim
lunarMonth = joinChalakim 0 29 12 793

------ building functions
splitChalakim :: TotalChalakim -> (Weeks, Weekday, Shaot, Chalakim)
splitChalakim tc =
    let (s', c) = shaotFromChalakim tc
        (d', s) = daysFromShaot s'
        (w, d) = weeksFromDays d'
     in (w, d, s, c)

#if TEST
case_splitChalakim :: Assertion
case_splitChalakim = do
    splitChalakim 1080 @=? (0, 0, 1, 0)
    splitChalakim (15 * 24 * 1080) @=? (2, 1, 0, 0)
#endif

joinChalakim :: Weeks -> Days -> Shaot -> Chalakim -> TotalChalakim
joinChalakim w d s c =
    chalakimFromShaot (shaotFromDays (daysFromWeeks w + d) + s) + c

#if TEST
prop_joinSplitChalakim :: TotalChalakim -> Bool
prop_joinSplitChalakim tc = tc == uncurry4 joinChalakim (splitChalakim tc)
    where
        uncurry4 :: (a -> b -> c -> d -> e) -> (a, b, c, d) -> e
        uncurry4 f (a, b, c, d) = f a b c d
#endif

------ year dependent constants
isLeapYear :: Years -> YearLeap
isLeapYear y =
    let res =
             case y `mod` 19 of
                3 -> Leap
                6 -> Leap
                8 -> Leap
                11 -> Leap
                14 -> Leap
                17 -> Leap
                0 -> Leap -- 19
                _ -> NonLeap
    in res

monthsTilTishrei :: Years -> Months
monthsTilTishrei (Years i) = Months $ (235 * i - 234) `div` 19

#if TEST
case_monthsTilTishrei :: Assertion
case_monthsTilTishrei = do
    0 @=? monthsTilTishrei 1
    12 @=? monthsTilTishrei 2
    24 @=? monthsTilTishrei 3
    37 @=? monthsTilTishrei 4
    235 @=? monthsTilTishrei 20

extraMonthCount :: Years -> Months
extraMonthCount i =
    case i of
        0 -> 0
        1 -> 0
        2 -> 0
        3 -> 1
        4 -> 1
        5 -> 1
        6 -> 2
        7 -> 2
        8 -> 3
        9 -> 3
        10 -> 3
        11 -> 4
        12 -> 4
        13 -> 4
        14 -> 5
        15 -> 5
        16 -> 5
        17 -> 6
        18 -> 6
        _ -> error $ "extraMonthCount: " ++ show i

monthsTilTishreiLong :: Years -> Months
monthsTilTishreiLong (Years y') =
    let (machzorim, y) = (y' - 1) `divMod` 19
        base = Months $ (y' - 1) * 12 + machzorim * 7
        extra = extraMonthCount $ Years y
     in base + extra

prop_monthsTilTishrei :: Years -> Bool
prop_monthsTilTishrei y = monthsTilTishrei y == monthsTilTishreiLong y
#endif

firstTishrei :: TotalChalakim
firstTishrei = joinChalakim 0 1 5 204

moladTishrei :: Years -> TotalChalakim
moladTishrei y = chalakimFromMonths (monthsTilTishrei y) + firstTishrei

#if TEST
case_moladTishrei :: Assertion
case_moladTishrei = do
    let testMolad w x y z = do
        let (_, d, s, c) = splitChalakim $ moladTishrei w
         in (w, d, s, c) @?= (w, x, y, z)
    testMolad 5764 5 10 491
    testMolad 1 1 5 204
    testMolad 2 5 14 0
    testMolad 3 2 22 876
    testMolad 4 1 20 385
    testMolad 5 6 5 181
    testMolad 6 3 13 1057
    testMolad 7 2 11 566
    testMolad 8 6 20 362
    testMolad 9 5 17 951
    testMolad 10 3 2 747
    testMolad 11 0 11 543
    testMolad 18 0 15 414
    testMolad 19 5 0 210
    testMolad 20 3 21 799
#endif

monthLength :: YearLeap -> YearType -> Month -> Days
monthLength _ _ Tishrei = 30
monthLength _ _ Tevet = 29
monthLength _ _ Shevat = 30
monthLength _ _ Nissan = 30
monthLength _ _ Iyar = 29
monthLength _ _ Sivan = 30
monthLength _ _ Tammuz = 29
monthLength _ _ Av = 30
monthLength _ _ Elul = 29
monthLength Leap _ Adar = 0
monthLength Leap _ Adar1 = 30
monthLength Leap _ Adar2 = 29
monthLength NonLeap _ Adar = 29
monthLength NonLeap _ Adar1 = 0
monthLength NonLeap _ Adar2 = 0
monthLength _ Shlema Cheshvan = 30
monthLength _ _ Cheshvan = 29
monthLength _ Chaser Kislev = 29
monthLength _ _ Kislev = 30

------ conversion functions
dateFromJulian :: YearLeap -> YearType -> Julian -> (Month, Date)
dateFromJulian yl yt j' =
    let ml = monthLength yl yt
        helper :: Month -> Julian -> (Month, Date)
        helper m j
            | ml m >= j = (m, j)
            | m == Elul =
                error $ "Invalid dateFromJulain args: " ++ show (yl, yt, j', j)
            | otherwise = helper (succ m) (j - ml m)
     in helper Tishrei j'

#if TEST
julianFromDate :: YearLeap -> YearType -> Month -> Date -> Julian
julianFromDate yl yt m d =
    let ml = monthLength yl yt
        months = case m of
            Tishrei -> []
            _ -> enumFromTo Tishrei (pred m)
     in d + sum (map ml months)

prop_dateToFromJulian :: YearLeap -> YearType -> Julian -> Bool
prop_dateToFromJulian yl yt j =
    j == uncurry (julianFromDate yl yt) (dateFromJulian yl yt j)
#endif

------ determining year stuff
roshHashana :: Years -> TotalDays
roshHashana y = daysFromWeeks w + d + dechiyot
    where
        (w, d, s, c) = splitChalakim $ moladTishrei y
        dechiyot
            | s > 18 || s == 18 && c > 0 =
                case d of
                    0 -> 1
                    1 -> 1
                    2 -> 2 -- otherwise it would be Wednesday
                    3 -> 1
                    4 -> 2 -- otherwise it would be Friday
                    5 -> 1
                    6 -> 2 -- otherwise it would be Sunday
                    _ -> error $ "roshHashana: d ==" ++ show d
            | d `elem` [0, 3, 5] = 1 -- ADU rosh
            | d == 2 &&
              isLeapYear y == NonLeap &&
              (s > 9 ||
               s == 9 && c > 204) = 2
            | isLeapYear (y - 1) == Leap &&
              d == 1 &&
              (s > 15 ||
               s == 15 && c > 589) = 2
            | otherwise = 0

#if TEST
case_firstRoshHashana :: Assertion
case_firstRoshHashana = roshHashana 1 @?= 1

dayOfWeek :: TotalDays -> Weekday
dayOfWeek t =
    let (_, w) = weeksFromDays t
     in w

prop_validRoshHashanaDay :: Years -> Bool
prop_validRoshHashanaDay = (`elem` [1, 2, 4, 6]) . dayOfWeek . roshHashana
#endif

yearLength :: Years -> TotalDays
yearLength y = roshHashana (y + 1) - roshHashana y

#if TEST
prop_yearLength :: Years -> Bool
prop_yearLength y =
    let l = yearLength y
     in l `elem` [353, 354, 355, 383, 384, 385]
#endif

julianFromDays :: TotalDays -> (Years, Julian)
julianFromDays td = uncurry helper $ approx td where
    helper :: Years -> TotalDays -> (Years, Julian)
    helper y d -- FIXME do not use yearLength here...
        | yearLength y < d = helper (y + 1) (d - yearLength y)
        | otherwise = (y, fromIntegral d)
    approx :: TotalDays -> (Years, TotalDays)
    approx (Days td') =
        let minYears = Years $ td' `div` 366
            Days rh = roshHashana minYears
            rem' = Days $ td' - rh + 1
         in (minYears, rem')

#if TEST
prop_roshHashana_julianFromDays :: Years -> Bool
prop_roshHashana_julianFromDays y = (y, 1) == julianFromDays (roshHashana y)
#endif

yearDef :: TotalDays -> TotalDays -> (YearLeap, YearType)
yearDef a b = case b - a of
                353 -> (NonLeap, Chaser)
                354 -> (NonLeap, Ksidran)
                355 -> (NonLeap, Shlema)
                383 -> (Leap, Chaser)
                384 -> (Leap, Ksidran)
                385 -> (Leap, Shlema)
                x -> error $ "Invalid year length: " ++ show x

------ convert dates
data HebrewDate = HebrewDate
    { year :: Int
    , month :: Month
    , date :: Int
    }
    deriving (Eq, Data, Typeable)
instance Show HebrewDate where
    show (HebrewDate y m d) = show d ++ " " ++ show m ++ ", " ++ show y

epochOffset :: Integral i => i
epochOffset = 2052004

fromHebrew :: HebrewDate -> Day
fromHebrew h =
    let Days td = totalDaysFromHebrew h
     in ModifiedJulianDay $ td - epochOffset

toHebrew :: Day -> HebrewDate
toHebrew d' =
    let jd = toModifiedJulianDay d' + epochOffset
        td = fromIntegral jd
        (y, j) = julianFromDays td
        (yl, yt) = yearDef (roshHashana y) (roshHashana $ y + 1)
        (m, d) = dateFromJulian yl yt j
     in HebrewDate (fromIntegral y) m (fromIntegral d)

totalDaysFromHebrew :: HebrewDate -> TotalDays
totalDaysFromHebrew (HebrewDate y m d) =
    let rh = roshHashana $ Years $ fromIntegral y
        rh2 = roshHashana $ Years $ fromIntegral $ y + 1
        (yl, yt) = yearDef rh rh2
        ml = monthLength yl yt
        ds = fromIntegral $ sum $ map ml [Tishrei ..m]
     in rh + ds + fromIntegral d - fromIntegral (ml m) - 1

#if TEST
prop_fromToHebrew :: Integer -> Bool
prop_fromToHebrew d' =
    let d = ModifiedJulianDay d'
     in d == fromHebrew (toHebrew d)

prop_sameWeekday :: HebrewDate -> Bool
prop_sameWeekday h =
    let td = totalDaysFromHebrew h
        wd1 = dayOfWeek td
        d = fromHebrew h
        (_, _, wd2) = toWeekDate d
        wd2' = fromIntegral wd2 `mod` 7
     in wd1 == wd2'

case_integralSpotCheck :: Assertion
case_integralSpotCheck = do
    (toModifiedJulianDay $ fromGregorian 2009 9 26) @=?
     (toModifiedJulianDay $ fromHebrew $ HebrewDate 5770 Tishrei 8)
    dayOfWeek (roshHashana 5770) @?= 6
    roshHashana 5770 @=? totalDaysFromHebrew (HebrewDate 5770 Tishrei 1)
    dayOfWeek (totalDaysFromHebrew $ HebrewDate 5770 Tishrei 3) @?= 1

case_spotChecks :: Assertion
case_spotChecks = do
    fromGregorian 1984 9 27 @=? fromHebrew (HebrewDate 5745 Tishrei 1)
    fromGregorian 1985 1 12 @=? fromHebrew (HebrewDate 5745 Tevet 19)
    fromGregorian 1986 9 8 @=? fromHebrew (HebrewDate 5746 Elul 4)
#endif

clip :: HebrewDate -> HebrewDate
clip (HebrewDate y m d) =
    let y' = Years $ fromIntegral y
        (yl, yt) = yearDef (roshHashana y') (roshHashana $ y' + 1)
        m' = adjustMonth yl m
        ml = fromIntegral $ monthLength yl yt m'
        d' = if d > ml then ml else d
     in HebrewDate y m' d'

adjustMonth :: YearLeap -> Month -> Month
adjustMonth Leap Adar = Adar2
adjustMonth Leap x = x
adjustMonth NonLeap Adar1 = Adar
adjustMonth NonLeap Adar2 = Adar
adjustMonth NonLeap x = x

anniversaryInYear :: Int -- ^ year
                  -> HebrewDate
                  -> HebrewDate
anniversaryInYear y (HebrewDate _ m d) = clip $ HebrewDate y m d

#if TEST
caseAnniversaryInYear :: IO ()
caseAnniversaryInYear = do
    -- Year 5770 is just the current year at time of writing
    -- Year 3 is a chaser leap year
    -- Year 4 is a leap year
    HebrewDate 5770 Tishrei 1 @=?
        anniversaryInYear 5770 (HebrewDate 1 Tishrei 1)
    HebrewDate 3 Cheshvan 29 @=?
        anniversaryInYear 3 (HebrewDate 1 Cheshvan 30)
    HebrewDate 3 Kislev 29 @=?
        anniversaryInYear 3 (HebrewDate 1 Kislev 30)
    HebrewDate 3 Adar2 1 @=?
        anniversaryInYear 3 (HebrewDate 1 Adar 1)
    HebrewDate 4 Adar 1 @=?
        anniversaryInYear 4 (HebrewDate 1 Adar1 1)
    HebrewDate 4 Adar 1 @=?
        anniversaryInYear 4 (HebrewDate 1 Adar2 1)
    HebrewDate 4 Adar 29 @=?
        anniversaryInYear 4 (HebrewDate 1 Adar1 30)
#endif

nextAnniversary :: HebrewDate -- ^ so to say current date
                -> HebrewDate -- ^ date of event
                -> HebrewDate -- ^ first anniversary of event after current
nextAnniversary (HebrewDate cy cm cd) hd@(HebrewDate _y m d)
    | cm > m || cm == m && cd > d = anniversaryInYear (cy + 1) hd
    | otherwise = anniversaryInYear cy hd

#if TEST
caseNextAnniversary :: IO ()
caseNextAnniversary = do
    HebrewDate 5770 Tishrei 2 @=?
        nextAnniversary (HebrewDate 5770 Tishrei 1) (HebrewDate 1 Tishrei 2)
    HebrewDate 5771 Tishrei 2 @=?
        nextAnniversary (HebrewDate 5770 Tishrei 3) (HebrewDate 1 Tishrei 2)

------ testing
testSuite :: Test
testSuite = testGroup "Data.Time.Calendar.Hebrew"
    [ testProperty "join and split chalakim" prop_joinSplitChalakim
    , testProperty "to/from julian date" prop_dateToFromJulian
    , testCase "first rosh hashana is day 1" case_firstRoshHashana
    , testProperty "rosh hashana/julianFromDays" prop_roshHashana_julianFromDays
    , testProperty "to/from hebrew" prop_fromToHebrew
    , testCase "splitChalakim " case_splitChalakim
    , testCase "molad tishrei" case_moladTishrei
    , testProperty "months til tishrei" prop_monthsTilTishrei
    , testProperty "valid year length" prop_yearLength
    , testCase "months til tishrei case" case_monthsTilTishrei
    , testProperty "rosh hashana valid weekday" prop_validRoshHashanaDay
    , testProperty "greg/hebrew same weekday" prop_sameWeekday
    , testCase "integral date spot check" case_integralSpotCheck
    , testCase "individual date spot checks" case_spotChecks
    , testCase "caseAnniversaryInYear" caseAnniversaryInYear
    , testCase "caseNextAnniversary" caseNextAnniversary
    ]

instance Arbitrary Chalakim where
    arbitrary = fromIntegral <$> (arbitrary :: Gen Int)

instance Arbitrary Days where
    arbitrary = fromIntegral . (+ 1) . (`mod` 353)
            <$> (arbitrary :: Gen Int)

instance Arbitrary Years where
    arbitrary = fromIntegral . (+ 1) . (`mod` 6000)
            <$> (arbitrary :: Gen Int)

enumAll :: Enum e => [e]
enumAll = enumFrom $ toEnum 1

instance Arbitrary YearLeap where
    arbitrary = elements enumAll

instance Arbitrary YearType where
    arbitrary = elements enumAll

instance Arbitrary HebrewDate where
    arbitrary = do
        m <- elements [Tishrei, Cheshvan, Kislev, Tevet, Shevat,
                       Nissan, Iyar, Sivan, Tammuz, Av, Elul]
        y <- (+ 1) . (`mod` 6000) <$> arbitrary
        day <- (+ 1) . (`mod` 29) <$> arbitrary
        return $! HebrewDate y m day
#endif
