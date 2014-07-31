module Settings where

debug :: Bool
debug = True

areas :: [String]
areas = ["alpha", "beta"]

startArea :: String
startArea = "alpha"

areaUserSpeed :: Int
areaUserSpeed = 6 --units per second

initUserDurability :: Int
initUserDurability = 100

startAreaPos :: (Int, Int)
startAreaPos = (10, 10)

areaTickMilliseconds :: Int
areaTickMilliseconds = 100

areaBroadcastEveryTick :: Int
areaBroadcastEveryTick = 10

