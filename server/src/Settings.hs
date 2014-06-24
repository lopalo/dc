module Settings where

debug :: Bool
debug = True

areas :: [String]
areas = ["start_area"]

startArea :: String
startArea = "start_area"

areaUserSpeed :: Int
areaUserSpeed = 30 --units per second

initUserDurability :: Int
initUserDurability = 100

startAreaPos :: (Int, Int)
startAreaPos = (10, 10)

areaTickMilliseconds :: Int
areaTickMilliseconds = 100

areaBroadcastEveryTick :: Int
areaBroadcastEveryTick = 40

