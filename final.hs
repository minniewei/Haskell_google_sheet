import qualified Data.Map as Map

-- 定義 User 數據類型 
data User = User{username ::String, sheets ::Map.Map String Sheet }

-- 客製化 User 的 show 
instance Show User where show(User username sheets) = "Create a user named \"" ++ username ++ "\"."

-- 定義一個函數來創造 User 
createUser ::IO User 
createUser = do 
    username <- getLine
    return (User username Map.empty)

-- 初始化 3x3 二維陣列為 0
emptyContent ::  [[Double]]
emptyContent = replicate 3 (replicate 3 0)

-- 定義 Sheet 數據類型
data Sheet = Sheet { sheetname :: String, content :: [[Double]] }

-- 定義一個函數來創造 Sheet
createSheet :: String -> IO Sheet
createSheet sheetname = do
    return (Sheet sheetname emptyContent)

-- 客製化 Sheet 的 show
instance Show Sheet where
    show (Sheet sheetname content) = unlines (map (concatMap (\x -> showCell x ++ ", ")) content)

-- 判斷數字為整數或小數
showCell :: Double -> String
showCell value
    | value == fromInteger (round value) = show (round value)
    | otherwise = show value

-- 查找特定的 Sheet
findSheet :: Map.Map String User -> String -> String -> Maybe Sheet
findSheet userMap uname sheetName = 
    case Map.lookup uname userMap of
        Just user -> Map.lookup sheetName (sheets user)
        Nothing -> Nothing

-- 更新 Sheet 的内容
updateSheetContent :: (Int, Int) -> Double -> Sheet -> Sheet
updateSheetContent (row, col) newVal sheet = 
    let oldContent = content sheet
        newContent = take row oldContent 
                     ++ [take col (oldContent !! row) ++ [newVal] ++ drop (col + 1) (oldContent !! row)] 
                     ++ drop (row + 1) oldContent
    in sheet { content = newContent }

-- 更新 userMap 的 Sheet
updateUserSheet :: Map.Map String User -> String -> String -> (Int, Int) -> Double -> Map.Map String User
updateUserSheet userMap uname sheetName (row, col) newVal =
    case Map.lookup uname userMap of
        Just user -> 
            let updatedSheets = Map.adjust (\sheet -> updateSheetContent (row, col) newVal sheet) sheetName (sheets user)
                updatedUser = user { sheets = updatedSheets }
            in Map.insert uname updatedUser userMap
        Nothing -> userMap

-- 主函数
main :: IO ()
main = do
    let userMap = Map.empty  -- 創建一個空的 Map 來存放用戶
    mainLoop userMap

-- 主循環函數
mainLoop :: Map.Map String User -> IO ()
mainLoop userMap = do
    putStrLn "---------------Menu---------------"
    putStrLn "1. Create a user"
    putStrLn "2. Create a sheet"
    putStrLn "3. Check a sheet"
    putStrLn "4. Change a value in a sheet"
    putStrLn "5. Change a sheet's access right."
    putStrLn "6. Collaborate with an other user"
    putStrLn "-----------------------------------"
    choose <- getLine
    case choose of
        "1" -> do
            user <- createUser
            putStrLn $ show user
            mainLoop (Map.insert (username user) user userMap)
        "2" -> do
            input <- getLine
            let [username, sheetName] = words input
            case Map.lookup username userMap of
                Just oldUser -> do
                    sheet <- createSheet sheetName
                    let updatedSheets = Map.insert sheetName sheet (sheets oldUser)
                    let updatedUser = oldUser { sheets = updatedSheets }
                    let updatedUserMap = Map.insert username updatedUser userMap
                    putStrLn $ "Create a sheet named \"" ++sheetName++ "\" for \"" ++username++"\"."
                    mainLoop updatedUserMap
                Nothing -> do
                    putStrLn $ "User " ++ username ++ " does not exist."
                    mainLoop userMap
        "3" -> do
            input <- getLine
            let [username, sheetName] = words input
            case findSheet userMap username sheetName of
                Just sheet -> putStrLn $ show sheet
                Nothing -> putStrLn "User or sheet not found."
            mainLoop userMap        
        "4" -> do
            input <- getLine
            let [username, sheetName] = words input
            case findSheet userMap username sheetName of
                Just sheet -> putStrLn $ show sheet
                Nothing -> putStrLn "User or sheet not found."
            input <- getLine
            let [row, col] = map read (take 2 $ words input) :: [Int]
            let value = read (last $ words input) :: Double
            let updatedUserMap = updateUserSheet userMap username sheetName (row, col) value
            case findSheet updatedUserMap username sheetName of
                Just sheet -> putStrLn $ show sheet
                Nothing -> putStrLn "User or sheet not found."
            mainLoop updatedUserMap
