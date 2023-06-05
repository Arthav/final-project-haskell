import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.State (StateT (runStateT), get, modify, put, state, execStateT, evalStateT)
import Data.Data (Data, Typeable)
import Data.Time
import System.IO
import Data.List (intercalate, maximumBy)
import Data.Ord (comparing)
import Control.Exception (catch)
import System.IO.Error (isDoesNotExistError)
import Data.Time.Clock (getCurrentTime)
import System.Exit (exitFailure)
import Data.Time.Format
import System.Directory (doesFileExist)
import Control.Monad (void)
import Control.Monad.Trans.Class (lift)
import Text.Read (readMaybe)


data User = User
  { 
    userId :: Int,
    name :: String,
    username :: String,
    password :: String
  }
  deriving (Show, Read)

data Membership = Membership
  { mname :: String,
    mage :: Int,
    phone :: String
  }
  deriving (Show, Read)

data TransactionItem = TransactionItem 
  {
    purchasedItemId :: Int,
    qtyBought :: Int,
    price :: Int
  }
  deriving (Show, Read)

data Transaction = Transaction
  { transactionId :: Int,
    transactionDate :: String,
    transactionItem :: [TransactionItem],
    totalPrice :: Int,
    cashierId :: Int
  }
  deriving (Show, Read)

data LoginState = LoginState
  { authenticated :: Bool,
    userLoginId :: Int
  }
  deriving (Show, Read)

data InventoryLog = InventoryLog
  { adminId :: Int,
    priceBefore :: Double,
    priceAfter :: Double,
    qtyBefore :: Int,
    qtyAfter :: Int,
    changeDate :: UTCTime
  }
  deriving (Show, Read)

data Inventory = Inventory
  { itemId :: Int,
    itemName :: String,
    itemQuantity :: Int,
    itemPrice :: Int
  }
  deriving (Show, Read)

data Database = Database
  { users :: [User],
    transactions :: [Transaction],
    inventory :: [Inventory],
    inventoryLog :: [InventoryLog],
    membership :: [Membership],
    loginState :: LoginState,
    transactionItems :: [TransactionItem]
  }deriving (Show, Read)

createUser :: User -> StateT Database (MaybeT IO) ()
createUser user = modify (\db -> db {users = user : users db})

readUser :: String -> StateT Database (MaybeT IO) User
readUser input = do
  db <- get
  let matchingUsers = filter (\user -> input == name user) (users db)
  case matchingUsers of
    [] -> liftIO $ putStrLn "User not found." >> fail "User not found."
    (p : _) -> return p

promptString :: String -> IO String
promptString prompt = do
  putStr prompt
  hFlush stdout
  getLine

promptInt :: String -> IO Int
promptInt prompt = do
  str <- promptString prompt
  case reads str of
    [(x, "")] -> return x
    _ -> do
      putStrLn "Invalid input. Please enter an integer."
      promptInt prompt

renderTransactionItem :: Database -> String
renderTransactionItem db =
  let border1 = "--------------------------------------------------------------------------------"
      header = "ITEM ID\t\tQTY BOUGHT\t\tPRICE"
      border2 = "--------------------------------------------------------------------------------"
      rows = map renderTransactionItemRow (transactionItems db)
  in unlines (border1 : header : border2 : rows)

renderTransactionItemRow :: TransactionItem -> String
renderTransactionItemRow (TransactionItem itemId qtyBought itemPrice) =
  show itemId ++ "\t\t" ++ show qtyBought ++ "\t\t\t" ++ show itemPrice

renderItemTable :: Database -> String
renderItemTable db =
  let border1 = "--------------------------------------------------------------------------------"
      header  = "ITEM ID\t\tITEM NAME\t\tAVAILABLE STOCK\t\tITEM PRICE"
      border2 = "--------------------------------------------------------------------------------"
      rows    = map (\(Inventory itemId itemName itemQuantity itemPrice) -> show itemId ++ "\t\t" ++ itemName ++ "\t\t\t" ++ show itemQuantity ++ "\t\t\t" ++ show itemPrice) (inventory db)
  in unlines (border1:header:border2:rows)

renderInventoryLogsTable :: Database -> String
renderInventoryLogsTable db =
  let border1 = "----------------------------------------------------------------------------------------------"
      header  = "ADMIN ID RESPONSIBLE\tPRICE BEFORE\tPRICE AFTER\tQTY BEFORE\tQTY AFTER\tCHANGE DATE"
      border2 = "----------------------------------------------------------------------------------------------"
      rows    = map formatInventoryLog (inventoryLog db)
      formattedRows = unlines rows
  in unlines [border1, header, border2, formattedRows]
  where
    formatInventoryLog :: InventoryLog -> String
    formatInventoryLog (InventoryLog adminId priceBefore priceAfter qtyBefore qtyAfter changeDate) =
      let formattedDate = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" changeDate
      in show adminId ++ "\t\t" ++ show priceBefore ++ "\t\t" ++ show priceAfter ++ "\t\t" ++
         show qtyBefore ++ "\t\t" ++ show qtyAfter ++ "\t\t" ++ formattedDate

addItemToCart :: StateT Database (MaybeT IO) ()
addItemToCart = do
  db <- get
  let inv = inventory db
  liftIO $ putStrLn "==== ADD ITEM TO CART ===="
  liftIO $ putStrLn (renderItemTable db)
  itemId <- liftIO $ promptInt "Enter item ID: "
  quantity <- liftIO $ promptInt "Enter quantity or 0 to go back: "
  if quantity == 0
    then do
      liftIO $ putStrLn "Returning to point of sale."
      pointsales
    else do
      case lookupInventoryItem itemId inv of
        Nothing -> do
          liftIO $ putStrLn "Invalid item ID. Please try again."
          addItemToCart
        Just (Inventory _ itemName availableStock itemPrice) ->
          if quantity > availableStock
            then do
              liftIO $ putStrLn "Insufficient stock. Please try again."
              addItemToCart
            else do
              let updatedTransactionItem = TransactionItem itemId quantity itemPrice
              modify (\db -> db { transactionItems = updatedTransactionItem : transactionItems db })
              liftIO $ putStrLn "Item added to cart."
              pointsales

calculateTotalPrice :: Database -> Int
calculateTotalPrice db =
  let inv = inventory db
      items = transactionItems db
      totalPrice = sum [calculateItemPrice itemId qty inv | TransactionItem itemId qty _ <- items]
  in totalPrice

calculateItemPrice :: Int -> Int -> [Inventory] -> Int
calculateItemPrice itemId quantity inv =
  case lookupInventoryItem itemId inv of
    Nothing -> 0
    Just (Inventory _ _ _ itemPrice) -> itemPrice * quantity

lookupInventoryItem :: Int -> [Inventory] -> Maybe Inventory
lookupInventoryItem targetItemId inv = 
  case filter (\item -> targetItemId == itemId item) inv of
    [] -> Nothing
    (item:_) -> Just item

formatCurrency :: Int -> String
formatCurrency price =
  let reversedStr = reverse (show price)
      groups = groupBy3 reversedStr
  in reverse (intercalate "." groups)

groupBy3 :: String -> [String]
groupBy3 [] = []
groupBy3 xs = take 3 xs : groupBy3 (drop 3 xs)

generateTransactionId :: StateT Database (MaybeT IO) Int
generateTransactionId = do
  db <- get
  let transactionIds = map transactionId (transactions db)
      lastTransactionId = if null transactionIds then 0 else maximum transactionIds
  return (lastTransactionId + 1)
  
goToPayment :: StateT Database (MaybeT IO) ()
goToPayment = do
  liftIO $ putStrLn "==== PAYMENT ===="
  db <- get
  liftIO $ putStrLn (renderTransactionItem db)
  let cartItems = transactionItems db
  let totalPrice = calculateTotalPrice db
  liftIO $ putStrLn ("Total Price: Rp. " ++ formatCurrency totalPrice)  -- Format totalPrice as currency
  choice <- liftIO $ promptInt "1. Payment done\n2. Go back to POS\nEnter your choice: "
  case choice of
    1 -> do
      transactionId <- generateTransactionId
      currentTime <- liftIO getCurrentTime
      let transactionDate = formatTime defaultTimeLocale "%Y-%m-%d" currentTime
      liftIO $ putStrLn "Payment completed."
      let currentLoginState = loginState db
      let cashierId = userLoginId currentLoginState
      let transactionItemsList = [TransactionItem { purchasedItemId = purchasedItemId item, qtyBought = qtyBought item, price = price item } | item <- cartItems]
          newTransaction = Transaction { transactionId = transactionId, transactionDate = transactionDate, transactionItem = transactionItemsList, totalPrice = totalPrice, cashierId = cashierId }
          updatedTransactions = newTransaction : transactions db
          updatedInventory = deductInventoryQuantity cartItems (inventory db)
      put (db { transactions = updatedTransactions, inventory = updatedInventory })
      clearCart
      liftIO $ saveAllData db
      pointsales
    2 -> pointsales
    _ -> do
      liftIO $ putStrLn "Invalid choice. Please try again."
      goToPayment

deductInventoryQuantity :: [TransactionItem] -> [Inventory] -> [Inventory]
deductInventoryQuantity [] inventory = inventory
deductInventoryQuantity (item : items) inventory =
  let itemId = purchasedItemId item
      qtyBoughts = qtyBought item
      updatedInventory = deductItemQuantity itemId qtyBoughts inventory
  in deductInventoryQuantity items updatedInventory

deductItemQuantity :: Int -> Int -> [Inventory] -> [Inventory]
deductItemQuantity _ _ [] = []
deductItemQuantity itemIds qtyToDeduct (item : items)
  | itemIds == itemId item =
    let updatedItem = item { itemQuantity = itemQuantity item - qtyToDeduct }
    in if itemQuantity updatedItem < 0
         then items
         else updatedItem : items
  | otherwise = item : deductItemQuantity itemIds qtyToDeduct items

clearCart :: StateT Database (MaybeT IO) ()
clearCart = do
  liftIO $ putStrLn "==== CLEAR CART ===="
  modify (\db -> db { transactionItems = [] })  -- Clear the transactionItem table
  liftIO $ putStrLn "Cart cleared."
  pointsales

pointsales :: StateT Database (MaybeT IO) ()
pointsales = do
  liftIO $ putStrLn "==== POINT OF SALES ===="
  liftIO $ putStrLn "1. Add item to cart"
  liftIO $ putStrLn "2. Go to payment"
  liftIO $ putStrLn "3. Remove all cart data"
  liftIO $ putStrLn "4. Back to main menu"
  choice <- liftIO $ promptInt "Enter your choice: "
  case choice of
    1 -> do
      addItemToCart
    2 -> do
      goToPayment
    3 -> do
      clearCart
    4 -> mainMenu
    _ -> do
      liftIO $ putStrLn "Invalid choice. Please try again."
      pointsales

manageInventory :: StateT Database (MaybeT IO) ()
manageInventory = do
  db <- get
  let inv = inventory db
  liftIO $ putStrLn "==== MANAGE INVENTORY ===="
  liftIO $ putStrLn (renderItemTable db)
  itemId <- liftIO $ promptInt "Enter item ID (or 0 to go back to the main menu): "
  if itemId == 0
    then mainMenu
    else do
      updatedPrice <- liftIO $ promptInt "Enter item updated price: "
      updatedQuantity <- liftIO $ promptInt "Enter item updated quantity: "
      case lookupInventoryItem itemId inv of
        Nothing -> do
          liftIO $ putStrLn "Invalid item ID. Please try again."
          manageInventory
        Just item -> do
          let updatedItem = item { itemQuantity = updatedQuantity, itemPrice = updatedPrice }
          updateInventoryItem itemId updatedItem
          liftIO $ putStrLn "Item quantity and price updated."
          liftIO $ saveAllData db
          mainMenu

updateInventoryItem :: Int -> Inventory -> StateT Database (MaybeT IO) ()
updateInventoryItem itemIdTarget updatedItem = do
  db <- get
  let inv = inventory db
      updatedInventory = map (\item -> if itemIdTarget == itemId item then updatedItem else item) inv
  modify (\db' -> db' { inventory = updatedInventory })
  liftIO $ putStrLn (renderItemTable db)
  addToInventoryLog updatedItem
  liftIO $ saveAllData db

addToInventoryLog :: Inventory -> StateT Database (MaybeT IO) ()
addToInventoryLog updatedItem = do
  currentTime <- liftIO getCurrentTime
  db <- get
  let invLog = inventoryLog db
      inventoryLogEntry = InventoryLog
        { adminId = 0, -- Set the appropriate admin ID
          priceBefore = 0.0, -- Set the appropriate price before update
          priceAfter = 0.0, -- Set the appropriate price after update
          qtyBefore = itemQuantity updatedItem,
          qtyAfter = itemQuantity updatedItem,
          changeDate = currentTime
        }
      updatedInventoryLog = inventoryLogEntry : invLog
  modify (\db' -> db' { inventoryLog = updatedInventoryLog }) 
  liftIO $ saveAllData db  

mainMenu :: StateT Database (MaybeT IO) ()
mainMenu = do
  db <- readDBFromFile
  liftIO $ putStrLn "==== Main Menu ===="
  liftIO $ putStrLn "0. Exit"
  liftIO $ putStrLn "1. POS"
  liftIO $ putStrLn "2. INVENTORY"
  liftIO $ putStrLn "3. DOWNLOAD TRANSACTION LOG"
  liftIO $ putStrLn "4. check log"
  liftIO $ putStrLn "5. Save database"
  choice <- liftIO $ promptInt "Enter your choice: "
  case choice of
    0 -> liftIO $ putStrLn "Program shut down!"
    1 -> pointsales
    2 -> manageInventory
    3 -> downloadTransaction
    4 -> do
      db <- get
      liftIO $ putStrLn (renderInventoryLogsTable db)
      mainMenu
    5 -> do
      db <- get
      liftIO $ saveAllData db
      mainMenu
    6 -> do
      db <- get  
      let login = loginState db  
      liftIO $ print login  
      mainMenu
    7 -> do
      db <- get  
      liftIO $ print db 
      mainMenu
    _ -> do
      lift $ liftIO $ putStrLn "Invalid choice. Please try again."
      mainMenu

authenticateAndProceed :: Int -> StateT Database (MaybeT IO) ()
authenticateAndProceed userId = do
  db <- get
  let updatedDb = db { loginState = LoginState { authenticated = True, userLoginId = userId } }
  result <- lift $ runMaybeT $ lift $ runStateT mainMenu updatedDb
  case result of
    Just (_, finalDatabase) -> liftIO $ putStrLn "Application completed."
    Nothing -> liftIO $ do
      putStrLn "Application terminated unexpectedly."
      exitFailure

authenticateUser :: StateT Database (MaybeT IO) (Maybe Int)
authenticateUser = do
  usernameInput <- liftIO $ promptString "Enter username: "
  passwordInput <- liftIO $ promptString "Enter password: "
  db <- get
  let matchingUsers = filter (\user -> usernameInput == username user && passwordInput == password user) (users db)
  case matchingUsers of
    [user] -> return (Just (userId user))
    _      -> do
      lift $ MaybeT $ return Nothing

initialDB :: Database
initialDB =
  Database
    { users =[ User
            { 
              userId = 1,
              name = "admin",
              username = "admin",
              password = "123"
            },
             User
            {
              userId = 2,
               name = "Cashier1",
              username = "Cash",
              password = "123"
            }
        ],
      transactions = [],
      inventory = [],
      inventoryLog = [],
      membership = [],
      loginState = LoginState {authenticated = False, userLoginId=0},
      transactionItems = []
    }

main :: IO ()
main = do
  putStrLn "POS AND INVENTORY..."
  maybeResult <- runMaybeT (evalStateT readDBFromFile initialDB)
  case maybeResult of
    Just () -> do
      putStrLn "Read database from file successfully."
      userVerification
    Nothing -> putStrLn "Failed to read database from file."

userVerification :: IO ()
userVerification = do
  maybeUserId <- runMaybeT (evalStateT authenticateUser initialDB)
  case maybeUserId of
    Just userId -> void $ runMaybeT $ evalStateT (authenticateAndProceed (maybeToInt userId)) initialDB
    Nothing -> do
      putStrLn "Invalid credentials. Please try again."
      userVerification

printUsers :: [User] -> IO ()
printUsers [] = putStrLn "No users in the database."
printUsers users = mapM_ (putStrLn . show) users

saveAllData :: Database -> IO ()
saveAllData db = writeFile "database.txt" (show db)

readDBFromFile :: StateT Database (MaybeT IO) ()
readDBFromFile = do
  dbExists <- liftIO $ doesFileExist "database.txt"
  if dbExists
    then readDBFromFileIfExists "database.txt"
    else readDBFromInitialFile 

readDBFromFileIfExists :: FilePath -> StateT Database (MaybeT IO) ()
readDBFromFileIfExists filePath = do
  contents <- liftIO $ readFile filePath
  let maybeDb = readMaybe contents :: Maybe Database
  case maybeDb of
    Just db -> put db
    Nothing -> liftIO $ putStrLn "Failed to parse database from file."

readDBFromInitialFile :: StateT Database (MaybeT IO) ()
readDBFromInitialFile = do
  contents <- liftIO $ readFile "initialDatabase.txt"
  let maybeDb = readMaybe contents :: Maybe Database
  case maybeDb of
    Just db -> put db
    Nothing -> liftIO $ putStrLn "Failed to parse database from file."

downloadTransaction :: StateT Database (MaybeT IO) ()
downloadTransaction = do
  liftIO $ putStrLn "==== DOWNLOAD TRANSACTION ===="
  db <- get
  let transactionsData = transactions db
  let formattedData = formatTransactionData transactionsData
  liftIO $ writeFile "transaction.txt" formattedData
  liftIO $ putStrLn "Transaction database downloaded and saved successfully."
  mainMenu

formatTransactionData :: [Transaction] -> String
formatTransactionData transactionsData =
  let border1 = "--------------------------------------------------------------------------------"
      header = "TRANSACTION ID\t\tTRANSACTION DATE\t\tTOTAL PRICE\t\tCASHIER ID"
      border2 = "--------------------------------------------------------------------------------"
      rows = map formatTransactionRow transactionsData
  in unlines (border1 : header : border2 : rows)

formatTransactionRow :: Transaction -> String
formatTransactionRow (Transaction transactionId transactionDate _ totalPrice cashierId) =
  show transactionId ++ "\t\t\t\t" ++ transactionDate ++ "\t\t\t\t\t" ++ show totalPrice ++ "\t\t\t\t\t" ++ show cashierId

maybeToInt :: Maybe Int -> Int
maybeToInt maybeValue = case maybeValue of
  Just value -> value
  Nothing    -> 0 

