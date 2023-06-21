module Main where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.State (StateT (runStateT), get, modify, put, state, execStateT, evalStateT)
import Data.Data (Data, Typeable)
import Data.Time
import System.IO
import Data.List (intercalate, maximumBy, sortBy)
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
  { userId :: Int,
    name :: String,
    username :: String,
    password :: String
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

data ActionLog = Insert | Update | Delete
  deriving (Show, Eq, Read)

data InventoryLog = InventoryLog
  { adminId :: Int,
    priceBefore :: Int,
    priceAfter :: Int,
    qtyBefore :: Int,
    qtyAfter :: Int,
    changeDate :: UTCTime,
    action :: ActionLog
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
    inventories :: [Inventory],
    inventoryLogs :: [InventoryLog],
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
      sortedInventories = sortBy (comparing itemId) (inventories db)
      rows    = map (\(Inventory itemId itemName itemQuantity itemPrice) -> show itemId ++ "\t\t" ++ itemName ++ "\t\t\t" ++ show itemQuantity ++ "\t\t\t" ++ show itemPrice) sortedInventories
  in unlines (border1:header:border2:rows)

renderInventoryLogsTable :: Database -> String
renderInventoryLogsTable db =
  let border1 = "----------------------------------------------------------------------------------------------"
      header  = "ADMIN ID RESPONSIBLE\tPRICE BEFORE\tPRICE AFTER\tQTY BEFORE\tQTY AFTER\tCHANGE DATE\tACTION"
      border2 = "----------------------------------------------------------------------------------------------"
      rows    = map formatInventoryLog (inventoryLogs db)
      formattedRows = unlines rows
  in unlines [border1, header, border2, formattedRows]
  where
    formatInventoryLog :: InventoryLog -> String
    formatInventoryLog (InventoryLog adminId priceBefore priceAfter qtyBefore qtyAfter changeDate action) =
      let formattedDate = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" changeDate
      in show adminId ++ "\t\t" ++ show priceBefore ++ "\t\t" ++ show priceAfter ++ "\t\t" ++
         show qtyBefore ++ "\t\t" ++ show qtyAfter ++ "\t\t" ++ formattedDate ++ "\t\t" ++ show action

addItemToCart :: StateT Database (MaybeT IO) ()
addItemToCart = do
  db <- get
  let inv = inventories db
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
  let inv = inventories db
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
          updatedInventory = deductInventoryQuantity cartItems (inventories db)
      put (db { transactions = updatedTransactions, inventories = updatedInventory })
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
  liftIO $ putStrLn "==== MANAGE INVENTORY ===="
  liftIO $ putStrLn "1. Create new item"
  liftIO $ putStrLn "2. Update item"
  liftIO $ putStrLn "3. Delete item"
  liftIO $ putStrLn "4. Read inventory"
  liftIO $ putStrLn "0. Go back to the main menu"
  choice <- liftIO $ promptInt "Enter your choice: "
  case choice of
    0 -> mainMenu
    1 -> createItem
    2 -> updateItem
    3 -> deleteItem
    4 -> readInventory
    _ -> do
      liftIO $ putStrLn "Invalid choice. Please try again."
      manageInventory

getNextItemId :: [Inventory] -> Int
getNextItemId [] = 1
getNextItemId items = maximum (map itemId items) + 1

addItemToInventory :: Inventory -> [Inventory] -> [Inventory]
addItemToInventory item inventory = item : inventory

createItem :: StateT Database (MaybeT IO) ()
createItem = do
  db <- get
  let inv = inventories db
  liftIO $ putStrLn "==== CREATE ITEM ===="
  itemName <- liftIO $ promptString "Enter item name: "
  itemPrice <- liftIO $ promptInt "Enter item price: "
  itemQuantity <- liftIO $ promptInt "Enter item quantity: "
  let newItem = Inventory (getNextItemId inv) itemName itemPrice itemQuantity
  modify (\s -> s { inventories = addItemToInventory newItem inv })
  liftIO $ putStrLn "Item created."
  addToInventoryLog newItem Insert
  liftIO $ saveAllData db
  manageInventory

updateItem :: StateT Database (MaybeT IO) ()
updateItem = do
  db <- get
  let inv = inventories db
  liftIO $ putStrLn "==== UPDATE ITEM ===="
  liftIO $ putStrLn (renderItemTable db)
  itemId <- liftIO $ promptInt "Enter item ID (or 0 to go back to the manage inventory menu): "
  if itemId == 0
    then manageInventory
    else do
      case lookupInventoryItem itemId inv of
        Nothing -> do
          liftIO $ putStrLn "Invalid item ID. Please try again."
          updateItem
        Just item -> do
          let itemNames = maybeToString (getItemNameFromInventory itemId inv)
          updatedPrice <- liftIO $ promptInt "Enter item updated price: "
          updatedQuantity <- liftIO $ promptInt "Enter item updated quantity: "
          let updatedItem = Inventory { itemId = itemId, itemQuantity = updatedQuantity, itemPrice = updatedPrice, itemName = itemNames }
          updateInventoryItem itemId updatedItem
          liftIO $ putStrLn "Item quantity and price updated."
          addToInventoryLog updatedItem Update
          liftIO $ saveAllData db
          manageInventory

getItemNameFromInventory :: Int -> [Inventory] -> Maybe String
getItemNameFromInventory _ [] = Nothing
getItemNameFromInventory itemIds (inv:invs)
  | itemIds == itemId inv = Just (itemName inv)
  | otherwise = getItemNameFromInventory itemIds invs

deleteInventoryItem :: Int -> [Inventory] -> [Inventory]
deleteInventoryItem _ [] = []
deleteInventoryItem itemIds (item:items)
  | itemIds == itemId item = items
  | otherwise = item : deleteInventoryItem itemIds items

deleteItem :: StateT Database (MaybeT IO) ()
deleteItem = do
  db <- get
  let inv = inventories db
  liftIO $ putStrLn "==== DELETE ITEM ===="
  liftIO $ putStrLn (renderItemTable db)
  itemId <- liftIO $ promptInt "Enter item ID (or 0 to go back to the manage inventory menu): "
  if itemId == 0
    then manageInventory
    else do
      case lookupInventoryItem itemId inv of
        Nothing -> do
          liftIO $ putStrLn "Invalid item ID. Please try again."
          deleteItem
        Just deletedItem -> do
          modify (\s -> s { inventories = deleteInventoryItem itemId inv })
          liftIO $ putStrLn "Item deleted."
          addToInventoryLog deletedItem Delete
          liftIO $ saveAllData db
          manageInventory

readInventory :: StateT Database (MaybeT IO) ()
readInventory = do
  db <- get
  let inv = inventories db
  liftIO $ putStrLn "==== READ INVENTORY ===="
  liftIO $ putStrLn (renderItemTable db)
  manageInventory

updateInventoryItem :: Int -> Inventory -> StateT Database (MaybeT IO) ()
updateInventoryItem itemIdTarget updatedItem = do
  modify (\db -> db { inventories = updateInventoryItem' (inventories db) })
  where
    updateInventoryItem' :: [Inventory] -> [Inventory]
    updateInventoryItem' inv = map (\item -> if itemIdTarget == itemId item then updatedItem else item) inv

getItemPrice :: Int -> [Inventory] -> Int
getItemPrice _ [] = 0  
getItemPrice itemIds (item : rest)
  | itemIds == itemId item = itemPrice item
  | otherwise = getItemPrice itemIds rest

addToInventoryLog :: Inventory -> ActionLog -> StateT Database (MaybeT IO) ()
addToInventoryLog updatedItem action = do
  currentTime <- liftIO getCurrentTime
  db <- get
  let currentLoginState = loginState db
  let adminId = userLoginId currentLoginState
  let invLog = inventoryLogs db
      inventoryLogEntry = InventoryLog
        { adminId = adminId, -- Set the appropriate admin ID
          priceBefore = case action of
            Insert -> 0
            Update -> getItemPrice (itemId updatedItem) (inventories db)
            Delete -> getItemPrice (itemId updatedItem) (inventories db),
          priceAfter = getItemPrice (itemId updatedItem) (inventories db),
          qtyBefore = case action of
            Insert -> 0
            Update -> itemQuantity updatedItem
            Delete -> itemQuantity updatedItem,
          qtyAfter = itemQuantity updatedItem,
          changeDate = currentTime,
          action = action
        }
      updatedInventoryLog = inventoryLogEntry : invLog
  modify (\db' -> db' { inventoryLogs = updatedInventoryLog }) 
  liftIO $ saveAllData db

mainMenu :: StateT Database (MaybeT IO) ()
mainMenu = do
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
authenticateAndProceed userIds = do
  db <- liftIO loadAllData
  let matchingUsers = filter (\user -> userIds == userId user) (users db)
  case matchingUsers of
    [] -> do
      liftIO $ putStrLn "User not found."
      fail "User not found."
    (user : _) -> do
      let updatedDb = db { loginState = LoginState { authenticated = True, userLoginId = userIds } }
      liftIO $ putStrLn $ "Logged in as " ++ name user ++ "."
      put updatedDb
      mainMenu

authenticateUser :: StateT Database (MaybeT IO) (Maybe Int)
authenticateUser = do
  db <- get
  usernameInput <- liftIO $ promptString "Enter username: "
  passwordInput <- liftIO $ promptString "Enter password: "
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
      inventories = [],
      inventoryLogs = [],
      loginState = LoginState {authenticated = False, userLoginId=0},
      transactionItems = []
    }

main :: IO ()
main = do
  putStrLn "====================================="
  putStrLn "POS AND INVENTORY..."
  putStrLn "====================================="
  maybeResult <- runMaybeT (evalStateT readDBFromFile initialDB)
  case maybeResult of
    Just () -> do
      putStrLn "Read database from file successfully."
      maybeDb <- runMaybeT (evalStateT (get :: StateT Database (MaybeT IO) Database) initialDB)
      case maybeDb of
        Just db -> do
          void $ runMaybeT (evalStateT (userVerification db) db)
        Nothing -> putStrLn "Failed to retrieve database."
    Nothing -> putStrLn "Failed to read database from file."

userVerification :: Database -> StateT Database (MaybeT IO) ()
userVerification db = do
  maybeUserId <- authenticateUser
  case maybeUserId of
    Just userId -> void $ authenticateAndProceed userId
    Nothing -> do
      liftIO $ putStrLn "Invalid credentials. Please try again."
      userVerification db

printUsers :: [User] -> IO ()
printUsers [] = putStrLn "No users in the database."
printUsers users = mapM_ (putStrLn . show) users

saveAllData :: Database -> IO ()
saveAllData db = writeFile "database.txt" (show db)

loadAllData :: IO Database
loadAllData = do
  fileExists <- doesFileExist "database.txt"
  if fileExists
    then do
      contents <- readFile "database.txt"
      case readMaybe contents of
        Just db -> return db
        Nothing -> do
          putStrLn "Invalid database file. Creating a new database."
          return initialDB
    else do
      putStrLn "Database file not found. Creating a new database."
      return initialDB

readDBFromFile :: StateT Database (MaybeT IO) ()
readDBFromFile = put initialDB

downloadTransaction :: StateT Database (MaybeT IO) ()
downloadTransaction = do
  liftIO $ putStrLn "==== DOWNLOAD TRANSACTION ===="
  db <- get
  let transactionsData = transactions db
  let formattedData = formatTransactionData transactionsData
  liftIO $ writeFile "transactionLog.txt" formattedData
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

maybeToString :: Maybe String -> String
maybeToString maybeValue = case maybeValue of
  Just value -> value
  Nothing    -> "" 

