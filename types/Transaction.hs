-- Transaction.hs

module TransactionModule (Transaction, transactionItem) where


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
