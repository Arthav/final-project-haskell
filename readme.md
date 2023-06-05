# Inventory Management System

This is a simple Inventory Management System implemented in Haskell. It provides functionalities for managing inventory, performing point of sale operations, and generating transaction logs.

## Requirements

- GHC (Glasgow Haskell Compiler)
- Haskell Stack

## How to Run

1. Clone the repository:

   ```bash
   git clone https://github.com/example/repository.git
   ```

2. Change to the project directory:

   ```bash
   cd inventory-management-system
   ```

3. Build and run the project:

   ```bash
   stack run
   ```

## Code Overview

The code is organized into several modules:

- `Main`: Contains the entry point of the application and the main menu logic.
- `Types`: Defines the data types used in the application, such as `User`, `Membership`, `Transaction`, `Inventory`, etc.
- `InventoryManagement`: Contains functions related to inventory management, such as adding items, updating item quantity and price, and generating inventory logs.
- `PointOfSale`: Implements the point of sale functionality, including adding items to the cart, calculating the total price, and performing payment.
- `TransactionLog`: Provides functions for generating and downloading transaction logs.

The application uses the `StateT` and `MaybeT` monad transformers to handle the application state and possible failures.

## Usage

Upon running the application, you will be presented with a main menu where you can choose various options:

- **POS**: Enter the point of sale module to perform point of sale operations, such as adding items to the cart and completing the payment.
- **INVENTORY**: Manage the inventory by updating item quantity and price.
- **DOWNLOAD TRANSACTION LOG**: Generate and download transaction logs.
- **EXIT**: Exit the application.

Please follow the on-screen prompts and instructions to navigate through the application and perform the desired actions.

## Additional Notes

- The data is stored in memory and not persisted between application runs.
- The authentication mechanism is simple and does not involve real user authentication. It is used to simulate different user roles.
- The code includes basic error handling for invalid inputs and missing data.
- The `renderItemTable` and `renderInventoryLogsTable` functions are used to display formatted tables in the console.
- The currency formatting is applied using the `formatCurrency` function.
- The application provides a command-line interface (CLI) for interaction.

Feel free to explore the code and make any modifications or improvements as needed.