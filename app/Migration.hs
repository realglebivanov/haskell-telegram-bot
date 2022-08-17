module Migration where

import Models.Book
import Repository

main :: IO ()
main = exec $ runMigration migrateAll