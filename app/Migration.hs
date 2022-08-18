module Main where

import Database.Persist.Postgresql (runMigration)
import Models.Book
import Repository

main :: IO ()
main = exec $ runMigration migrateAll