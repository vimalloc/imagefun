{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Models.Category where

import Opaleye (Column, Table(Table), required, PGInt4, PGText, pgString,
                pgInt4, optional)
import Data.Aeson
import Data.Profunctor.Product.TH (makeAdaptorAndInstance)


data Category' id name = Category
    { categoryId   :: id
    , categoryName :: name
    } deriving (Show)

type CategoryRead        = Category' Int String
type CategoryWrite       = Category' (Maybe Int) String
type CategoryColumnRead  = Category' (Column PGInt4)
                                     (Column PGText)
type CategoryColumnWrite = Category' (Maybe (Column PGInt4))
                                     (Column PGText)

instance ToJSON CategoryRead where
    toJSON post = object [ "id"        .= categoryId post
                         , "name"     .= categoryName post
                         ]

$(makeAdaptorAndInstance "pCategory" ''Category')

categoryTable :: Table CategoryColumnWrite CategoryColumnRead
categoryTable = Table "Categories" (pCategory Category
                                       { categoryId   = optional "CategoryId"
                                       , categoryName = required "Name"
                                       })

categoryToPG :: CategoryWrite -> CategoryColumnWrite
categoryToPG = pCategory Category { categoryId   = const Nothing
                                  , categoryName = pgString
                                  }

makeCategory :: String -> CategoryWrite
makeCategory name = Category Nothing name
