{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Models.Category where

import           Data.Aeson (ToJSON, (.=), toJSON, object)
import           Data.Profunctor.Product.TH (makeAdaptorAndInstance)
import qualified Opaleye as O


data Category' id name = Category
    { categoryId   :: id
    , categoryName :: name
    } deriving (Show)

type CategoryRead        = Category' Int String
type CategoryWrite       = Category' (Maybe Int) String
type CategoryColumnRead  = Category' (O.Column O.PGInt4)
                                     (O.Column O.PGText)
type CategoryColumnWrite = Category' (Maybe (O.Column O.PGInt4))
                                     (O.Column O.PGText)

instance ToJSON CategoryRead where
    toJSON post = object [ "id"   .= categoryId post
                         , "name" .= categoryName post
                         ]

$(makeAdaptorAndInstance "pCategory" ''Category')

categoryTable :: O.Table CategoryColumnWrite CategoryColumnRead
categoryTable = O.Table "Categories" (pCategory Category
                                       { categoryId   = O.optional "CategoryId"
                                       , categoryName = O.required "Name"
                                       })

categoryToPG :: CategoryWrite -> CategoryColumnWrite
categoryToPG = pCategory Category { categoryId   = const Nothing
                                  , categoryName = O.pgString
                                  }

makeCategory :: String -> CategoryWrite
makeCategory name = Category Nothing name
