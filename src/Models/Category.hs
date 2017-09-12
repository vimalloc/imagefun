{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Models.Category where

import           Data.Aeson (ToJSON, (.=), toJSON, object, FromJSON, parseJSON,
                             Value(Object), (.:?), (.:), withObject)
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

-- Even if "id" gets passed in here (which it is not required), the
-- categoryToCategoryColumn function will insure that it gets erased, so
-- we don't have any security concerns that pop up as a result of that.
instance FromJSON CategoryWrite where
    parseJSON = withObject "CategoryWrite" $ \c -> Category
        <$> c .:? "id"
        <*> c .:  "name"

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

categoryToCategoryColumn :: CategoryWrite -> CategoryColumnWrite
categoryToCategoryColumn = pCategory Category { categoryId   = const Nothing
                                              , categoryName = O.pgString
                                              }

-- TODO this name sucks. Come up with a better name (at least until
--      https://github.com/tomjaguarpaw/haskell-opaleye/issues/92 is resolved)
categoryToCategoryColumnId :: Int -> CategoryWrite -> CategoryColumnWrite
categoryToCategoryColumnId catId  = pCategory Category { categoryId   = const $ Just (O.pgInt4 catId)
                                                       , categoryName = O.pgString
                                                       }
