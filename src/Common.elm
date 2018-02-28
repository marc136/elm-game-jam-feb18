module Common exposing (..)

{-| Common functions used in multiple modules
-}


rowToY : Int -> Float
rowToY row =
    13 - (toFloat row * 5)
