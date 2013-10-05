module CurveAST where

type Program = [Def]
data Def = Def Ident Curve [Def] deriving (Eq, Show)
data Curve = Connect Curve Curve
           | Over Curve Curve
           | Translate Curve Point
           | Scale Curve Expr
           | Refv Curve Expr
           | Refh Curve Expr
           | Rot Curve Expr
           | Single Point
           | Id Ident
           deriving (Eq, Show)
data Point = Point Expr Expr deriving (Eq, Show)
data Expr = Mult Expr Expr
          | Add Expr Expr
          | Width Curve
          | Height Curve
          | Const Number
          deriving (Eq, Show)
type Ident = String
type Number = Double
