{-# LANGUAGE GADTs, FlexibleInstances, KindSignatures, RankNTypes, TypeSynonymInstances #-}
module MiniLang where

import Control.Monad.Identity
import Data.Monoid
import PGF hiding (Tree)

----------------------------------------------------
-- automatic translation from GF to Haskell
----------------------------------------------------

class Gf a where
  gf :: a -> Expr
  fg :: Expr -> a

instance Gf GString where
  gf (GString x) = mkStr x
  fg t =
    case unStr t of
      Just x  ->  GString x
      Nothing -> error ("no GString " ++ show t)

instance Gf GInt where
  gf (GInt x) = mkInt x
  fg t =
    case unInt t of
      Just x  ->  GInt x
      Nothing -> error ("no GInt " ++ show t)

instance Gf GFloat where
  gf (GFloat x) = mkFloat x
  fg t =
    case unFloat t of
      Just x  ->  GFloat x
      Nothing -> error ("no GFloat " ++ show t)

----------------------------------------------------
-- below this line machine-generated
----------------------------------------------------

type GA = Tree GA_
data GA_
type GAP = Tree GAP_
data GAP_
type GAdv = Tree GAdv_
data GAdv_
type GCN = Tree GCN_
data GCN_
type GCl = Tree GCl_
data GCl_
type GConj = Tree GConj_
data GConj_
type GDet = Tree GDet_
data GDet_
type GN = Tree GN_
data GN_
type GNP = Tree GNP_
data GNP_
type GPN = Tree GPN_
data GPN_
type GPol = Tree GPol_
data GPol_
type GPrep = Tree GPrep_
data GPrep_
type GPron = Tree GPron_
data GPron_
type GS = Tree GS_
data GS_
type GUtt = Tree GUtt_
data GUtt_
type GV = Tree GV_
data GV_
type GV2 = Tree GV2_
data GV2_
type GVP = Tree GVP_
data GVP_
type GString = Tree GString_
data GString_
type GInt = Tree GInt_
data GInt_
type GFloat = Tree GFloat_
data GFloat_

data Tree :: * -> * where
  LexA :: String -> Tree GA_
  GPositA :: GA -> Tree GAP_
  GPrepNP :: GPrep -> GNP -> Tree GAdv_
  LexAdv :: String -> Tree GAdv_
  GAdjCN :: GAP -> GCN -> Tree GCN_
  GUseN :: GN -> Tree GCN_
  GPredVP :: GNP -> GVP -> Tree GCl_
  LexConj :: String -> Tree GConj_
  LexDet :: String -> Tree GDet_
  LexN :: String -> Tree GN_
  GDetCN :: GDet -> GCN -> Tree GNP_
  GMassNP :: GCN -> Tree GNP_
  GUsePN :: GPN -> Tree GNP_
  GUsePron :: GPron -> Tree GNP_
  LexPN :: String -> Tree GPN_
  GPNeg :: Tree GPol_
  GPPos :: Tree GPol_
  LexPrep :: String -> Tree GPrep_
  LexPron :: String -> Tree GPron_
  GCoordS :: GConj -> GS -> GS -> Tree GS_
  GUsePresCl :: GPol -> GCl -> Tree GS_
  GUttNP :: GNP -> Tree GUtt_
  GUttS :: GS -> Tree GUtt_
  LexV :: String -> Tree GV_
  LexV2 :: String -> Tree GV2_
  GAdvVP :: GVP -> GAdv -> Tree GVP_
  GComplV2 :: GV2 -> GNP -> Tree GVP_
  GReflV2 :: GV2 -> Tree GVP_
  GUseAP :: GAP -> Tree GVP_
  GUseV :: GV -> Tree GVP_
  GString :: String -> Tree GString_
  GInt :: Int -> Tree GInt_
  GFloat :: Double -> Tree GFloat_

instance Eq (Tree a) where
  i == j = case (i,j) of
    (LexA x,LexA y) -> x == y
    (GPositA x1,GPositA y1) -> and [ x1 == y1 ]
    (GPrepNP x1 x2,GPrepNP y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (LexAdv x,LexAdv y) -> x == y
    (GAdjCN x1 x2,GAdjCN y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GUseN x1,GUseN y1) -> and [ x1 == y1 ]
    (GPredVP x1 x2,GPredVP y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (LexConj x,LexConj y) -> x == y
    (LexDet x,LexDet y) -> x == y
    (LexN x,LexN y) -> x == y
    (GDetCN x1 x2,GDetCN y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GMassNP x1,GMassNP y1) -> and [ x1 == y1 ]
    (GUsePN x1,GUsePN y1) -> and [ x1 == y1 ]
    (GUsePron x1,GUsePron y1) -> and [ x1 == y1 ]
    (LexPN x,LexPN y) -> x == y
    (GPNeg,GPNeg) -> and [ ]
    (GPPos,GPPos) -> and [ ]
    (LexPrep x,LexPrep y) -> x == y
    (LexPron x,LexPron y) -> x == y
    (GCoordS x1 x2 x3,GCoordS y1 y2 y3) -> and [ x1 == y1 , x2 == y2 , x3 == y3 ]
    (GUsePresCl x1 x2,GUsePresCl y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GUttNP x1,GUttNP y1) -> and [ x1 == y1 ]
    (GUttS x1,GUttS y1) -> and [ x1 == y1 ]
    (LexV x,LexV y) -> x == y
    (LexV2 x,LexV2 y) -> x == y
    (GAdvVP x1 x2,GAdvVP y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GComplV2 x1 x2,GComplV2 y1 y2) -> and [ x1 == y1 , x2 == y2 ]
    (GReflV2 x1,GReflV2 y1) -> and [ x1 == y1 ]
    (GUseAP x1,GUseAP y1) -> and [ x1 == y1 ]
    (GUseV x1,GUseV y1) -> and [ x1 == y1 ]
    (GString x, GString y) -> x == y
    (GInt x, GInt y) -> x == y
    (GFloat x, GFloat y) -> x == y
    _ -> False

instance Gf GA where
  gf (LexA x) = mkApp (mkCId x) []

  fg t =
    case unApp t of

      Just (i,[]) -> LexA (showCId i)
      _ -> error ("no A " ++ show t)

instance Gf GAP where
  gf (GPositA x1) = mkApp (mkCId "PositA") [gf x1]

  fg t =
    case unApp t of
      Just (i,[x1]) | i == mkCId "PositA" -> GPositA (fg x1)


      _ -> error ("no AP " ++ show t)

instance Gf GAdv where
  gf (GPrepNP x1 x2) = mkApp (mkCId "PrepNP") [gf x1, gf x2]
  gf (LexAdv x) = mkApp (mkCId x) []

  fg t =
    case unApp t of
      Just (i,[x1,x2]) | i == mkCId "PrepNP" -> GPrepNP (fg x1) (fg x2)

      Just (i,[]) -> LexAdv (showCId i)
      _ -> error ("no Adv " ++ show t)

instance Gf GCN where
  gf (GAdjCN x1 x2) = mkApp (mkCId "AdjCN") [gf x1, gf x2]
  gf (GUseN x1) = mkApp (mkCId "UseN") [gf x1]

  fg t =
    case unApp t of
      Just (i,[x1,x2]) | i == mkCId "AdjCN" -> GAdjCN (fg x1) (fg x2)
      Just (i,[x1]) | i == mkCId "UseN" -> GUseN (fg x1)


      _ -> error ("no CN " ++ show t)

instance Gf GCl where
  gf (GPredVP x1 x2) = mkApp (mkCId "PredVP") [gf x1, gf x2]

  fg t =
    case unApp t of
      Just (i,[x1,x2]) | i == mkCId "PredVP" -> GPredVP (fg x1) (fg x2)


      _ -> error ("no Cl " ++ show t)

instance Gf GConj where
  gf (LexConj x) = mkApp (mkCId x) []

  fg t =
    case unApp t of

      Just (i,[]) -> LexConj (showCId i)
      _ -> error ("no Conj " ++ show t)

instance Gf GDet where
  gf (LexDet x) = mkApp (mkCId x) []

  fg t =
    case unApp t of

      Just (i,[]) -> LexDet (showCId i)
      _ -> error ("no Det " ++ show t)

instance Gf GN where
  gf (LexN x) = mkApp (mkCId x) []

  fg t =
    case unApp t of

      Just (i,[]) -> LexN (showCId i)
      _ -> error ("no N " ++ show t)

instance Gf GNP where
  gf (GDetCN x1 x2) = mkApp (mkCId "DetCN") [gf x1, gf x2]
  gf (GMassNP x1) = mkApp (mkCId "MassNP") [gf x1]
  gf (GUsePN x1) = mkApp (mkCId "UsePN") [gf x1]
  gf (GUsePron x1) = mkApp (mkCId "UsePron") [gf x1]

  fg t =
    case unApp t of
      Just (i,[x1,x2]) | i == mkCId "DetCN" -> GDetCN (fg x1) (fg x2)
      Just (i,[x1]) | i == mkCId "MassNP" -> GMassNP (fg x1)
      Just (i,[x1]) | i == mkCId "UsePN" -> GUsePN (fg x1)
      Just (i,[x1]) | i == mkCId "UsePron" -> GUsePron (fg x1)


      _ -> error ("no NP " ++ show t)

instance Gf GPN where
  gf (LexPN x) = mkApp (mkCId x) []

  fg t =
    case unApp t of

      Just (i,[]) -> LexPN (showCId i)
      _ -> error ("no PN " ++ show t)

instance Gf GPol where
  gf GPNeg = mkApp (mkCId "PNeg") []
  gf GPPos = mkApp (mkCId "PPos") []

  fg t =
    case unApp t of
      Just (i,[]) | i == mkCId "PNeg" -> GPNeg 
      Just (i,[]) | i == mkCId "PPos" -> GPPos 


      _ -> error ("no Pol " ++ show t)

instance Gf GPrep where
  gf (LexPrep x) = mkApp (mkCId x) []

  fg t =
    case unApp t of

      Just (i,[]) -> LexPrep (showCId i)
      _ -> error ("no Prep " ++ show t)

instance Gf GPron where
  gf (LexPron x) = mkApp (mkCId x) []

  fg t =
    case unApp t of

      Just (i,[]) -> LexPron (showCId i)
      _ -> error ("no Pron " ++ show t)

instance Gf GS where
  gf (GCoordS x1 x2 x3) = mkApp (mkCId "CoordS") [gf x1, gf x2, gf x3]
  gf (GUsePresCl x1 x2) = mkApp (mkCId "UsePresCl") [gf x1, gf x2]

  fg t =
    case unApp t of
      Just (i,[x1,x2,x3]) | i == mkCId "CoordS" -> GCoordS (fg x1) (fg x2) (fg x3)
      Just (i,[x1,x2]) | i == mkCId "UsePresCl" -> GUsePresCl (fg x1) (fg x2)


      _ -> error ("no S " ++ show t)

instance Gf GUtt where
  gf (GUttNP x1) = mkApp (mkCId "UttNP") [gf x1]
  gf (GUttS x1) = mkApp (mkCId "UttS") [gf x1]

  fg t =
    case unApp t of
      Just (i,[x1]) | i == mkCId "UttNP" -> GUttNP (fg x1)
      Just (i,[x1]) | i == mkCId "UttS" -> GUttS (fg x1)


      _ -> error ("no Utt " ++ show t)

instance Gf GV where
  gf (LexV x) = mkApp (mkCId x) []

  fg t =
    case unApp t of

      Just (i,[]) -> LexV (showCId i)
      _ -> error ("no V " ++ show t)

instance Gf GV2 where
  gf (LexV2 x) = mkApp (mkCId x) []

  fg t =
    case unApp t of

      Just (i,[]) -> LexV2 (showCId i)
      _ -> error ("no V2 " ++ show t)

instance Gf GVP where
  gf (GAdvVP x1 x2) = mkApp (mkCId "AdvVP") [gf x1, gf x2]
  gf (GComplV2 x1 x2) = mkApp (mkCId "ComplV2") [gf x1, gf x2]
  gf (GReflV2 x1) = mkApp (mkCId "ReflV2") [gf x1]
  gf (GUseAP x1) = mkApp (mkCId "UseAP") [gf x1]
  gf (GUseV x1) = mkApp (mkCId "UseV") [gf x1]

  fg t =
    case unApp t of
      Just (i,[x1,x2]) | i == mkCId "AdvVP" -> GAdvVP (fg x1) (fg x2)
      Just (i,[x1,x2]) | i == mkCId "ComplV2" -> GComplV2 (fg x1) (fg x2)
      Just (i,[x1]) | i == mkCId "ReflV2" -> GReflV2 (fg x1)
      Just (i,[x1]) | i == mkCId "UseAP" -> GUseAP (fg x1)
      Just (i,[x1]) | i == mkCId "UseV" -> GUseV (fg x1)


      _ -> error ("no VP " ++ show t)


instance Compos Tree where
  compos r a f t = case t of
    GPositA x1 -> r GPositA `a` f x1
    GPrepNP x1 x2 -> r GPrepNP `a` f x1 `a` f x2
    GAdjCN x1 x2 -> r GAdjCN `a` f x1 `a` f x2
    GUseN x1 -> r GUseN `a` f x1
    GPredVP x1 x2 -> r GPredVP `a` f x1 `a` f x2
    GDetCN x1 x2 -> r GDetCN `a` f x1 `a` f x2
    GMassNP x1 -> r GMassNP `a` f x1
    GUsePN x1 -> r GUsePN `a` f x1
    GUsePron x1 -> r GUsePron `a` f x1
    GCoordS x1 x2 x3 -> r GCoordS `a` f x1 `a` f x2 `a` f x3
    GUsePresCl x1 x2 -> r GUsePresCl `a` f x1 `a` f x2
    GUttNP x1 -> r GUttNP `a` f x1
    GUttS x1 -> r GUttS `a` f x1
    GAdvVP x1 x2 -> r GAdvVP `a` f x1 `a` f x2
    GComplV2 x1 x2 -> r GComplV2 `a` f x1 `a` f x2
    GReflV2 x1 -> r GReflV2 `a` f x1
    GUseAP x1 -> r GUseAP `a` f x1
    GUseV x1 -> r GUseV `a` f x1
    _ -> r t

class Compos t where
  compos :: (forall a. a -> m a) -> (forall a b. m (a -> b) -> m a -> m b)
         -> (forall a. t a -> m (t a)) -> t c -> m (t c)

composOp :: Compos t => (forall a. t a -> t a) -> t c -> t c
composOp f = runIdentity . composOpM (Identity . f)

composOpM :: (Compos t, Monad m) => (forall a. t a -> m (t a)) -> t c -> m (t c)
composOpM = compos return ap

composOpM_ :: (Compos t, Monad m) => (forall a. t a -> m ()) -> t c -> m ()
composOpM_ = composOpFold (return ()) (>>)

composOpMonoid :: (Compos t, Monoid m) => (forall a. t a -> m) -> t c -> m
composOpMonoid = composOpFold mempty mappend

composOpMPlus :: (Compos t, MonadPlus m) => (forall a. t a -> m b) -> t c -> m b
composOpMPlus = composOpFold mzero mplus

composOpFold :: Compos t => b -> (b -> b -> b) -> (forall a. t a -> b) -> t c -> b
composOpFold z c f = unC . compos (\_ -> C z) (\(C x) (C y) -> C (c x y)) (C . f)

newtype C b a = C { unC :: b }
