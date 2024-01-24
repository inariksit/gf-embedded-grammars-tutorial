module MiniLang where

import PGF2 hiding (Tree)

showCId :: CId -> String
showCId = id

----------------------------------------------------
-- automatic translation from GF to Haskell
----------------------------------------------------

class Gf a where
  gf :: a -> Expr
  fg :: Expr -> a

newtype GString = GString String deriving Show

instance Gf GString where
  gf (GString x) = mkStr x
  fg t =
    case unStr t of
      Just x  ->  GString x
      Nothing -> error ("no GString " ++ show t)

newtype GInt = GInt Int deriving Show

instance Gf GInt where
  gf (GInt x) = mkInt x
  fg t =
    case unInt t of
      Just x  ->  GInt x
      Nothing -> error ("no GInt " ++ show t)

newtype GFloat = GFloat Double deriving Show

instance Gf GFloat where
  gf (GFloat x) = mkFloat x
  fg t =
    case unFloat t of
      Just x  ->  GFloat x
      Nothing -> error ("no GFloat " ++ show t)

----------------------------------------------------
-- below this line machine-generated
----------------------------------------------------

data GA =
   LexA String
  deriving Show

data GAP = GPositA GA
  deriving Show

data GAdv =
   GPrepNP GPrep GNP
 | LexAdv String
  deriving Show

data GCN =
   GAdjCN GAP GCN
 | GUseN GN
  deriving Show

data GCl = GPredVP GNP GVP
  deriving Show

data GConj =
   LexConj String
  deriving Show

data GDet =
   LexDet String
  deriving Show

data GN =
   LexN String
  deriving Show

data GNP =
   GDetCN GDet GCN
 | GMassNP GCN
 | GUsePN GPN
 | GUsePron GPron
  deriving Show

data GPN =
   LexPN String
  deriving Show

data GPol =
   GPNeg
 | GPPos
  deriving Show

data GPrep =
   LexPrep String
  deriving Show

data GPron =
   LexPron String
  deriving Show

data GS =
   GCoordS GConj GS GS
 | GUsePresCl GPol GCl
  deriving Show

data GUtt =
   GUttNP GNP
 | GUttS GS
  deriving Show

data GV =
   LexV String
  deriving Show

data GV2 =
   LexV2 String
  deriving Show

data GVP =
   GAdvVP GVP GAdv
 | GComplV2 GV2 GNP
 | GReflV2 GV2
 | GUseAP GAP
 | GUseV GV
  deriving Show


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


