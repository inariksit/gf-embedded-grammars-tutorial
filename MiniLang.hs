module MiniLang where

import PGF hiding (Tree)

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
 | Galready_Adv
 | Gnow_Adv
  deriving Show

data GCN =
   GAdjCN GAP GCN
 | GUseN GN
  deriving Show

data GCl = GPredVP GNP GVP
  deriving Show

data GConj =
   Gand_Conj
 | Gor_Conj
  deriving Show

data GDet =
   GaPl_Det
 | Ga_Det
 | Gevery_Det
 | GthePl_Det
 | Gthe_Det
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
   Gjohn_PN
 | Gparis_PN
  deriving Show

data GPol =
   GPNeg
 | GPPos
  deriving Show

data GPrep =
   Gin_Prep
 | Gon_Prep
 | Gwith_Prep
  deriving Show

data GPron =
   Ghe_Pron
 | Gi_Pron
 | Gshe_Pron
 | Gthey_Pron
 | Gwe_Pron
 | GyouPl_Pron
 | GyouSg_Pron
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
  gf Galready_Adv = mkApp (mkCId "already_Adv") []
  gf Gnow_Adv = mkApp (mkCId "now_Adv") []

  fg t =
    case unApp t of
      Just (i,[x1,x2]) | i == mkCId "PrepNP" -> GPrepNP (fg x1) (fg x2)
      Just (i,[]) | i == mkCId "already_Adv" -> Galready_Adv
      Just (i,[]) | i == mkCId "now_Adv" -> Gnow_Adv


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
  gf Gand_Conj = mkApp (mkCId "and_Conj") []
  gf Gor_Conj = mkApp (mkCId "or_Conj") []

  fg t =
    case unApp t of
      Just (i,[]) | i == mkCId "and_Conj" -> Gand_Conj
      Just (i,[]) | i == mkCId "or_Conj" -> Gor_Conj


      _ -> error ("no Conj " ++ show t)

instance Gf GDet where
  gf GaPl_Det = mkApp (mkCId "aPl_Det") []
  gf Ga_Det = mkApp (mkCId "a_Det") []
  gf Gevery_Det = mkApp (mkCId "every_Det") []
  gf GthePl_Det = mkApp (mkCId "thePl_Det") []
  gf Gthe_Det = mkApp (mkCId "the_Det") []

  fg t =
    case unApp t of
      Just (i,[]) | i == mkCId "aPl_Det" -> GaPl_Det
      Just (i,[]) | i == mkCId "a_Det" -> Ga_Det
      Just (i,[]) | i == mkCId "every_Det" -> Gevery_Det
      Just (i,[]) | i == mkCId "thePl_Det" -> GthePl_Det
      Just (i,[]) | i == mkCId "the_Det" -> Gthe_Det


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
  gf Gjohn_PN = mkApp (mkCId "john_PN") []
  gf Gparis_PN = mkApp (mkCId "paris_PN") []

  fg t =
    case unApp t of
      Just (i,[]) | i == mkCId "john_PN" -> Gjohn_PN
      Just (i,[]) | i == mkCId "paris_PN" -> Gparis_PN


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
  gf Gin_Prep = mkApp (mkCId "in_Prep") []
  gf Gon_Prep = mkApp (mkCId "on_Prep") []
  gf Gwith_Prep = mkApp (mkCId "with_Prep") []

  fg t =
    case unApp t of
      Just (i,[]) | i == mkCId "in_Prep" -> Gin_Prep
      Just (i,[]) | i == mkCId "on_Prep" -> Gon_Prep
      Just (i,[]) | i == mkCId "with_Prep" -> Gwith_Prep


      _ -> error ("no Prep " ++ show t)

instance Gf GPron where
  gf Ghe_Pron = mkApp (mkCId "he_Pron") []
  gf Gi_Pron = mkApp (mkCId "i_Pron") []
  gf Gshe_Pron = mkApp (mkCId "she_Pron") []
  gf Gthey_Pron = mkApp (mkCId "they_Pron") []
  gf Gwe_Pron = mkApp (mkCId "we_Pron") []
  gf GyouPl_Pron = mkApp (mkCId "youPl_Pron") []
  gf GyouSg_Pron = mkApp (mkCId "youSg_Pron") []

  fg t =
    case unApp t of
      Just (i,[]) | i == mkCId "he_Pron" -> Ghe_Pron
      Just (i,[]) | i == mkCId "i_Pron" -> Gi_Pron
      Just (i,[]) | i == mkCId "she_Pron" -> Gshe_Pron
      Just (i,[]) | i == mkCId "they_Pron" -> Gthey_Pron
      Just (i,[]) | i == mkCId "we_Pron" -> Gwe_Pron
      Just (i,[]) | i == mkCId "youPl_Pron" -> GyouPl_Pron
      Just (i,[]) | i == mkCId "youSg_Pron" -> GyouSg_Pron


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


