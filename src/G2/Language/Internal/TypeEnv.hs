module G2.Language.Internal.TypeEnv where


import G2.Language.Internal.Syntax


data GAlgDataTy n
  = DataTyCon { bound_ids :: [GId n]
              , data_cons :: [GDataCon n] }
  | NewTyCon { bound_ids :: [GId n]
             , data_con :: GDataCon n
             , rep_type :: GType n }
  | TypeSynonym { bound_ids :: [GId n]
                , synonym_of :: GType n
                } deriving (Show, Eq, Read)


type GProgramType n = (GName n, GAlgDataTy n)



