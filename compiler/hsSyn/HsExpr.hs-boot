{-# LANGUAGE CPP, KindSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-} -- Note [Pass sensitive types]
                                      -- in module PlaceHolder
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RoleAnnotations #-}

module HsExpr where

import SrcLoc     ( Located )
import Outputable ( SDoc, OutputableBndr, Outputable )
import {-# SOURCE #-} HsPat  ( LPat )
import PlaceHolder ( DataId, NameOrRdrName )
import Data.Data hiding ( Fixity )

type role HsExpr nominal
type role HsCmd nominal
type role MatchGroup nominal representational
type role GRHSs nominal representational
type role HsSplice nominal
type role SyntaxExpr nominal
data HsExpr (i :: *)
data HsCmd  (i :: *)
data HsSplice (i :: *)
data MatchGroup (a :: *) (body :: *)
data GRHSs (a :: *) (body :: *)
data SyntaxExpr (i :: *)

instance (DataId id) => Data (HsSplice id)
instance (DataId id) => Data (HsExpr id)
instance (DataId id) => Data (HsCmd id)
instance (Data body,DataId id) => Data (MatchGroup id body)
instance (Data body,DataId id) => Data (GRHSs id body)
instance (DataId id) => Data (SyntaxExpr id)

instance (OutputableBndr id, OutputableBndr (NameOrRdrName id))
         => Outputable (HsExpr id)
instance (OutputableBndr id, OutputableBndr (NameOrRdrName id))
         => Outputable (HsCmd id)

type LHsExpr a = Located (HsExpr a)

pprLExpr :: (OutputableBndr id,OutputableBndr (NameOrRdrName id))
         => LHsExpr id -> SDoc

pprExpr :: (OutputableBndr id, OutputableBndr (NameOrRdrName id))
        => HsExpr id -> SDoc

pprSplice :: (OutputableBndr id, OutputableBndr (NameOrRdrName id))
          => HsSplice id -> SDoc

pprPatBind :: (OutputableBndr bndr,
               OutputableBndr (NameOrRdrName bndr),
               OutputableBndr id, Outputable body,
               OutputableBndr (NameOrRdrName id))
           => LPat bndr -> GRHSs id body -> SDoc

pprFunBind :: (OutputableBndr idR, Outputable body,
              OutputableBndr (NameOrRdrName idR))
           => MatchGroup idR body -> SDoc
