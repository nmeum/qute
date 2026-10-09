-- SPDX-FileCopyrightText: 2026 Sören Tempel <soeren+git@soeren-tempel.net>
--
-- SPDX-License-Identifier: GPL-3.0-only

-- | This module implements type classes on top of 'Language.QBE.Types' that
-- implement a map operation over the 'Language.QBE.Value',
-- 'Language.QBE.UserIdent', and 'Language.QBE.GlobalIdent' used by AST nodes.
module Language.QBE.Uses
  ( Value (..),
    GlobalIdent (..),
    UserIdent (..),
  )
where

import Data.Map qualified as Map
import Language.QBE.Types qualified as Q

mapFieldType :: Q.Field -> (Q.UserIdent -> Q.UserIdent) -> Q.Field
mapFieldType (sty, n) f = (mapType sty f, n)

-- | Type class for AST nodes using a 'Q.UserIdent' internally.
class UserIdent a where
  -- | Invoke the provided function for each 'Q.UserIdent' within the AST node @a@.
  mapType :: a -> (Q.UserIdent -> Q.UserIdent) -> a

instance UserIdent Q.TypeDef where
  mapType ty@(Q.TypeDef {Q.aggName = name, Q.aggType = aTy}) f =
    ty {Q.aggName = f name, Q.aggType = mapType aTy f}

instance UserIdent Q.SubType where
  mapType (Q.SUserDef uid) f = Q.SUserDef $ f uid
  mapType ty@(Q.SExtType _) _ = ty

instance UserIdent Q.AggType where
  mapType (Q.ARegular fields) f = Q.ARegular $ map (`mapFieldType` f) fields
  mapType (Q.AUnion fields) f = Q.AUnion $ map (map (`mapFieldType` f)) fields
  mapType (Q.AOpaque w64) _ = Q.AOpaque w64

instance UserIdent Q.Abity where
  mapType (Q.AUserDef uid) f = Q.AUserDef (f uid)
  mapType abity _ = abity

instance UserIdent Q.FuncDef where
  mapType func@(Q.FuncDef {Q.fAbity = Just abity}) f =
    func {Q.fAbity = Just $ mapType abity f}
  mapType func _ = func

------------------------------------------------------------------------

replaceGlobal :: (Q.GlobalIdent -> Q.GlobalIdent) -> Q.Value -> Q.Value
replaceGlobal _ v@(Q.VLocal _) = v
replaceGlobal f (Q.VConst dynConst) =
  Q.VConst $ mapGlobals dynConst f

-- | Type class for AST nodes using a 'Q.GlobalIdent' internally.
class GlobalIdent a where
  -- | Invoke the provided function for each 'Q.GlobalIdent' within the AST node @a@.
  mapGlobals :: a -> (Q.GlobalIdent -> Q.GlobalIdent) -> a

instance GlobalIdent Q.Const where
  mapGlobals (Q.Global i) f = Q.Global (f i)
  mapGlobals c@(Q.DFP _) _ = c
  mapGlobals c@(Q.SFP _) _ = c
  mapGlobals c@(Q.Number _) _ = c

instance GlobalIdent Q.DynConst where
  mapGlobals (Q.Const c) f = Q.Const $ mapGlobals c f
  mapGlobals (Q.Thread i) f = Q.Thread (f i)
  mapGlobals (Q.Common i) f = Q.Common (f i)
  mapGlobals (Q.Extern i) f = Q.Extern (f i)
  mapGlobals (Q.ExternThread i) f = Q.ExternThread (f i)

instance GlobalIdent Q.DataDef where
  mapGlobals def f =
    def
      { Q.name = f $ Q.name def,
        Q.objs = map (`mapGlobals` f) $ Q.objs def
      }

instance GlobalIdent Q.DataObj where
  mapGlobals o@(Q.OZeroFill _) _ = o
  mapGlobals (Q.OItem ty items) f =
    Q.OItem ty $ map (`mapGlobals` f) items

instance GlobalIdent Q.DataItem where
  mapGlobals (Q.DSymOff ident off) f = Q.DSymOff (f ident) off
  mapGlobals (Q.DConst c) f = Q.DConst $ mapGlobals c f
  mapGlobals s@(Q.DString _) _ = s

instance GlobalIdent Q.FuncDef where
  mapGlobals func f =
    func
      { Q.fName = f (Q.fName func),
        Q.fBlock = Map.map (`mapOperands` replaceGlobal f) $ Q.fBlock func
      }

------------------------------------------------------------------------

-- | Type class for AST nodes using a 'Q.Value' internally.
class Value a where
  -- | Invoke the provided function for each 'Q.Value' within the AST node @a@.
  mapOperands :: a -> (Q.Value -> Q.Value) -> a

instance Value Q.FuncArg where
  mapOperands fa f =
    case fa of
      Q.ArgReg a v -> Q.ArgReg a (f v)
      Q.ArgEnv v -> Q.ArgEnv (f v)
      Q.ArgVar -> Q.ArgVar

instance Value Q.JumpInstr where
  mapOperands ji f =
    case ji of
      Q.Jump ident -> Q.Jump ident
      Q.Jnz v ifT ifF -> Q.Jnz (f v) ifT ifF
      Q.Return mayVal -> Q.Return (f <$> mayVal)
      Q.Halt -> Q.Halt

instance Value Q.Instr where
  mapOperands instr f =
    case instr of
      Q.Add lhs rhs -> Q.Add (f lhs) (f rhs)
      Q.Sub lhs rhs -> Q.Sub (f lhs) (f rhs)
      Q.Div lhs rhs -> Q.Div (f lhs) (f rhs)
      Q.Mul lhs rhs -> Q.Mul (f lhs) (f rhs)
      Q.Neg val -> Q.Neg (f val)
      Q.URem lhs rhs -> Q.URem (f lhs) (f rhs)
      Q.Rem lhs rhs -> Q.Rem (f lhs) (f rhs)
      Q.UDiv lhs rhs -> Q.UDiv (f lhs) (f rhs)
      Q.Or lhs rhs -> Q.Or (f lhs) (f rhs)
      Q.Xor lhs rhs -> Q.Xor (f lhs) (f rhs)
      Q.And lhs rhs -> Q.And (f lhs) (f rhs)
      Q.Sar lhs rhs -> Q.Sar (f lhs) (f rhs)
      Q.Shr lhs rhs -> Q.Shr (f lhs) (f rhs)
      Q.Shl lhs rhs -> Q.Shl (f lhs) (f rhs)
      Q.Alloc siz val -> Q.Alloc siz (f val)
      Q.Load ty val -> Q.Load ty (f val)
      Q.CompareInt a c lhs rhs -> Q.CompareInt a c (f lhs) (f rhs)
      Q.CompareFloat a c lhs rhs -> Q.CompareFloat a c (f lhs) (f rhs)
      Q.Ext n val -> Q.Ext n (f val)
      Q.FloatToInt a b val -> Q.FloatToInt a b (f val)
      Q.IntToFloat a b val -> Q.IntToFloat a b (f val)
      Q.TruncDouble val -> Q.TruncDouble (f val)
      Q.Cast val -> Q.Cast (f val)
      Q.Copy val -> Q.Copy (f val)
      Q.VAArg val -> Q.VAArg (f val)

instance Value Q.VolatileInstr where
  mapOperands vi f =
    case vi of
      Q.Store ty lhs rhs -> Q.Store ty (f lhs) (f rhs)
      Q.VAStart val -> Q.VAStart (f val)
      Q.Blit lhs rhs w -> Q.Blit (f lhs) (f rhs) w
      dbg@(Q.DBGLoc {}) -> dbg

instance Value Q.Statement where
  mapOperands s f =
    case s of
      Q.Assign ident ty instr -> Q.Assign ident ty (mapOperands instr f)
      Q.Call retTy func args -> Q.Call retTy (f func) (map (`mapOperands` f) args)
      Q.Volatile vi -> Q.Volatile (mapOperands vi f)

instance Value Q.Phi where
  mapOperands p f = p {Q.pLabels = Map.map f (Q.pLabels p)}

instance Value Q.Block where
  mapOperands b f =
    b
      { Q.phi = map (`mapOperands` f) $ Q.phi b,
        Q.stmt = map (`mapOperands` f) $ Q.stmt b,
        Q.term = mapOperands (Q.term b) f
      }
