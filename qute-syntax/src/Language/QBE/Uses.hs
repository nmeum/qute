-- SPDX-FileCopyrightText: 2026 Sören Tempel <soeren+git@soeren-tempel.net>
--
-- SPDX-License-Identifier: GPL-3.0-only

-- | This module implements type classes on top of 'Language.QBE.Types' that
-- implement a map operation over the Types, Values, and GlobalIdents used by
-- AST nodes.
module Language.QBE.Uses
  ( Operation (..),
    Definition (..),
    UsesType (..),
  )
where

import Data.Map qualified as Map
import Language.QBE.Types

mapFieldType :: Field -> (UserIdent -> UserIdent) -> Field
mapFieldType (sty, n) f = (mapType sty f, n)

class UsesType a where
  mapType :: a -> (UserIdent -> UserIdent) -> a

instance UsesType TypeDef where
  mapType ty@(TypeDef {aggName = name, aggType = aTy}) f =
    ty {aggName = f name, aggType = mapType aTy f}

instance UsesType SubType where
  mapType (SUserDef uid) f = SUserDef $ f uid
  mapType ty@(SExtType _) _ = ty

instance UsesType AggType where
  mapType (ARegular fields) f = ARegular $ map (`mapFieldType` f) fields
  mapType (AUnion fields) f = AUnion $ map (map (`mapFieldType` f)) fields
  mapType (AOpaque w64) _ = AOpaque w64

instance UsesType Abity where
  mapType (AUserDef uid) f = AUserDef (f uid)
  mapType abity _ = abity

instance UsesType FuncDef where
  mapType func@(FuncDef {fAbity = Just abity}) f =
    func {fAbity = Just $ mapType abity f}
  mapType func _ = func

------------------------------------------------------------------------

replaceGlobal :: (GlobalIdent -> GlobalIdent) -> Value -> Value
replaceGlobal _ v@(VLocal _) = v
replaceGlobal f (VConst dynConst) =
  VConst $ mapGlobals dynConst f

class Definition a where
  mapGlobals :: a -> (GlobalIdent -> GlobalIdent) -> a

instance Definition Const where
  mapGlobals (Global i) f = Global (f i)
  mapGlobals c@(DFP _) _ = c
  mapGlobals c@(SFP _) _ = c
  mapGlobals c@(Number _) _ = c

instance Definition DynConst where
  mapGlobals (Const c) f = Const $ mapGlobals c f
  mapGlobals (Thread i) f = Thread (f i)
  mapGlobals (Common i) f = Common (f i)
  mapGlobals (Extern i) f = Extern (f i)
  mapGlobals (ExternThread i) f = ExternThread (f i)

instance Definition DataDef where
  mapGlobals def f =
    def
      { name = f $ name def,
        objs = map (`mapGlobals` f) $ objs def
      }

instance Definition DataObj where
  mapGlobals o@(OZeroFill _) _ = o
  mapGlobals (OItem ty items) f =
    OItem ty $ map (`mapGlobals` f) items

instance Definition DataItem where
  mapGlobals (DSymOff ident off) f = DSymOff (f ident) off
  mapGlobals (DConst c) f = DConst $ mapGlobals c f
  mapGlobals s@(DString _) _ = s

instance Definition FuncDef where
  mapGlobals func f =
    func
      { fName = f (fName func),
        fBlock = Map.map (`mapOperands` replaceGlobal f) $ fBlock func
      }

------------------------------------------------------------------------

class Operation a where
  mapOperands :: a -> (Value -> Value) -> a

instance Operation FuncArg where
  mapOperands fa f =
    case fa of
      ArgReg a v -> ArgReg a (f v)
      ArgEnv v -> ArgEnv (f v)
      ArgVar -> ArgVar

instance Operation JumpInstr where
  mapOperands ji f =
    case ji of
      Jump ident -> Jump ident
      Jnz v ifT ifF -> Jnz (f v) ifT ifF
      Return mayVal -> Return (f <$> mayVal)
      Halt -> Halt

instance Operation Instr where
  mapOperands instr f =
    case instr of
      Add lhs rhs -> Add (f lhs) (f rhs)
      Sub lhs rhs -> Sub (f lhs) (f rhs)
      Div lhs rhs -> Div (f lhs) (f rhs)
      Mul lhs rhs -> Mul (f lhs) (f rhs)
      Neg val -> Neg (f val)
      URem lhs rhs -> URem (f lhs) (f rhs)
      Rem lhs rhs -> Rem (f lhs) (f rhs)
      UDiv lhs rhs -> UDiv (f lhs) (f rhs)
      Or lhs rhs -> Or (f lhs) (f rhs)
      Xor lhs rhs -> Xor (f lhs) (f rhs)
      And lhs rhs -> And (f lhs) (f rhs)
      Sar lhs rhs -> Sar (f lhs) (f rhs)
      Shr lhs rhs -> Shr (f lhs) (f rhs)
      Shl lhs rhs -> Shl (f lhs) (f rhs)
      Alloc siz val -> Alloc siz (f val)
      Load ty val -> Load ty (f val)
      CompareInt a c lhs rhs -> CompareInt a c (f lhs) (f rhs)
      CompareFloat a c lhs rhs -> CompareFloat a c (f lhs) (f rhs)
      Ext n val -> Ext n (f val)
      FloatToInt a b val -> FloatToInt a b (f val)
      IntToFloat a b val -> IntToFloat a b (f val)
      TruncDouble val -> TruncDouble (f val)
      Cast val -> Cast (f val)
      Copy val -> Copy (f val)
      VAArg val -> VAArg (f val)

instance Operation VolatileInstr where
  mapOperands vi f =
    case vi of
      Store ty lhs rhs -> Store ty (f lhs) (f rhs)
      VAStart val -> VAStart (f val)
      Blit lhs rhs w -> Blit (f lhs) (f rhs) w
      dbg@(DBGLoc {}) -> dbg

instance Operation Statement where
  mapOperands s f =
    case s of
      Assign ident ty instr -> Assign ident ty (mapOperands instr f)
      Call retTy func args -> Call retTy (f func) (map (`mapOperands` f) args)
      Volatile vi -> Volatile (mapOperands vi f)

instance Operation Phi where
  mapOperands p f = p {pLabels = Map.map f (pLabels p)}

instance Operation Block where
  mapOperands b f =
    b
      { phi = map (`mapOperands` f) $ phi b,
        stmt = map (`mapOperands` f) $ stmt b,
        term = mapOperands (term b) f
      }
