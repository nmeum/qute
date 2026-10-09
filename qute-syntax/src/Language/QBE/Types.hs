-- SPDX-FileCopyrightText: 2025-2026 Sören Tempel <soeren+git@soeren-tempel.net>
--
-- SPDX-License-Identifier: GPL-3.0-only

module Language.QBE.Types
  ( -- * Identifiers
    UserIdent (..),
    LocalIdent (..),
    BlockIdent (..),
    GlobalIdent (..),

    -- * Types
    BaseType (..),
    baseTypeByteSize,
    baseTypeBitSize,
    ExtType (..),
    extTypeBitSize,
    extTypeByteSize,
    SubWordType (..),
    SubType (..),
    LoadType (..),
    loadByteSize,

    -- * Values
    Const (..),
    DynConst (..),
    Value (..),

    -- * Definitions
    TypeDef (..),
    DataDef (..),
    Linkage (..),
    Field,
    AggType (..),
    dataSize,
    DataObj (..),
    objAlign,
    objSize,
    DataItem (..),
    JumpInstr (..),

    -- * Functions
    FuncDef (..),
    FuncParam (..),
    FuncArg (..),
    Abity (..),
    abityToBase,
    Block (..),
    fEntry,

    -- * Instructions
    Statement (..),
    Instr (..),
    VolatileInstr (..),
    ExtArg (..),
    toExtType,
    FloatArg (..),
    f2BaseType,
    IntArg (..),
    i2BaseType,
    IntCmpOp (..),
    FloatCmpOp (..),
    Phi (..),
    AllocSize (..),
    getSize,

    -- * Type Classes
    Operation (..),
    Definition (..),
  )
where

import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromJust)
import Data.Word (Word64)

-- TODO: Prefix all constructors

newtype UserIdent = UserIdent {userIdent :: String}
  deriving (Eq, Ord)

instance Show UserIdent where
  show (UserIdent s) = ':' : s

newtype LocalIdent = LocalIdent {localIdent :: String}
  deriving (Eq, Ord)

instance Show LocalIdent where
  show (LocalIdent s) = '%' : s

newtype BlockIdent = BlockIdent {blockIdent :: String}
  deriving (Eq, Ord)

instance Show BlockIdent where
  show (BlockIdent s) = '@' : s

newtype GlobalIdent = GlobalIdent {globalIdent :: String}
  deriving (Eq, Ord)

instance Show GlobalIdent where
  show (GlobalIdent s) = '$' : s

------------------------------------------------------------------------

class Definition a where
  mapGlobals :: a -> (GlobalIdent -> GlobalIdent) -> a

replaceGlobal :: (GlobalIdent -> GlobalIdent) -> Value -> Value
replaceGlobal _ v@(VLocal _) = v
replaceGlobal f (VConst dynConst) =
  VConst $ mapGlobals dynConst f

class Operation a where
  mapOperands :: a -> (Value -> Value) -> a

data BaseType
  = Word
  | Long
  | Single
  | Double
  deriving (Show, Eq)

baseTypeByteSize :: BaseType -> Int
baseTypeByteSize Word = 4
baseTypeByteSize Long = 8
baseTypeByteSize Single = 4
baseTypeByteSize Double = 8

baseTypeBitSize :: BaseType -> Int
baseTypeBitSize ty = baseTypeByteSize ty * 8

data ExtType
  = Base BaseType
  | Byte
  | HalfWord
  deriving (Show, Eq)

extTypeByteSize :: ExtType -> Int
extTypeByteSize (Base b) = baseTypeByteSize b
extTypeByteSize Byte = 1
extTypeByteSize HalfWord = 2

extTypeBitSize :: ExtType -> Int
extTypeBitSize ty = extTypeByteSize ty * 8

data SubWordType
  = SignedByte
  | UnsignedByte
  | SignedHalf
  | UnsignedHalf
  deriving (Show, Eq)

data Abity
  = ABase BaseType
  | ASubWordType SubWordType
  | AUserDef UserIdent
  deriving (Show, Eq)

abityToBase :: Abity -> BaseType
-- Calls with a sub-word return type define a temporary of base type
-- w with its most significant bits unspecified.
abityToBase (ASubWordType _) = Word
-- When an aggregate type is used as argument type or return type, the
-- value respectively passed or returned needs to be a pointer to a
-- memory location holding the value.
abityToBase (AUserDef _) = Long
abityToBase (ABase ty) = ty

data Const
  = Number Word64
  | SFP Float
  | DFP Double
  | Global GlobalIdent
  deriving (Show, Eq)

instance Definition Const where
  mapGlobals (Global i) f = Global (f i)
  mapGlobals c@(DFP _) _ = c
  mapGlobals c@(SFP _) _ = c
  mapGlobals c@(Number _) _ = c

data DynConst
  = Const Const
  | Thread GlobalIdent
  | Common GlobalIdent
  | Extern GlobalIdent
  | ExternThread GlobalIdent
  deriving (Show, Eq)

instance Definition DynConst where
  mapGlobals (Const c) f = Const $ mapGlobals c f
  mapGlobals (Thread i) f = Thread (f i)
  mapGlobals (Common i) f = Common (f i)
  mapGlobals (Extern i) f = Extern (f i)
  mapGlobals (ExternThread i) f = ExternThread (f i)

data Value
  = VConst DynConst
  | VLocal LocalIdent
  deriving (Show, Eq)

data Linkage
  = LExport
  | LCommon
  | LThread
  | LSection String (Maybe String)
  deriving (Show, Eq)

data AllocSize
  = AllocWord
  | AllocLong
  | AllocLongLong
  deriving (Show, Eq)

getSize :: AllocSize -> Int
getSize AllocWord = 4
getSize AllocLong = 8
getSize AllocLongLong = 16

data TypeDef
  = TypeDef
  { aggName :: UserIdent,
    aggAlign :: Maybe Word64,
    aggType :: AggType
  }
  deriving (Show, Eq)

data SubType
  = SExtType ExtType
  | SUserDef UserIdent
  deriving (Show, Eq)

type Field = (SubType, Maybe Word64)

-- TODO: Type for tuple
data AggType
  = ARegular [Field]
  | AUnion [[Field]]
  | AOpaque Word64
  deriving (Show, Eq)

data DataDef
  = DataDef
  { linkage :: [Linkage],
    name :: GlobalIdent,
    align :: Maybe Word64,
    objs :: [DataObj]
  }
  deriving (Show, Eq)

instance Definition DataDef where
  mapGlobals def f =
    def
      { name = f $ name def,
        objs = map (`mapGlobals` f) $ objs def
      }

dataSize :: DataDef -> Int
dataSize dataDef =
  sum $ map objSize (objs dataDef)

data DataObj
  = OItem ExtType [DataItem]
  | OZeroFill Word64
  deriving (Show, Eq)

instance Definition DataObj where
  mapGlobals o@(OZeroFill _) _ = o
  mapGlobals (OItem ty items) f =
    OItem ty $ map (`mapGlobals` f) items

objAlign :: DataObj -> Word64
objAlign (OZeroFill _) = 1 :: Word64
objAlign (OItem ty _) = fromIntegral $ extTypeByteSize ty

objSize :: DataObj -> Int
objSize (OZeroFill n) = fromIntegral n
objSize (OItem ty items) = extTypeByteSize ty * cnt items
  where
    cnt :: [DataItem] -> Int
    cnt [] = 0
    cnt ((DString s) : xs) = length s + cnt xs
    cnt (_ : xs) = 1 + cnt xs

data DataItem
  = DSymOff GlobalIdent Word64
  | DString String
  | DConst Const
  deriving (Show, Eq)

instance Definition DataItem where
  mapGlobals (DSymOff ident off) f = DSymOff (f ident) off
  mapGlobals (DConst c) f = DConst $ mapGlobals c f
  mapGlobals s@(DString _) _ = s

data FuncDef
  = FuncDef
  { fLinkage :: [Linkage],
    fName :: GlobalIdent,
    fStart :: BlockIdent,
    fAbity :: Maybe Abity,
    fParams :: [FuncParam],
    fBlock :: Map BlockIdent Block
  }
  deriving (Show, Eq)

instance Definition FuncDef where
  mapGlobals func f =
    func
      { fName = f (fName func),
        fBlock = Map.map (`mapOperands` replaceGlobal f) $ fBlock func
      }

fEntry :: FuncDef -> Block
fEntry func = fromJust $ Map.lookup (fStart func) (fBlock func)

data FuncParam
  = Regular Abity LocalIdent
  | Env LocalIdent
  | Variadic
  deriving (Show, Eq)

data FuncArg
  = ArgReg Abity Value
  | ArgEnv Value
  | ArgVar
  deriving (Show, Eq)

instance Operation FuncArg where
  mapOperands fa f =
    case fa of
      ArgReg a v -> ArgReg a (f v)
      ArgEnv v -> ArgEnv (f v)
      ArgVar -> ArgVar

data JumpInstr
  = Jump BlockIdent
  | Jnz Value BlockIdent BlockIdent
  | Return (Maybe Value)
  | Halt
  deriving (Show, Eq)

instance Operation JumpInstr where
  mapOperands ji f =
    case ji of
      Jump ident -> Jump ident
      Jnz v ifT ifF -> Jnz (f v) ifT ifF
      Return mayVal -> Return (f <$> mayVal)
      Halt -> Halt

data LoadType
  = LSubWord SubWordType
  | LBase BaseType
  deriving (Show, Eq)

-- TODO: Could/Should define this on ExtType instead.
loadByteSize :: LoadType -> Word64
loadByteSize (LSubWord UnsignedByte) = 1
loadByteSize (LSubWord SignedByte) = 1
loadByteSize (LSubWord SignedHalf) = 2
loadByteSize (LSubWord UnsignedHalf) = 2
loadByteSize (LBase Word) = 4
loadByteSize (LBase Long) = 8
loadByteSize (LBase Single) = 4
loadByteSize (LBase Double) = 8

data ExtArg
  = ExtSingle
  | ExtSubWord SubWordType
  | ExtSignedWord
  | ExtUnsignedWord
  deriving (Show, Eq)

toExtType :: ExtArg -> (Bool, ExtType)
toExtType (ExtSubWord SignedByte) = (True, Byte)
toExtType (ExtSubWord UnsignedByte) = (False, Byte)
toExtType (ExtSubWord SignedHalf) = (True, HalfWord)
toExtType (ExtSubWord UnsignedHalf) = (False, HalfWord)
toExtType ExtSignedWord = (True, Base Word)
toExtType ExtUnsignedWord = (False, Base Word)
toExtType ExtSingle = (True, Base Single)

data FloatArg = FDouble | FSingle
  deriving (Show, Eq)

f2BaseType :: FloatArg -> BaseType
f2BaseType FSingle = Single
f2BaseType FDouble = Double

data IntArg = IWord | ILong
  deriving (Show, Eq)

i2BaseType :: IntArg -> BaseType
i2BaseType IWord = Word
i2BaseType ILong = Long

-- TODO: Distinict types for floating point comparison?
data IntCmpOp
  = IEq
  | INe
  | ISle
  | ISlt
  | ISge
  | ISgt
  | IUle
  | IUlt
  | IUge
  | IUgt
  deriving (Show, Eq)

data FloatCmpOp
  = FEq
  | FNe
  | FLe
  | FLt
  | FGe
  | FGt
  | FOrd
  | FUnord
  deriving (Show, Eq)

data Instr
  = Add Value Value
  | Sub Value Value
  | Div Value Value
  | Mul Value Value
  | Neg Value
  | URem Value Value
  | Rem Value Value
  | UDiv Value Value
  | Or Value Value
  | Xor Value Value
  | And Value Value
  | Sar Value Value
  | Shr Value Value
  | Shl Value Value
  | Alloc AllocSize Value
  | Load LoadType Value
  | CompareInt IntArg IntCmpOp Value Value
  | CompareFloat FloatArg FloatCmpOp Value Value
  | Ext ExtArg Value
  | FloatToInt FloatArg Bool Value
  | IntToFloat IntArg Bool Value
  | TruncDouble Value
  | Cast Value
  | Copy Value
  | VAArg Value
  deriving (Show, Eq)

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

data VolatileInstr
  = Store ExtType Value Value
  | VAStart Value
  | Blit Value Value Word64
  | DBGLoc Word64 Word64 (Maybe Word64)
  deriving (Show, Eq)

instance Operation VolatileInstr where
  mapOperands vi f =
    case vi of
      Store ty lhs rhs -> Store ty (f lhs) (f rhs)
      VAStart val -> VAStart (f val)
      Blit lhs rhs w -> Blit (f lhs) (f rhs) w
      dbg@(DBGLoc {}) -> dbg

data Statement
  = Assign LocalIdent BaseType Instr
  | Call (Maybe (LocalIdent, Abity)) Value [FuncArg]
  | Volatile VolatileInstr
  deriving (Show, Eq)

instance Operation Statement where
  mapOperands s f =
    case s of
      Assign ident ty instr -> Assign ident ty (mapOperands instr f)
      Call retTy func args -> Call retTy (f func) (map (`mapOperands` f) args)
      Volatile vi -> Volatile (mapOperands vi f)

data Phi
  = Phi
  { pName :: LocalIdent,
    pType :: BaseType,
    pLabels :: Map BlockIdent Value
  }
  deriving (Show, Eq)

instance Operation Phi where
  mapOperands p f = p {pLabels = Map.map f (pLabels p)}

data Block
  = Block
  { label :: BlockIdent, -- TODO: Consider removing this (part of the Map)
    phi :: [Phi],
    stmt :: [Statement],
    term :: JumpInstr
  }
  deriving (Show, Eq)

instance Operation Block where
  mapOperands b f =
    b
      { phi = map (`mapOperands` f) $ phi b,
        stmt = map (`mapOperands` f) $ stmt b,
        term = mapOperands (term b) f
      }
