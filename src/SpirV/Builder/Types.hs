{-
 - Types.hs
 - By Steven Smith
 -}

module SpirV.Builder.Types where

import Control.Applicative hiding (empty)
import Control.Monad.Fix
import Data.Sequence (Seq)
import Data.Word

import SpirV.Builder.Types.Internal
import SpirV.Instructions (Id(..), Instruction)

newtype Builder a = Builder { runBuilder :: Id -> Module -> (a, Id, Module) }

instance Functor Builder where
    fmap f (Builder b) = Builder go
      where
        go i m = let (a, i', m') = b i m
                  in (f a, i', m')

instance Applicative Builder where
    pure a = Builder go
      where
        go i m = (a, i, m)
    Builder b1 <*> Builder b2 = Builder go
      where
        go i m = let (f, i', m') = b1 i m
                     (a, i'', m'') = b2 i' m'
                  in (f a, i'', m'')

instance Monad Builder where
    return = pure
    Builder b1 >>= f = Builder go
      where
        go i m = let (a, i', m') = b1 i m
                     (Builder b2) = f a
                  in b2 i' m'

instance MonadFix Builder where
    mfix f = Builder go
      where
        go i m = let tup@(a, _, _) = runBuilder (f a) i m
                  in tup

data SpirVModule = SpirVModule {
    spirVMagicNumber :: Word32,
    spirVVersionNumber :: Word32,
    genMagicNumber :: Word32,
    idBound :: Word32,
    instructionStream :: Seq Instruction
}

newtype TypeId = TypeId { runTypeId :: Id }
newtype FileId = FileId { runFileId :: Id }
newtype ExtSet = ExtSet { runExtSet :: Id }
newtype DecorationGroup = DecorationGroup { runDecGroup :: Id }
newtype LabelId = LabelId { runLabelId :: Id }
newtype NDRangeId = NDRangeId { runNDRangeId :: Id }
newtype ReserveId = ReserveId { runReserveId :: Id }

class IsId a where
    toId :: a -> Id

instance IsId Id where
    toId = id

instance IsId TypeId where
    toId (TypeId i) = i

instance IsId FileId where
    toId (FileId i) = i

instance IsId DecorationGroup where
    toId (DecorationGroup i) = i

instance IsId LabelId where
    toId (LabelId i) = i

instance IsId NDRangeId where
    toId (NDRangeId i) = i

instance IsId ReserveId where
    toId (ReserveId i) = i
