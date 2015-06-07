{-
 - Internal.hs
 - By Steven Smith
 -}

module SpirV.Builder.Types.Internal where

import Data.Bits ((.|.))
import Data.Foldable (foldl')
import Data.Map.Strict (Map)
import Data.Sequence (Seq, empty)
import Data.Text (Text)
import Data.Word

import SpirV.Instructions

import qualified Data.Map.Strict as M

data Module = Module {
    sourceInstr :: Maybe Instruction,
    sourceExts :: Seq Instruction,
    compileFlags :: Seq Instruction,
    extensions :: Seq Instruction,
    extInstImports :: Map Text (Id, Instruction),
    memModelInstr :: Instruction,
    entryPoints :: Seq Instruction,
    executionModes :: Seq Instruction,
    debugAnnotations :: DebugInstructions,
    typeDeclarations :: Map TypeDecRep (Id, Instruction),
    constDeclarations :: Map ConstDecRep (Id, Instruction),
    specDeclarations :: Seq Instruction,
    globalDeclarations :: Seq Instruction,
    instructions :: Seq Instruction
}

data DebugInstructions = DebugInstructions {
    opStrings :: Map Text (Id, Instruction),
    opNames :: Seq Instruction,
    opLines :: Seq Instruction,
    opDecorations :: Seq Instruction,
    opGroupDecorations :: Seq Instruction
}

emptyModule :: Module
emptyModule = Module Nothing empty empty empty M.empty defMemModel empty empty
    emptyDebug M.empty M.empty empty empty empty
  where
    emptyDebug = DebugInstructions M.empty empty empty empty empty
    defMemModel = OpMemoryModel Logical Simple

data TypeDecRep = RepVoid
                | RepBool
                | RepInt Word32 Signedness
                | RepFloat Word32
                | RepVector Id Word32
                | RepMatrix Id Word32
                | RepSampler Id Dim SamplerContent ArrayedContent
                    DepthComparison MultiSampled (Maybe AccessQualifier)
                | RepFilter
                | RepArray Id Id
                | RepRuntimeArray Id
                | RepStruct [Id]
                | RepOpaque Text
                | RepPointer StorageClass Id
                | RepFunction Id [Id]
                | RepEvent
                | RepDeviceEvent
                | RepReserveId
                | RepQueue
                | RepPipe Id AccessQualifier
    deriving (Eq, Ord)

data ConstDecRep = CRepTrue Id
                 | CRepFalse Id
                 | CRepConstant Id [Word32]
                 | CRepComposite Id [Id]
                 | CRepSampler Id SamplerAddressingMode SamplerParam
                     SamplerFilterMode
                 | CRepNullPtr Id
                 | CRepNullObj Id
    deriving (Eq, Ord)

class BitMask a where
    toBitMask :: a -> Word32

enumToBitMask :: Enum a => a -> Word32
enumToBitMask = fromIntegral . fromEnum

instance BitMask Signedness where
    toBitMask = enumToBitMask

instance BitMask SamplerContent where
    toBitMask = enumToBitMask

instance BitMask ArrayedContent where
    toBitMask = enumToBitMask

instance BitMask DepthComparison where
    toBitMask = enumToBitMask

instance BitMask MultiSampled where
    toBitMask = enumToBitMask

instance BitMask SamplerParam where
    toBitMask = enumToBitMask

instance BitMask ExecutionModel where
    toBitMask = enumToBitMask

instance BitMask AddressingModel where
    toBitMask = enumToBitMask

instance BitMask MemoryModel where
    toBitMask = enumToBitMask

instance BitMask ExecutionMode where
    toBitMask em = case em of
        Invocations _           -> 0x00
        SpacingEqual            -> 0x01
        SpacingFractionalEqual  -> 0x02
        SpacingFractionalOdd    -> 0x03
        VertexOrderCw           -> 0x04
        VertexOrderCcw          -> 0x05
        PixelCenterImage        -> 0x06
        OriginUpperLeft         -> 0x07
        EarlyFragmentTests      -> 0x08
        PointMode               -> 0x09
        Xfb                     -> 0x0A
        DepthReplacing          -> 0x0B
        DepthAny                -> 0x0C
        DepthGreater            -> 0x0D
        DepthLess               -> 0x0E
        DepthUnchanged          -> 0x0F
        LocalSize _ _ _         -> 0x10
        LocalSizeHint _ _ _     -> 0x11
        InputPoints             -> 0x12
        InputLines              -> 0x13
        InputLinesAdjacency     -> 0x14
        InputTriangles          -> 0x15
        InputTrianglesAdjacency -> 0x16
        InputQuads              -> 0x17
        InputIsolines           -> 0x18
        OutputVertices _        -> 0x19
        OutputPoints            -> 0x1A
        OutputLineStrip         -> 0x1B
        OutputTriangleStrip     -> 0x1C
        VecTypeHint _           -> 0x1D
        ContractionOff          -> 0x1E

instance BitMask StorageClass where
    toBitMask = enumToBitMask

instance BitMask Dim where
    toBitMask = enumToBitMask

instance BitMask SamplerAddressingMode where
    toBitMask = enumToBitMask

instance BitMask SamplerFilterMode where
    toBitMask = enumToBitMask

instance BitMask FPFastMathMode where
    toBitMask fmm = case fmm of
        NotNaN     -> 0x01
        NotInf     -> 0x02
        NSZ        -> 0x04
        AllowRecip -> 0x08
        Fast       -> 0x10

instance BitMask FPRoundingMode where
    toBitMask = enumToBitMask

instance BitMask LinkageType where
    toBitMask = enumToBitMask

instance BitMask AccessQualifier where
    toBitMask = enumToBitMask

instance BitMask FunctionParameterAttribute where
    toBitMask = enumToBitMask

instance BitMask Decoration where
    toBitMask d = case d of
        PrecisionLow          -> 0x00
        PrecisionMedium       -> 0x01
        PrecisionHigh         -> 0x02
        Block                 -> 0x03
        BufferBlock           -> 0x04
        RowMajor              -> 0x05
        ColMajor              -> 0x06
        GLSLShared            -> 0x07
        GLSLStd140            -> 0x08
        GLSLStd430            -> 0x09
        GLSLPacked            -> 0x0A
        Smooth                -> 0x0B
        Noperspective         -> 0x0C
        Flat                  -> 0x0D
        Patch                 -> 0x0E
        Centroid              -> 0x0F
        Sample                -> 0x10
        Invariant             -> 0x11
        Restrict              -> 0x12
        Aliased               -> 0x13
        Volatile              -> 0x14
        Constant              -> 0x15
        Coherent              -> 0x16
        Nonwritable           -> 0x17
        Nonreadable           -> 0x18
        Uniform               -> 0x19
        NoStaticUse           -> 0x1A
        CPacked               -> 0x1B
        FPSaturatedConversion -> 0x1C
        Stream _              -> 0x1D
        Location _            -> 0x1E
        Component _           -> 0x1F
        Index _               -> 0x20
        Binding _             -> 0x21
        DescriptorSet _       -> 0x22
        Offset _              -> 0x23
        Alignment _           -> 0x24
        XfbBuffer _           -> 0x25
        Stride _              -> 0x26
        Built'In _            -> 0x27
        FuncParamAttr _       -> 0x28
        FPRoundingMode _      -> 0x29
        FPFastMathMode _      -> 0x2A
        LinkageType _         -> 0x2B
        SpecId _              -> 0x2C

instance BitMask BuiltIn where
    toBitMask = enumToBitMask

instance BitMask SelectionControl where
    toBitMask sc = case sc of
        SelectionFlatten     -> 0x1
        SelectionDontFlatten -> 0x2

instance BitMask LoopControl where
    toBitMask lc = case lc of
        LoopUnroll     -> 0x1
        LoopDontUnroll -> 0x2

instance BitMask FunctionControl where
    toBitMask fc = case fc of
        FunctionControlInline     -> 0x1
        FunctionControlDontInline -> 0x2
        FunctionControlPure       -> 0x4
        FunctionControlConst      -> 0x8

instance BitMask MemorySemantics where
    toBitMask ms = case ms of
        Relaxed                -> 0x001
        SequentiallyConsistent -> 0x002
        Acquire                -> 0x004
        Release                -> 0x008
        UniformMemory          -> 0x010
        SubgroupMemory         -> 0x020
        WorkgroupLocalMemory   -> 0x040
        WorkgroupGlobalMemory  -> 0x080
        AtomicCounterMemory    -> 0x100
        ImageMemory            -> 0x200

instance BitMask MemoryAccess where
    toBitMask ma = case ma of
        MemoryNormal            -> 0x0
        MemoryVolatile          -> 0x1
        MemoryAligned _         -> 0x2
        MemoryVolatileAligned _ ->
            toBitMask MemoryVolatile .|. toBitMask (MemoryAligned undefined)

instance BitMask ExecutionScope where
    toBitMask = enumToBitMask

instance BitMask GroupOperation where
    toBitMask = enumToBitMask

instance BitMask KernelEnqueueFlags where
    toBitMask = enumToBitMask

instance BitMask KernelProfilingInfo where
    toBitMask kp = case kp of
        CmdExeTime -> 0x1

instance BitMask a => BitMask [a] where
    toBitMask = foldl' (\b a -> b .|. toBitMask a) 0

instance BitMask Id where
    toBitMask (Id i) = fromIntegral i

instance BitMask Word32 where
    toBitMask = id
