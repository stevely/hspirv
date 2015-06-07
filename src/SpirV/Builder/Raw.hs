{-
 - Raw.hs
 - By Steven Smith
 -}

module SpirV.Builder.Raw 
( buildModule
, nop
, undef
, source
, sourceExtension
, compileFlag
, extension
, extInstImport
, extInst
, memoryModel
, entryPoint
, executionMode
, string
, name
, memberName
, line
, decorationGroup
, decorate
, memberDecorate
, groupDecorate
, groupMemberDecorate
, typeVoid
, typeBool
, typeInt
, typeFloat
, typeVector
, typeMatrix
, typeSampler
, typeFilter
, typeArray
, typeRuntimeArray
, typeStruct
, typeOpaque
, typePointer
, typeFunction
, typeEvent
, typeDeviceEvent
, typeReserveId
, typeQueue
, typePipe
, constantTrue
, constantFalse
, constant
, constantComposite
, constantSampler
, constantNullPointer
, constantNullObject
, specConstantTrue
, specConstantFalse
, specConstant
, specConstantComposite
, variable
, variableArray
, load
, store
, copyMemory
, copyMemorySized
, accessChain
, inBoundsAccessChain
, arrayLength
, imagePointer
, genericPtrMemSemantics
, function
, functionParameter
, functionEnd
, functionCall
, sampler
, textureSample
, textureSampleDref
, textureSampleLod
, textureSampleProj
, textureSampleGrad
, textureSampleOffset
, textureSampleProjLod
, textureSampleProjGrad
, textureSampleLodOffset
, textureSampleProjOffset
, textureSampleGradOffset
, textureSampleProjLodOffset
, textureSampleProjGradOffset
, textureFetchTexelLod
, textureFetchTexelOffset
, textureFetchTexelSample
, textureFetchTexel
, textureGather
, textureGatherOffset
, textureGatherOffsets
, textureQuerySizeLod
, textureQuerySize
, textureQueryLod
, textureQueryLevels
, textureQuerySamples
, convertFToU
, convertFToS
, convertSToF
, convertUToF
, uConvert
, sConvert
, fConvert
, convertPtrToU
, convertUToPtr
, ptrCastToGeneric
, genericCastToPtr
, bitcast
, genericCastToPtrExplicit
, satConvertSToU
, satConvertUToS
, vectorExtractDynamic
, vectorInsertDynamic
, vectorShuffle
, compositeConstruct
, compositeExtract
, compositeInsert
, copyObject
, transpose
, sNegate
, fNegate
, bNot
, iAdd
, fAdd
, iSub
, fSub
, iMul
, fMul
, uDiv
, sDiv
, fDiv
, uMod
, sRem
, sMod
, fRem
, fMod
, vectorTimesScalar
, matrixTimesScalar
, vectorTimesMatrix
, matrixTimesVector
, matrixTimesMatrix
, outerProduct
, dot
, shiftRightLogical
, shiftRightArithmetic
, shiftLeftLogical
, bitwiseOr
, bitwiseXor
, bitwiseAnd
, vAny
, vAll
, fIsNaN
, isInf
, isFinite
, isNormal
, signBitSet
, lessOrGreater
, ordered
, unordered
, logicalOr
, logicalXor
, logicalAnd
, select
, iEqual
, fOrdEqual
, fUnordEqual
, iNotEqual
, fOrdNotEqual
, fUnordNotEqual
, uLessThan
, sLessThan
, fOrdLessThan
, fUnordLessThan
, uGreaterThan
, sGreaterThan
, fOrdGreaterThan
, fUnordGreaterThan
, uLessThanEqual
, sLessThanEqual
, fOrdLessThanEqual
, fUnordLessThanEqual
, uGreaterThanEqual
, sGreaterThanEqual
, fOrdGreaterThanEqual
, fUnordGreaterThanEqual
, dPdx
, dPdy
, fWidth
, dPdxFine
, dPdyFine
, fWidthFine
, dPdxCoarse
, dPdyCoarse
, fWidthCoarse
, phi
, loopMerge
, selectionMerge
, label
, branch
, branchConditional
, switch
, kill
, return_
, returnValue
, unreachable
, lifetimeStart
, lifetimeStop
, atomicInit
, atomicLoad
, atomicStore
, atomicExchange
, atomicCompareExchange
, atomicCompareExchangeWeak
, atomicIIncrement
, atomicIDecrement
, atomicIAdd
, atomicISub
, atomicUMin
, atomicUMax
, atomicAnd
, atomicOr
, atomicXor
, atomicIMin
, atomicIMax
, emitVertex
, endPrimitive
, emitStreamVertex
, endStreamPrimitive
, controlBarrier
, memoryBarrier
, asyncGroupCopy
, waitGroupEvents
, groupAll
, groupAny
, groupBroadcast
, groupIAdd
, groupFAdd
, groupFMin
, groupUMin
, groupSMin
, groupFMax
, groupUMax
, groupSMax
, enqueueMarker
, enqueueKernel
, getKernelNDrangeSubGroupCount
, getKernelNDrangeMaxSubGroupCount
, getKernelWorkGroupSize
, getKernelPreferredWorkGroupSizeMultiple
, retainEvent
, releaseEvent
, createUserEvent
, isValidEvent
, setUserEventStatus
, captureEventProfilingInfo
, getDefaultQueue
, buildNDRange
, readPipe
, writePipe
, reservedReadPipe
, reservedWritePipe
, reserveReadPipePackets
, reserveWritePipePackets
, commitReadPipe
, commitWritePipe
, isValidReserveId
, getNumPipePackets
, getMaxPipePackets
, groupReserveReadPipePackets
, groupReserveWritePipePackets
, groupCommitReadPipe
, groupCommitWritePipe
-- Re-exports from SpirV.Instructions
, Id(..)
, Signedness(..)
, SamplerContent(..)
, ArrayedContent(..)
, DepthComparison(..)
, MultiSampled(..)
, SamplerParam(..)
, ExecutionModel(..)
, AddressingModel(..)
, MemoryModel(..)
, ExecutionMode(..)
, StorageClass(..)
, Dim(..)
, SamplerAddressingMode(..)
, SamplerFilterMode(..)
, FPFastMathMode(..)
, FPRoundingMode(..)
, LinkageType(..)
, AccessQualifier(..)
, FunctionParameterAttribute(..)
, Decoration(..)
, BuiltIn(..)
, SelectionControl(..)
, LoopControl(..)
, FunctionControl(..)
, MemorySemantics(..)
, MemoryAccess(..)
, ExecutionScope(..)
, GroupOperation(..)
, KernelEnqueueFlags(..)
, KernelProfilingInfo(..)
-- Re-exports from SpirV.Types
, Builder(..)
)
where

import Data.Word
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Data.Sequence (singleton, empty, (><), (|>), (<|), unstableSortBy)
import Data.Function (on)

import qualified Data.Map.Strict as M

import SpirV.Instructions
import SpirV.Builder.Types
import SpirV.Builder.Types.Internal

buildModule :: Builder a -> SpirVModule
buildModule (Builder b) = SpirVModule {
    spirVMagicNumber = 0x07230203,
    spirVVersionNumber = 99, -- Pre-release
    genMagicNumber = 2718, -- This library's magic number
    idBound = i,
    instructionStream = src >< sxs >< cfs >< exts >< eiis >< mm >< eps >< ems
        >< strs >< nms >< lns >< decs >< gdecs >< tcg >< ins
}
  where
    (_, Id i, m) = b (Id 1) emptyModule
    src = maybe empty singleton (sourceInstr m)
    sxs = sourceExts m
    cfs = compileFlags m
    exts = extensions m
    eiis = M.foldr ((<|) . snd) empty (extInstImports m)
    mm = singleton . memModelInstr $ m
    eps = entryPoints m
    ems = executionModes m
    strs = M.foldr ((<|) . snd) empty (opStrings (debugAnnotations m))
    nms = opNames . debugAnnotations $ m
    lns = opLines . debugAnnotations $ m
    decs = opDecorations . debugAnnotations $ m
    gdecs = opGroupDecorations . debugAnnotations $ m
    tcg = unstableSortBy (compare `on` getResultId)
        (M.foldr ((<|) . snd) empty (typeDeclarations m)
            >< M.foldr ((<|) . snd) empty (constDeclarations m)
            >< specDeclarations m
            >< globalDeclarations m)
    ins = instructions m

-- Internal functions
genId :: Builder Id
genId = Builder go
  where
    go id'@(Id i) m = (id', Id (i + 1), m)

writeInstruction :: (Module -> Module) -> Builder ()
writeInstruction f = Builder go
  where
    go i m = ((), i, f m)

addSource :: Word32 -> Word32 -> Module -> Module
addSource sl vn m =
    m { sourceInstr = Just (OpSource sl vn) }

addSourceExt :: Text -> Module -> Module
addSourceExt ext m =
    m { sourceExts = sourceExts m |> OpSourceExtension (encodeUtf8 ext) }

addCompileFlag :: Text -> Module -> Module
addCompileFlag f m =
    m { compileFlags = compileFlags m |> OpCompileFlag (encodeUtf8 f) }

addExtension :: Text -> Module -> Module
addExtension e m =
    m { extensions = extensions m |> OpExtension (encodeUtf8 e) }

addMemoryModel :: AddressingModel -> MemoryModel -> Module -> Module
addMemoryModel am mm m =
    m { memModelInstr = OpMemoryModel am mm }

addExecutionMode :: Instruction -> Module -> Module
addExecutionMode instr m =
    m { executionModes = executionModes m |> instr }

addOpName :: Instruction -> Module -> Module
addOpName instr m =
    m { debugAnnotations =
        debug { opNames = opNames debug |> instr } }
  where
    debug = debugAnnotations m

addOpLine :: Instruction -> Module -> Module
addOpLine instr m =
    m { debugAnnotations =
        debug { opLines = opLines debug |> instr } }
  where
    debug = debugAnnotations m

addOpDecoration :: Instruction -> Module -> Module
addOpDecoration instr m =
    m { debugAnnotations =
        debug { opDecorations = opDecorations debug |> instr } }
  where
    debug = debugAnnotations m

addOpGroupDecoration :: Instruction -> Module -> Module
addOpGroupDecoration instr m =
    m { debugAnnotations =
        debug { opGroupDecorations = opGroupDecorations debug |> instr } }
  where
    debug = debugAnnotations m

addTypeDeclaration :: TypeDecRep -> (Id -> Instruction) -> Builder Id
addTypeDeclaration rep f = Builder go
  where
    go id'@(Id i) m = maybe addType getBuild (M.lookup rep typeDecls)
      where
        typeDecls = typeDeclarations m
        addType = (id', Id (i + 1), m
            { typeDeclarations = M.insert rep (id', f id') typeDecls })
        getBuild (i', _) = (i', id', m)

addConstDeclaration :: ConstDecRep -> (Id -> Instruction) -> Builder Id
addConstDeclaration rep f = Builder go
  where
    go id'@(Id i) m = maybe addConst getBuild (M.lookup rep constDecls)
      where
        constDecls = constDeclarations m
        addConst = (id', Id (i + 1), m
            { constDeclarations = M.insert rep (id', f id') constDecls })
        getBuild (i', _) = (i', id', m)

addOpSpecDeclaration :: Instruction -> Module -> Module
addOpSpecDeclaration instr m =
    m { specDeclarations = specDeclarations m |> instr }

addOpGlobalDeclaration :: Instruction -> Module -> Module
addOpGlobalDeclaration instr m =
    m { globalDeclarations = globalDeclarations m |> instr }

addOpInstruction :: Instruction -> Module -> Module
addOpInstruction instr m =
    m { instructions = instructions m |> instr }

-- Helper functions for instruction section
zeroOp :: Instruction -> Builder ()
zeroOp op = writeInstruction (addOpInstruction op)

oneOp :: (Id -> Instruction) -> Builder Id
oneOp op = do
    i <- genId
    writeInstruction (addOpInstruction (op i))
    return i

twoOp :: (a -> Id -> Instruction) -> a -> Builder Id
twoOp op t = do
    i <- genId
    writeInstruction (addOpInstruction (op t i))
    return i

threeOp :: (a -> Id -> b -> Instruction) -> a -> b -> Builder Id
threeOp op t v = do
    i <- genId
    writeInstruction (addOpInstruction (op t i v))
    return i

fourOp :: (a -> Id -> b -> c -> Instruction) -> a -> b -> c -> Builder Id
fourOp op t v1 v2 = do
    i <- genId
    writeInstruction (addOpInstruction (op t i v1 v2))
    return i

fiveOp :: (a -> Id -> b -> c -> d -> Instruction) -> a -> b -> c -> d
       -> Builder Id
fiveOp op t v1 v2 v3 = do
    i <- genId
    writeInstruction (addOpInstruction (op t i v1 v2 v3))
    return i

sixOp :: (a -> Id -> b -> c -> d -> e -> Instruction)
      -> a -> b -> c -> d -> e
      -> Builder Id
sixOp op t v1 v2 v3 v4 = do
    i <- genId
    writeInstruction (addOpInstruction (op t i v1 v2 v3 v4))
    return i

sevenOp :: (a -> Id -> b -> c -> d -> e -> f -> Instruction)
        -> a -> b -> c -> d -> e -> f
        -> Builder Id
sevenOp op t v1 v2 v3 v4 v5 = do
    i <- genId
    writeInstruction (addOpInstruction (op t i v1 v2 v3 v4 v5))
    return i

eightOp :: (a -> Id -> b -> c -> d -> e -> f -> g -> Instruction)
        -> a -> b -> c -> d -> e -> f -> g
        -> Builder Id
eightOp op t v1 v2 v3 v4 v5 v6 = do
    i <- genId
    writeInstruction (addOpInstruction (op t i v1 v2 v3 v4 v5 v6))
    return i

-- User-facing builder functions

nop :: Builder ()
nop = zeroOp OpNop

-- Id: Result type
undef :: Id -> Builder Id
undef = twoOp OpUndef

-- Word32 1: Source language number (see docs)
-- Word32 2: Source language version number
source :: Word32 -> Word32 -> Builder ()
source sl vn = writeInstruction (addSource sl vn)

-- Text: Documents the existence of a source extension
sourceExtension :: Text -> Builder ()
sourceExtension t = writeInstruction (addSourceExt t)

-- Text: Compile flag
compileFlag :: Text -> Builder ()
compileFlag t = writeInstruction (addCompileFlag t)

-- Text: Extension
extension :: Text -> Builder ()
extension t = writeInstruction (addExtension t)

extInstImport :: Text -> Builder Id
extInstImport t = Builder go
  where
    go id'@(Id i) m = maybe addEII getBuild (M.lookup t eiis)
      where
        eiis = extInstImports m
        addEII = (id', Id (i + 1), m { extInstImports =
            M.insert t (id', OpExtInstImport id' (encodeUtf8 t)) eiis })
        getBuild (i', _) = (i', id', m)

-- Id 1: Result type
-- Id 2: Set, result of ExtInstImport instruction
-- Word32: Enumerant of instruction to execute within extended instruction set
-- Ids: Operands to the instruction to execute
extInst :: Id -> Id -> Word32 -> [Id] -> Builder Id
extInst = fiveOp OpExtInst

memoryModel :: AddressingModel -> MemoryModel -> Builder ()
memoryModel am mm = writeInstruction (addMemoryModel am mm)

-- Id: OpFunction to be the entry point
entryPoint :: ExecutionModel -> Id -> Builder ()
entryPoint em entry = do
    writeInstruction (addExecutionMode (OpEntryPoint em entry))

-- Id: OpFunction declared as an entry point previously
executionMode :: ExecutionMode -> Id -> Builder ()
executionMode em entry = do
    writeInstruction (addExecutionMode (OpExecutionMode entry em))

string :: Text -> Builder Id
string t = Builder go
  where
    go id'@(Id i) m = maybe addStr getBuild (M.lookup t strs)
      where
        strs = opStrings (debugAnnotations m)
        addStr = (id', Id (i + 1), m { debugAnnotations =
            (debugAnnotations m) { opStrings =
                M.insert t (id', OpString id' (encodeUtf8 t)) strs } })
        getBuild (i', _) = (i', id', m)

name :: Id -> Text -> Builder ()
name i t = do
    writeInstruction (addOpName (OpName i (encodeUtf8 t)))

memberName :: Id -> Word32 -> Text -> Builder ()
memberName i m t = do
    writeInstruction (addOpName (OpMemberName i m (encodeUtf8 t)))

line :: Id -> Id -> Word32 -> Word32 -> Builder ()
line t f l c = do
    writeInstruction (addOpLine (OpLine t f l c))

decorationGroup :: Builder Id
decorationGroup = do
    i <- genId
    writeInstruction (addOpGroupDecoration (OpDecorationGroup i))
    return i

-- Id: Target to decorate
decorate :: Id -> Decoration -> Builder ()
decorate i d = do
    writeInstruction (addOpDecoration (OpDecorate i d))

-- Id: Target to decorate
-- Word32: Number of the member to decorate
memberDecorate :: Id -> Word32 -> Decoration -> Builder ()
memberDecorate i m d = do
    writeInstruction (addOpDecoration (OpMemberDecorate i m d))

groupDecorate :: Id -> [Id] -> Builder ()
groupDecorate g ts = do
    writeInstruction (addOpGroupDecoration (OpGroupDecorate g ts))

groupMemberDecorate :: Id -> [Id] -> Builder ()
groupMemberDecorate g ts = do
    writeInstruction (addOpGroupDecoration (OpGroupMemberDecorate g ts))

typeVoid :: Builder Id
typeVoid = addTypeDeclaration RepVoid OpTypeVoid

typeBool :: Builder Id
typeBool = addTypeDeclaration RepBool OpTypeBool

typeInt :: Word32 -> Signedness -> Builder Id
typeInt w s = addTypeDeclaration (RepInt w s) (\i -> OpTypeInt i w s)

typeFloat :: Word32 -> Builder Id
typeFloat w = addTypeDeclaration (RepFloat w) (\i -> OpTypeFloat i w)

-- Id: Type of components
typeVector :: Id -> Word32 -> Builder Id
typeVector t c = addTypeDeclaration (RepVector t c) (\i -> OpTypeVector i t c)

-- Id: Type of columns, must be vector type
-- Word32: Number of columns, must be >= 2
typeMatrix :: Id -> Word32 -> Builder Id
typeMatrix t c = addTypeDeclaration (RepMatrix t c) (\i -> OpTypeMatrix i t c)

-- Id: Type of components when sampled through this sampler
typeSampler :: Id -> Dim -> SamplerContent -> ArrayedContent -> DepthComparison
            -> MultiSampled -> Maybe AccessQualifier -> Builder Id
typeSampler t d sc ac dc ms aq =
    addTypeDeclaration (RepSampler t d sc ac dc ms aq)
        (\i -> OpTypeSampler i t d sc ac dc ms aq)

typeFilter :: Builder Id
typeFilter = addTypeDeclaration RepFilter OpTypeFilter

-- Id 1: Type of elements
-- Id 2: Id of constant instruction with scalar integer type that is >= 1
typeArray :: Id -> Id -> Builder Id
typeArray t l = addTypeDeclaration (RepArray t l) (\i -> OpTypeArray i t l)

-- Id: Type of elements
typeRuntimeArray :: Id -> Builder Id
typeRuntimeArray t =
    addTypeDeclaration (RepRuntimeArray t) (\i -> OpTypeRuntimeArray i t)

-- Ids: List of types for each component in the struct, in order
typeStruct :: [Id] -> Builder Id
typeStruct ts = addTypeDeclaration (RepStruct ts) (\i -> OpTypeStruct i ts)

-- Text: Name of opaque type
typeOpaque :: Text -> Builder Id
typeOpaque n =
    addTypeDeclaration (RepOpaque n) (\i -> OpTypeOpaque i (encodeUtf8 n))

-- Id: Type of object being pointed to
typePointer :: StorageClass -> Id -> Builder Id
typePointer sc t =
    addTypeDeclaration (RepPointer sc t) (\i -> OpTypePointer i sc t)

-- Id: Return type
-- Ids: Types of parameters, in order
typeFunction :: Id -> [Id] -> Builder Id
typeFunction rt ts =
    addTypeDeclaration (RepFunction rt ts) (\i -> OpTypeFunction i rt ts)

typeEvent :: Builder Id
typeEvent = addTypeDeclaration RepEvent OpTypeEvent

typeDeviceEvent :: Builder Id
typeDeviceEvent = addTypeDeclaration RepDeviceEvent OpTypeDeviceEvent

typeReserveId :: Builder Id
typeReserveId = addTypeDeclaration RepReserveId OpTypeReserveId

typeQueue :: Builder Id
typeQueue = addTypeDeclaration RepQueue OpTypeQueue

-- Id: Type of data in pipe
typePipe :: Id -> AccessQualifier -> Builder Id
typePipe t aq = addTypeDeclaration (RepPipe t aq) (\i -> OpTypePipe i t aq)

-- Id: Scalar boolean type
constantTrue :: Id -> Builder Id
constantTrue t = addConstDeclaration (CRepTrue t) (OpConstantTrue t)

-- Id: Scalar boolean type
constantFalse :: Id -> Builder Id
constantFalse t = addConstDeclaration (CRepFalse t) (OpConstantFalse t)

-- TODO: revisit
-- Id: Type of the constant
-- Words: Value of constant, multiple words in the case of 64+ bit constants
constant :: Id -> [Word32] -> Builder Id
constant t ws =
    addConstDeclaration (CRepConstant t ws) (\i -> OpConstant t i ws)

-- Id: Return type (must be a composite type)
-- Ids: Constants for the constituents of the composite value
constantComposite :: Id -> [Id] -> Builder Id
constantComposite t is =
    addConstDeclaration (CRepComposite t is) (\i -> OpConstantComposite t i is)

-- Id: Return type (must be sampler type)
constantSampler :: Id -> SamplerAddressingMode -> SamplerParam
                -> SamplerFilterMode -> Builder Id
constantSampler t am p fm = addConstDeclaration (CRepSampler t am p fm)
    (\i -> OpConstantSampler t i am p fm)

-- Id: Return type (must be pointer type)
constantNullPointer :: Id -> Builder Id
constantNullPointer t =
    addConstDeclaration (CRepNullPtr t) (OpConstantNullPointer t)

-- Id: Return type (can be queue, event, or reservation type)
constantNullObject :: Id -> Builder Id
constantNullObject t =
    addConstDeclaration (CRepNullObj t) (OpConstantNullObject t)

-- Id: Scalar boolean type
specConstantTrue :: Id -> Builder Id
specConstantTrue t = do
    i <- genId
    writeInstruction (addOpSpecDeclaration (OpSpecConstantTrue t i))
    return i

-- Id: Scalar boolean type
specConstantFalse :: Id -> Builder Id
specConstantFalse t = do
    i <- genId
    writeInstruction (addOpSpecDeclaration (OpSpecConstantFalse t i))
    return i

-- TODO: revist
-- Id: Type of the constant
-- Words: Value of constant, multiple words in the case of 64+ bit constants
specConstant :: Id -> [Word32] -> Builder Id
specConstant t ws = do
    i <- genId
    writeInstruction (addOpSpecDeclaration (OpSpecConstant t i ws))
    return i

-- Id: Return type (must be a composite type)
-- Ids: Constants for the constituents of the composite value
specConstantComposite :: Id -> [Id] -> Builder Id
specConstantComposite t is = do
    i <- genId
    writeInstruction (addOpSpecDeclaration (OpSpecConstantComposite t i is))
    return i

-- Id: Result type (must be pointer type)
-- Maybe Id: Initializer (must be from constant instruction)
variable :: Id -> StorageClass -> Maybe Id -> Builder Id
variable t sc ini = do
    i <- genId
    case sc of
        StorageFunction -> writeInstruction (addOpInstruction
            (OpVariable t i sc ini))
        _ -> writeInstruction (addOpGlobalDeclaration (OpVariable t i sc ini))
    return i

-- Id 1: Result type (must be pointer type)
-- Id 2: Number of objects to allocate
variableArray :: Id -> StorageClass -> Id -> Builder Id
variableArray t sc n = do
    i <- genId
    case sc of
        StorageFunction -> writeInstruction (addOpInstruction
            (OpVariableArray t i sc n))
        _ -> writeInstruction (addOpGlobalDeclaration
            (OpVariableArray t i sc n))
    return i

-- Id 1: Result type
-- Id 2: Pointer to load through (must be pointer type)
load :: Id -> Id -> MemoryAccess -> Builder Id
load = fourOp OpLoad

-- Id 1: Pointer to store through (must be pointer type)
-- Id 2: Object to store
store :: Id -> Id -> MemoryAccess -> Builder ()
store p v ma = zeroOp (OpStore p v ma)

-- Id 1: Target to copy into (must be same pointer type)
-- Id 2: Source to copy from (must be same pointer type)
copyMemory :: Id -> Id -> MemoryAccess -> Builder ()
copyMemory t s ma = zeroOp (OpCopyMemory t s ma)

-- Id 1: Target to copy into (must be same pointer type)
-- Id 2: Target to copy into (must be same pointer type)
-- Id 3: Number of bytes to copy
copyMemorySized :: Id -> Id -> Id -> MemoryAccess -> Builder ()
copyMemorySized tar src size ma = zeroOp (OpCopyMemorySized tar src size ma)

-- Id 1: Result type
-- Id 2: Pointer to base object (must be pointer type)
-- Ids: Indices of structure to walk down to reach result value
accessChain :: Id -> Id -> [Id] -> Builder Id
accessChain = fourOp OpAccessChain

-- Id 1: Result type
-- Id 2: Pointer to base object (must be pointer type)
-- Ids: Indices of structure to walk down to reach result value
inBoundsAccessChain :: Id -> Id -> [Id] -> Builder Id
inBoundsAccessChain = fourOp OpInBoundsAccessChain

-- Id 1: Result type
-- Id 2: Structure which has a member that is an array
-- Word32: TODO
arrayLength :: Id -> Id -> Word32 -> Builder Id
arrayLength = fourOp OpArrayLength

-- Id 1: Result type
-- Id 2: Pointer to variable of type sampler
-- Id 3: Which texel coordinate to use
-- Id 4: Which texel sample to use
imagePointer :: Id -> Id -> Id -> Id -> Builder Id
imagePointer = fiveOp OpImagePointer

-- Id 1: Result type, must be a 32-bit int
-- Id 2: A pointer that must point to Generic
genericPtrMemSemantics :: Id -> Id -> Builder Id
genericPtrMemSemantics = threeOp OpGenericPtrMemSemantics

-- Id 1: Result type from calling this function
-- Id 2: The function type for this function (the return type must be the same
--       as Id 1)
function :: Id -> [FunctionControl] -> Id -> Builder Id
function = fourOp OpFunction

-- Id: Type of this function parameter
functionParameter :: Id -> Builder Id
functionParameter = twoOp OpFunctionParameter

functionEnd :: Builder ()
functionEnd = zeroOp OpFunctionEnd

-- Id 1: Result type from calling the given function
-- Id 2: The function to call
-- Ids: The function parameters
functionCall :: Id -> Id -> [Id] -> Builder Id
functionCall = fourOp OpFunctionCall

-- Id 1: Result type, must be a sampler type with both texture and filter
-- Id 2: Sampler with a texture and no filter
-- Id 3: Filter
sampler :: Id -> Id -> Id -> Builder Id
sampler = fourOp OpSampler

-- Id 1: Result type
-- Id 2: Sampler
-- Id 3: Texture coordinate, floating point scalar or vector
-- Maybe Id: Bias for implicit level of detail
textureSample :: Id -> Id -> Id -> Maybe Id -> Builder Id
textureSample = fiveOp OpTextureSample

-- Id 1: Result type
-- Id 2: Sampler, must be cube-arrayed depth-comparison type
-- Id 3: Texture coordinate, vector containing (u, v, w, array layer)
-- Id 4: Depth-comparison reference value
textureSampleDref :: Id -> Id -> Id -> Id -> Builder Id
textureSampleDref = fiveOp OpTextureSampleDref

-- Id 1: Result type
-- Id 2: Sampler
-- Id 3: Texture coordinate, floating point scalar or vector
-- Id 4: Level of detail to use when sampling
textureSampleLod :: Id -> Id -> Id -> Id -> Builder Id
textureSampleLod = fiveOp OpTextureSampleLod

-- Id 1: Result type
-- Id 2: Sampler
-- Id 3: Texture coordinate, floating point vector of 4 components
-- Maybe Id: Bias for implicit level of detail
textureSampleProj :: Id -> Id -> Id -> Maybe Id -> Builder Id
textureSampleProj = fiveOp OpTextureSampleProj

-- Id 1: Result type
-- Id 2: Sampler
-- Id 3: Texture coordinate, floating point scalar or vector
-- Id 4: dx, explicit derivative to use in calculating level of detail
-- Id 5: dy, explicit derivative to use in calculating level of detail
textureSampleGrad :: Id -> Id -> Id -> Id -> Id -> Builder Id
textureSampleGrad = sixOp OpTextureSampleGrad

-- Id 1: Result type
-- Id 2: Sampler
-- Id 3: Texture coordinate, floating point scalar or vector
-- Id 4: Offset added before texel lookup
-- Maybe Id: Bias for implicit level of detail
textureSampleOffset :: Id -> Id -> Id -> Id -> Maybe Id -> Builder Id
textureSampleOffset = sixOp OpTextureSampleOffset

-- Id 1: Result type
-- Id 2: Sampler
-- Id 3: Texture coordinate, floating point vector of 4 components
-- Id 4: Level of detail to use when sampling
textureSampleProjLod :: Id -> Id -> Id -> Id -> Builder Id
textureSampleProjLod = fiveOp OpTextureSampleProjLod

-- Id 1: Result type
-- Id 2: Sampler
-- Id 3: Texture coordinate, floating point vector of 4 components
-- Id 4: dx, explicit derivative to use in calculating level of detail
-- Id 5: dy, explicit derivative to use in calculating level of detail
textureSampleProjGrad :: Id -> Id -> Id -> Id -> Id -> Builder Id
textureSampleProjGrad = sixOp OpTextureSampleProjGrad

-- Id 1: Result type
-- Id 2: Sampler
-- Id 3: Texture coordinate, floating point scalar or vector
-- Id 4: Level of detail to use when sampling
-- Id 5: Offset added before texel lookup
textureSampleLodOffset :: Id -> Id -> Id -> Id -> Id -> Builder Id
textureSampleLodOffset = sixOp OpTextureSampleLodOffset

-- Id 1: Result type
-- Id 2: Sampler
-- Id 3: Texture coordinate, floating point vector of 4 components
-- Id 4: Offset added before texel lookup
-- Maybe Id: Bias for implicit level of detail
textureSampleProjOffset :: Id -> Id -> Id -> Id -> Maybe Id -> Builder Id
textureSampleProjOffset = sixOp OpTextureSampleProjOffset

-- Id 1: Result type
-- Id 2: Sampler
-- Id 3: Texture coordinate, floating point scalar or vector
-- Id 4: dx, explicit derivative to use in calculating level of detail
-- Id 5: dy, explicit derivative to use in calculating level of detail
-- Id 6: Offset added before texel lookup
textureSampleGradOffset :: Id -> Id -> Id -> Id -> Id -> Id -> Builder Id
textureSampleGradOffset = sevenOp OpTextureSampleGradOffset

-- Id 1: Result type
-- Id 2: Sampler
-- Id 3: Texture coordinate, floating point vector of 4 components
-- Id 4: Level of detail to use when sampling
-- Id 5: Offset added before texel lookup
textureSampleProjLodOffset :: Id -> Id -> Id -> Id -> Id -> Builder Id
textureSampleProjLodOffset = sixOp OpTextureSampleProjLodOffset

-- Id 1: Result type
-- Id 2: Sampler
-- Id 3: Texture coordinate, floating point vector of 4 components
-- Id 4: dx, explicit derivative to use in calculating level of detail
-- Id 5: dy, explicit derivative to use in calculating level of detail
-- Id 6: Offset added before texel lookup
textureSampleProjGradOffset :: Id -> Id -> Id -> Id -> Id -> Id -> Builder Id
textureSampleProjGradOffset = sevenOp OpTextureSampleProjGradOffset

-- Id 1: Result type
-- Id 2: Sampler, cannot have dim of cube or buffer, no depth-comparison
-- Id 3: Texture coordinate, integer scalar or vector
-- Id 4: Level of detail to use when sampling
textureFetchTexelLod :: Id -> Id -> Id -> Id -> Builder Id
textureFetchTexelLod = fiveOp OpTextureFetchTexelLod

-- Id 1: Result type
-- Id 2: Sampler, cannot have dim of cube or buffer, no depth-comparison
-- Id 3: Texture coordinate, integer scalar or vector
-- Id 4: Offset added before texel lookup
textureFetchTexelOffset :: Id -> Id -> Id -> Id -> Builder Id
textureFetchTexelOffset = fiveOp OpTextureFetchTexelOffset

-- Id 1: Result type
-- Id 2: Sampler, must be multi-sample texture
-- Id 3: Texture coordinate, integer scalar or vector
-- Id 4: Sample number to return
textureFetchTexelSample :: Id -> Id -> Id -> Id -> Builder Id
textureFetchTexelSample = fiveOp OpTextureFetchTexelSample

-- Id 1: Result type
-- Id 2: Sampler, must have dim of buffer
-- Id 3: Scalar integer index into the buffer
textureFetchTexel :: Id -> Id -> Id -> Builder Id
textureFetchTexel = fourOp OpTextureFetchTexel

-- Id 1: Result type
-- Id 2: Sampler, must have dim of 2D, rect, or cube
-- Id 3: Texture coordinate, floating point scalar or vector
-- Id 4: Component number that will be gathered from all 4 texels. Must be 0-3
textureGather :: Id -> Id -> Id -> Id -> Builder Id
textureGather = fiveOp OpTextureGather

-- Id 1: Result type
-- Id 2: Sampler, must have dim of 2D, rect, or cube
-- Id 3: Texture coordinate, floating point scalar or vector
-- Id 4: Component number that will be gathered from all 4 texels. Must be 0-3
-- Id 5: Offset added before texel lookup
textureGatherOffset :: Id -> Id -> Id -> Id -> Id -> Builder Id
textureGatherOffset = sixOp OpTextureGatherOffset

-- Id 1: Result type
-- Id 2: Sampler, must have dim of 2D, rect, or cube
-- Id 3: Texture coordinate, floating point scalar or vector
-- Id 4: Component number that will be fathered from all 4 texels. Must be 0-3
-- Id 5: Offset added before texel lookup. Must be constant array of size 4 of
--       vectors of 2 integers
textureGatherOffsets :: Id -> Id -> Id -> Id -> Id -> Builder Id
textureGatherOffsets = sixOp OpTextureGatherOffsets

-- Id 1: Result type, must have base type of integer
-- Id 2: Sampler, must have dim of 1D, 2D, 3D, or cube
-- Id 3: Level of detail, used to calculate which mipmap level to query
textureQuerySizeLod :: Id -> Id -> Id -> Builder Id
textureQuerySizeLod = fourOp OpTextureQuerySizeLod

-- Id 1: Result type, must have base type of integer
-- Id 2: Sampler, must have type of rect, buffer, multisampled 2D
textureQuerySize :: Id -> Id -> Builder Id
textureQuerySize = threeOp OpTextureQuerySize

-- Id 1: Result type, must be 2 component floating point vector
-- Id 2: Sampler, must have dim of 1D, 2D, 3D, or cube
-- Id 3: Texture coordinate, floating point scalar or vector
textureQueryLod :: Id -> Id -> Id -> Builder Id
textureQueryLod = fourOp OpTextureQueryLod

-- Id 1: Result type, must be a scalar integer
-- Id 2: Sampler, must have dim of 1D, 2D, 3D, or cube
textureQueryLevels :: Id -> Id -> Builder Id
textureQueryLevels = threeOp OpTextureQueryLevels

-- Id 1: Result type, must be a scalar integer
-- Id 2: Sampler, must have dim of 2D and be a multisample texture
textureQuerySamples :: Id -> Id -> Builder Id
textureQuerySamples = threeOp OpTextureQuerySamples

-- Id 1: Result type, must be unsigned int
-- Id 2: Float value to convert
convertFToU :: Id -> Id -> Builder Id
convertFToU = threeOp OpConvertFToU

-- Id 1: Result type, must be signed int
-- Id 2: Float value to convert
convertFToS :: Id -> Id -> Builder Id
convertFToS = threeOp OpConvertFToS

-- Id 1: Result type, must be float value
-- Id 2: Signed value to convert
convertSToF :: Id -> Id -> Builder Id
convertSToF = threeOp OpConvertSToF

-- Id 1: Result type, must be float value
-- Id 2: Unsigned value to convert
convertUToF :: Id -> Id -> Builder Id
convertUToF = threeOp OpConvertUToF

-- Id 1: Result type, must be unsigned value
-- Id 2: Unsigned value to change width
uConvert :: Id -> Id -> Builder Id
uConvert = threeOp OpUConvert

-- Id 1: Result type, must be signed value
-- Id 2: Signed value to change width
sConvert :: Id -> Id -> Builder Id
sConvert = threeOp OpSConvert

-- Id 1: Result type, must be float value
-- Id 2: Float value to change width
fConvert :: Id -> Id -> Builder Id
fConvert = threeOp OpFConvert

-- Id 1: Result type, must be unsigned value
-- Id 2: Pointer value to cast to unsigned
convertPtrToU :: Id -> Id -> Builder Id
convertPtrToU = threeOp OpConvertPtrToU

-- Id 1: Result type, must be pointer
-- Id 2: Unsigned value to cast to pointer
convertUToPtr :: Id -> Id -> Builder Id
convertUToPtr = threeOp OpConvertUToPtr

-- Id 1: Result type, must be same type as source pointer
-- Id 2: Source pointer, must have storage class of generic
ptrCastToGeneric :: Id -> Id -> Builder Id
ptrCastToGeneric = threeOp OpPtrCastToGeneric

-- Id 1: Result type, must be same type as source pointer
-- Id 2: Source pointer, must point to WorkgroupLocal, WorkgroupGlobal, Private
genericCastToPtr :: Id -> Id -> Builder Id
genericCastToPtr = threeOp OpGenericCastToPtr

-- Id 1: Result type, must have same width as operand
-- Id 2: Operand, must be numeric or pointer type
bitcast :: Id -> Id -> Builder Id
bitcast = threeOp OpBitcast

-- Id 1: Result type, must point to WorkgroupLocal, WorkgroupGlobal, Private
-- Id 2: Source pointer, must point to Generic
genericCastToPtrExplicit :: Id -> Id -> StorageClass -> Builder Id
genericCastToPtrExplicit = fourOp OpGenericCastToPtrExplicit

-- Id 1: Result type, must be signed integer scalar or vector
-- Id 2: Signed integer value to convert
satConvertSToU :: Id -> Id -> Builder Id
satConvertSToU = threeOp OpSatConvertSToU

-- Id 1: Result type, must be unsigned integer scalar or vector
-- Id 2: Unsigned integer value to convert
satConvertUToS :: Id -> Id -> Builder Id
satConvertUToS = threeOp OpSatConvertUToS

-- Id 1: Result type, must be same type as given vector
-- Id 2: Vector to read from, must be vector type
-- Id 3: Index, must be scalar integer type
vectorExtractDynamic :: Id -> Id -> Id -> Builder Id
vectorExtractDynamic = fourOp OpVectorExtractDynamic

-- Id 1: Result type, must be same type as given vector
-- Id 2: Vector to insert into, must be vector type
-- Id 3: Component to write into given vector
-- Id 4: Index of vector to insert value
vectorInsertDynamic :: Id -> Id -> Id -> Id -> Builder Id
vectorInsertDynamic = fiveOp OpVectorInsertDynamic

-- Id 1: Result type, must be same type as given vector
-- Id 2: First vector
-- Id 3: Second vector
-- Word32s: List of indices to use to construct new vector from the given
--          vectors concatenated together
vectorShuffle :: Id -> Id -> Id -> [Word32] -> Builder Id
vectorShuffle = fiveOp OpVectorShuffle

-- Id 1: Result type, must be a composite type
-- Ids: Components of the composite to construct
compositeConstruct :: Id -> [Id] -> Builder Id
compositeConstruct = threeOp OpCompositeConstruct

-- Id 1: Result type, must be the type of the component being extracted
-- Id 2: Composite to extract from
-- Word32s: Indexes to walk to reach the component to extract
compositeExtract :: Id -> Id -> [Word32] -> Builder Id
compositeExtract = fourOp OpCompositeExtract

-- Id 1: Result type, must be the type of the component to insert
-- Id 2: Object to insert into the given composite
-- Id 3: Composite to insert into
-- Words32s: Indexes to walk to reach the component to insert
compositeInsert :: Id -> Id -> Id -> [Word32] -> Builder Id
compositeInsert = fiveOp OpCompositeInsert

-- Id 1: Result type, must be the same as the given object's type
-- Id 2: Object to copy
copyObject :: Id -> Id -> Builder Id
copyObject = threeOp OpCopyObject

-- Id 1: Result type, must be the type of the given matrix transposed
-- Id 2: Matrix to transpose
transpose :: Id -> Id -> Builder Id
transpose = threeOp OpTranspose

-- Id 1: Result type, must be signed integer
-- Id 2: Signed value to negate
sNegate :: Id -> Id -> Builder Id
sNegate = threeOp OpSNegate

-- Id 1: Result type, must be float
-- Id 2: Float value to negate
fNegate :: Id -> Id -> Builder Id
fNegate = threeOp OpFNegate

-- Id 1: Result type, must be integer type
-- Id 2: Operand to complement the bits of
bNot :: Id -> Id -> Builder Id
bNot = threeOp OpNot

-- Id 1: Result type, must be integer type
-- Id 2: First operand
-- Id 3: Second operand
iAdd :: Id -> Id -> Id -> Builder Id
iAdd = fourOp OpIAdd

-- Id 1: Result type, must be float type
-- Id 2: First operand
-- Id 3: Second operand
fAdd :: Id -> Id -> Id -> Builder Id
fAdd = fourOp OpFAdd

-- Id 1: Result type, must be integer type
-- Id 2: First operand
-- Id 3: Second operand
iSub :: Id -> Id -> Id -> Builder Id
iSub = fourOp OpISub

-- Id 1: Result type, must be float type
-- Id 2: First operand
-- Id 3: Second operand
fSub :: Id -> Id -> Id -> Builder Id
fSub = fourOp OpFSub

-- Id 1: Result type, must be integer type
-- Id 2: First operand
-- Id 3: Second operand
iMul :: Id -> Id -> Id -> Builder Id
iMul = fourOp OpIMul

-- Id 1: Result type, must be float type
-- Id 2: First operand
-- Id 3: Second operand
fMul :: Id -> Id -> Id -> Builder Id
fMul = fourOp OpFMul

-- Id 1: Result type, must be unsigned integer type
-- Id 2: First operand
-- Id 3: Second operand
uDiv :: Id -> Id -> Id -> Builder Id
uDiv = fourOp OpUDiv

-- Id 1: Result type, must be signed integer type
-- Id 2: First operand
-- Id 3: Second operand
sDiv :: Id -> Id -> Id -> Builder Id
sDiv = fourOp OpSDiv

-- Id 1: Result type, must be float type
-- Id 2: First operand
-- Id 3: Second operand
fDiv :: Id -> Id -> Id -> Builder Id
fDiv = fourOp OpFDiv

-- Id 1: Result type, must be unsigned integer type
-- Id 2: First operand
-- Id 3: Second operand
uMod :: Id -> Id -> Id -> Builder Id
uMod = fourOp OpUMod

-- Id 1: Result type, must be signed integer type
-- Id 2: First operand
-- Id 3: Second operand
sRem :: Id -> Id -> Id -> Builder Id
sRem = fourOp OpSRem

-- Id 1: Result type, must be signed integer type
-- Id 2: First operand
-- Id 3: Second operand
sMod :: Id -> Id -> Id -> Builder Id
sMod = fourOp OpSMod

-- Id 1: Result type, must be float type
-- Id 2: First operand
-- Id 3: Second operand
fRem :: Id -> Id -> Id -> Builder Id
fRem = fourOp OpFRem

-- Id 1: Result type, must be float type
-- Id 2: First operand
-- Id 3: Second operand
fMod :: Id -> Id -> Id -> Builder Id
fMod = fourOp OpFMod

-- Id 1: Result type, must be same vector type as first operand
-- Id 2: Vector, must be floating point vector
-- Id 3: Scalar, must be floating point value
vectorTimesScalar :: Id -> Id -> Id -> Builder Id
vectorTimesScalar = fourOp OpVectorTimesScalar

-- Id 1: Result type, must be same matrix type as first operand
-- Id 2: Matrix, must be floating point matrix
-- Id 3: Scalar, must be floating point value
matrixTimesScalar :: Id -> Id -> Id -> Builder Id
matrixTimesScalar = fourOp OpMatrixTimesScalar

-- Id 1: Result type, must be vector type with same number of columns as matrix
-- Id 2: Vector, must be floating point vector
-- Id 3: Matrix, must be floating point matrix
vectorTimesMatrix :: Id -> Id -> Id -> Builder Id
vectorTimesMatrix = fourOp OpVectorTimesMatrix

-- Id 1: Result type, must be vector type with same number of rows as matrix
-- Id 2: Matrix, must be floating point matrix
-- Id 3: Vector, must be floating point vector
matrixTimesVector :: Id -> Id -> Id -> Builder Id
matrixTimesVector = fourOp OpMatrixTimesVector

-- Id 1: Result type, must be matrix type
-- Id 2: Matrix, must be floating point matrix
-- Id 3: Matrix, must be floating point matrix
matrixTimesMatrix :: Id -> Id -> Id -> Builder Id
matrixTimesMatrix = fourOp OpMatrixTimesMatrix

-- Id 1: Result type, must be matrix type
-- Id 2: Vector, must be floating point vector
-- Id 3: Vector, must be floating point vector
outerProduct :: Id -> Id -> Id -> Builder Id
outerProduct = fourOp OpOuterProduct

-- Id 1: Result type, must be floating point type
-- Id 2: Vector, must be floating point vector
-- Id 3: Vector, must be floating point vector
dot :: Id -> Id -> Id -> Builder Id
dot = fourOp OpDot

-- Id 1: Result type, must be integer type
-- Id 2: First operand
-- Id 3: Second operand
shiftRightLogical :: Id -> Id -> Id -> Builder Id
shiftRightLogical = fourOp OpShiftRightLogical

-- Id 1: Result type, must be integer type
-- Id 2: First operand
-- Id 3: Second operand
shiftRightArithmetic :: Id -> Id -> Id -> Builder Id
shiftRightArithmetic = fourOp OpShiftRightArithmetic

-- Id 1: Result type, must be integer type
-- Id 2: First operand
-- Id 3: Second operand
shiftLeftLogical :: Id -> Id -> Id -> Builder Id
shiftLeftLogical = fourOp OpShiftLeftLogical

-- Id 1: Result type, must be integer type
-- Id 2: First operand
-- Id 3: Second operand
bitwiseOr :: Id -> Id -> Id -> Builder Id
bitwiseOr = fourOp OpBitwiseOr

-- Id 1: Result type, must be integer type
-- Id 2: First operand
-- Id 3: Second operand
bitwiseXor :: Id -> Id -> Id -> Builder Id
bitwiseXor = fourOp OpBitwiseXor

-- Id 1: Result type, must be integer type
-- Id 2: First operand
-- Id 3: Second operand
bitwiseAnd :: Id -> Id -> Id -> Builder Id
bitwiseAnd = fourOp OpBitwiseAnd

-- Id 1: Result type, must be boolean scalar type
-- Id 2: Operand, must be boolean vector type
vAny :: Id -> Id -> Builder Id
vAny = threeOp OpAny

-- Id 1: Result type, must be boolean scalar type
-- Id 2: Operand, must be boolean vector type
vAll :: Id -> Id -> Builder Id
vAll = threeOp OpAll

-- Id 1: Result type, must be boolean type
-- Id 2: Operand, must be floating point type
fIsNaN :: Id -> Id -> Builder Id
fIsNaN = threeOp OpIsNaN

-- Id 1: Result type, must be boolean type
-- Id 2: Operand, must be floating point type
isInf :: Id -> Id -> Builder Id
isInf = threeOp OpIsInf

-- Id 1: Result type, must be boolean type
-- Id 2: Operand, must be floating point type
isFinite :: Id -> Id -> Builder Id
isFinite = threeOp OpIsFinite

-- Id 1: Result type, must be boolean type
-- Id 2: Operand, must be floating point type
isNormal :: Id -> Id -> Builder Id
isNormal = threeOp OpIsNormal

-- Id 1: Result type, must be boolean type
-- Id 2: Operand, must be numeric type
signBitSet :: Id -> Id -> Builder Id
signBitSet = threeOp OpSignBitSet

-- Id 1: Result type, must be boolean type
-- Id 2: First operand, must be numeric type
-- Id 3: Second operand, must be numeric type
lessOrGreater :: Id -> Id -> Id -> Builder Id
lessOrGreater = fourOp OpLessOrGreater

-- Id 1: Result type, must be boolean type
-- Id 2: First operand, must be numeric type
-- Id 3: Second operand, must be numeric type
ordered :: Id -> Id -> Id -> Builder Id
ordered = fourOp OpOrdered

-- Id 1: Result type, must be boolean type
-- Id 2: First operand, must be numeric type
-- Id 3: Second operand, must be numeric type
unordered :: Id -> Id -> Id -> Builder Id
unordered = fourOp OpUnordered

-- Id 1: Result type, must be boolean type
-- Id 2: First operand, must be numeric type
-- Id 3: Second operand, must be numeric type
logicalOr :: Id -> Id -> Id -> Builder Id
logicalOr = fourOp OpLogicalOr

-- Id 1: Result type, must be boolean type
-- Id 2: First operand, must be numeric type
-- Id 3: Second operand, must be numeric type
logicalXor :: Id -> Id -> Id -> Builder Id
logicalXor = fourOp OpLogicalXor

-- Id 1: Result type, must be boolean type
-- Id 2: First operand, must be numeric type
-- Id 3: Second operand, must be numeric type
logicalAnd :: Id -> Id -> Id -> Builder Id
logicalAnd = fourOp OpLogicalAnd

-- Id 1: Result type, must be same type as both operands
-- Id 2: Condition to select upon, must be boolean type
-- Id 3: First operand
-- Id 4: Second operand
select :: Id -> Id -> Id -> Id -> Builder Id
select t c v1 v2 = do
    i <- genId
    writeInstruction (addOpInstruction (OpSelect t i c v1 v2))
    return i

-- Id 1: Result type, must be boolean type
-- Id 2: First operand
-- Id 3: Second operand
iEqual :: Id -> Id -> Id -> Builder Id
iEqual = fourOp OpIEqual

-- Id 1: Result type, must be boolean type
-- Id 2: First operand
-- Id 3: Second operand
fOrdEqual :: Id -> Id -> Id -> Builder Id
fOrdEqual = fourOp OpFOrdEqual

-- Id 1: Result type, must be boolean type
-- Id 2: First operand
-- Id 3: Second operand
fUnordEqual :: Id -> Id -> Id -> Builder Id
fUnordEqual = fourOp OpFUnordEqual

-- Id 1: Result type, must be boolean type
-- Id 2: First operand
-- Id 3: Second operand
iNotEqual :: Id -> Id -> Id -> Builder Id
iNotEqual = fourOp OpINotEqual

-- Id 1: Result type, must be boolean type
-- Id 2: First operand
-- Id 3: Second operand
fOrdNotEqual :: Id -> Id -> Id -> Builder Id
fOrdNotEqual = fourOp OpFOrdNotEqual

-- Id 1: Result type, must be boolean type
-- Id 2: First operand
-- Id 3: Second operand
fUnordNotEqual :: Id -> Id -> Id -> Builder Id
fUnordNotEqual = fourOp OpFUnordNotEqual

-- Id 1: Result type, must be boolean type
-- Id 2: First operand
-- Id 3: Second operand
uLessThan :: Id -> Id -> Id -> Builder Id
uLessThan = fourOp OpULessThan

-- Id 1: Result type, must be boolean type
-- Id 2: First operand
-- Id 3: Second operand
sLessThan :: Id -> Id -> Id -> Builder Id
sLessThan = fourOp OpSLessThan

-- Id 1: Result type, must be boolean type
-- Id 2: First operand
-- Id 3: Second operand
fOrdLessThan :: Id -> Id -> Id -> Builder Id
fOrdLessThan = fourOp OpFOrdLessThan

-- Id 1: Result type, must be boolean type
-- Id 2: First operand
-- Id 3: Second operand
fUnordLessThan :: Id -> Id -> Id -> Builder Id
fUnordLessThan = fourOp OpFUnordLessThan

-- Id 1: Result type, must be boolean type
-- Id 2: First operand
-- Id 3: Second operand
uGreaterThan :: Id -> Id -> Id -> Builder Id
uGreaterThan = fourOp OpUGreaterThan

-- Id 1: Result type, must be boolean type
-- Id 2: First operand
-- Id 3: Second operand
sGreaterThan :: Id -> Id -> Id -> Builder Id
sGreaterThan = fourOp OpSGreaterThan

-- Id 1: Result type, must be boolean type
-- Id 2: First operand
-- Id 3: Second operand
fOrdGreaterThan :: Id -> Id -> Id -> Builder Id
fOrdGreaterThan = fourOp OpFOrdGreaterThan

-- Id 1: Result type, must be boolean type
-- Id 2: First operand
-- Id 3: Second operand
fUnordGreaterThan :: Id -> Id -> Id -> Builder Id
fUnordGreaterThan = fourOp OpFUnordGreaterThan

-- Id 1: Result type, must be boolean type
-- Id 2: First operand
-- Id 3: Second operand
uLessThanEqual :: Id -> Id -> Id -> Builder Id
uLessThanEqual = fourOp OpULessThanEqual

-- Id 1: Result type, must be boolean type
-- Id 2: First operand
-- Id 3: Second operand
sLessThanEqual :: Id -> Id -> Id -> Builder Id
sLessThanEqual = fourOp OpSLessThanEqual

-- Id 1: Result type, must be boolean type
-- Id 2: First operand
-- Id 3: Second operand
fOrdLessThanEqual :: Id -> Id -> Id -> Builder Id
fOrdLessThanEqual = fourOp OpFOrdLessThanEqual

-- Id 1: Result type, must be boolean type
-- Id 2: First operand
-- Id 3: Second operand
fUnordLessThanEqual :: Id -> Id -> Id -> Builder Id
fUnordLessThanEqual = fourOp OpFUnordLessThanEqual

-- Id 1: Result type, must be boolean type
-- Id 2: First operand
-- Id 3: Second operand
uGreaterThanEqual :: Id -> Id -> Id -> Builder Id
uGreaterThanEqual = fourOp OpUGreaterThanEqual

-- Id 1: Result type, must be boolean type
-- Id 2: First operand
-- Id 3: Second operand
sGreaterThanEqual :: Id -> Id -> Id -> Builder Id
sGreaterThanEqual = fourOp OpSGreaterThanEqual

-- Id 1: Result type, must be boolean type
-- Id 2: First operand
-- Id 3: Second operand
fOrdGreaterThanEqual :: Id -> Id -> Id -> Builder Id
fOrdGreaterThanEqual = fourOp OpFOrdGreaterThanEqual

-- Id 1: Result type, must be boolean type
-- Id 2: First operand
-- Id 3: Second operand
fUnordGreaterThanEqual :: Id -> Id -> Id -> Builder Id
fUnordGreaterThanEqual = fourOp OpFUnordGreaterThanEqual

-- Id 1: Result type, must be float type
-- Id 2: Operand
dPdx :: Id -> Id -> Builder Id
dPdx = threeOp OpDPdx

-- Id 1: Result type, must be float type
-- Id 2: Operand
dPdy :: Id -> Id -> Builder Id
dPdy = threeOp OpDPdy

-- Id 1: Result type, must be float type
-- Id 2: Operand
fWidth :: Id -> Id -> Builder Id
fWidth = threeOp OpFwidth

-- Id 1: Result type, must be float type
-- Id 2: Operand
dPdxFine :: Id -> Id -> Builder Id
dPdxFine = threeOp OpDPdxFine

-- Id 1: Result type, must be float type
-- Id 2: Operand
dPdyFine :: Id -> Id -> Builder Id
dPdyFine = threeOp OpDPdyFine

-- Id 1: Result type, must be float type
-- Id 2: Operand
fWidthFine :: Id -> Id -> Builder Id
fWidthFine = threeOp OpFwidthFine

-- Id 1: Result type, must be float type
-- Id 2: Operand
dPdxCoarse :: Id -> Id -> Builder Id
dPdxCoarse = threeOp OpDPdxCoarse

-- Id 1: Result type, must be float type
-- Id 2: Operand
dPdyCoarse :: Id -> Id -> Builder Id
dPdyCoarse = threeOp OpDPdyCoarse

-- Id 1: Result type, must be float type
-- Id 2: Operand
fWidthCoarse :: Id -> Id -> Builder Id
fWidthCoarse = threeOp OpFwidthCoarse

-- Id: Result type
-- Id 1s: Variable, must have same type as result
-- Id 2s: Parent block
phi :: Id -> [(Id, Id)] -> Builder Id
phi = threeOp OpPhi

-- Id: Label of merge block
loopMerge :: Id -> [LoopControl] -> Builder ()
loopMerge l lc = zeroOp (OpLoopMerge l lc)

-- Id: Label of merge block
selectionMerge :: Id -> [SelectionControl] -> Builder ()
selectionMerge l sc = zeroOp (OpSelectionMerge l sc)

label :: Builder Id
label = oneOp OpLabel

-- Id: Target label to branch to
branch :: Id -> Builder ()
branch t = zeroOp (OpBranch t)

-- Id 1: Conditional, must be a boolean type
-- Id 2: Label to take if the condition is true, must be in current function
-- Id 3: Label to take if the condition is false, must be in current function
-- Maybe (Word32, Word32): Branch weights
branchConditional :: Id -> Id -> Id -> Maybe (Word32, Word32) -> Builder ()
branchConditional c t f w = zeroOp (OpBranchConditional c t f w)

-- Id 1: Selector value, must be scalar integer type
-- Id 2: Default label
-- Word32s: Scalar integer literal
-- Ids: Corresponding label
switch :: Id -> Id -> [(Word32, Id)] -> Builder ()
switch s d ts = zeroOp (OpSwitch s d ts)

kill :: Builder ()
kill = zeroOp OpKill

return_ :: Builder ()
return_ = zeroOp OpReturn

-- Id: Value to return
returnValue :: Id -> Builder ()
returnValue v = zeroOp (OpReturnValue v)

unreachable :: Builder ()
unreachable = zeroOp OpUnreachable

-- Id: Value whose lifetime starts here
-- Word32: 0 if Id is non-void, otherwise is the amount of memory whose lifetime
--         is starting
lifetimeStart :: Id -> Word32 -> Builder ()
lifetimeStart i v = zeroOp (OpLifetimeStart i v)

-- Id: Value whose lifetime ends here
-- Word32: 0 if Id is non-void, otherwise is the amount of memory whose lifetime
--         is ending
lifetimeStop :: Id -> Word32 -> Builder ()
lifetimeStop i v = zeroOp (OpLifetimeStart i v)

-- Id 1: Pointer
-- Id 2: Value, must have same type as the value Pointer points to
atomicInit :: Id -> Id -> Builder ()
atomicInit p v = zeroOp (OpAtomicInit p v)

-- Id 1: Result type
-- Id 2: Pointer to load
atomicLoad :: Id -> Id -> ExecutionScope -> [MemorySemantics] -> Builder Id
atomicLoad = fiveOp OpAtomicLoad

-- Id 1: Pointer
-- Id 2: Value to store
atomicStore :: Id -> ExecutionScope -> [MemorySemantics] -> Id -> Builder ()
atomicStore p es ms v = zeroOp (OpAtomicStore p es ms v)

-- Id 1: Result type
-- Id 2: Pointer
-- Id 3: Value to exchange
atomicExchange :: Id -> Id -> ExecutionScope -> [MemorySemantics] -> Id
               -> Builder Id
atomicExchange = sixOp OpAtomicExchange

-- Id 1: Result type
-- Id 2: Pointer
-- Id 3: Value
-- Id 4: Comparator
atomicCompareExchange :: Id -> Id -> ExecutionScope -> [MemorySemantics] -> Id
                      -> Id -> Builder Id
atomicCompareExchange = sevenOp OpAtomicCompareExchange

-- Id 1: Result type
-- Id 2: Pointer
-- Id 3: Value
-- Id 4: Comparator
atomicCompareExchangeWeak :: Id -> Id -> ExecutionScope -> [MemorySemantics]
                          -> Id -> Id -> Builder Id
atomicCompareExchangeWeak = sevenOp OpAtomicCompareExchangeWeak

-- Id 1: Result type
-- Id 2: Pointer, must point to scalar integer type
atomicIIncrement :: Id -> Id -> ExecutionScope -> [MemorySemantics]
                 -> Builder Id
atomicIIncrement = fiveOp OpAtomicIIncrement

-- Id 1: Result type
-- Id 2: Pointer, must point to scalar integer type
atomicIDecrement :: Id -> Id -> ExecutionScope -> [MemorySemantics]
                 -> Builder Id
atomicIDecrement = fiveOp OpAtomicIDecrement

-- Id 1: Result type
-- Id 2: Pointer, must point to scalar integer type
-- Id 3: Value
atomicIAdd :: Id -> Id -> ExecutionScope -> [MemorySemantics] -> Id
           -> Builder Id
atomicIAdd = sixOp OpAtomicIAdd

-- Id 1: Result type
-- Id 2: Pointer, must point to scalar integer type
-- Id 3: Value
atomicISub :: Id -> Id -> ExecutionScope -> [MemorySemantics] -> Id
           -> Builder Id
atomicISub = sixOp OpAtomicISub

-- Id 1: Result type
-- Id 2: Pointer, must point to scalar integer type
-- Id 3: Value
atomicUMin :: Id -> Id -> ExecutionScope -> [MemorySemantics] -> Id
           -> Builder Id
atomicUMin = sixOp OpAtomicUMin

-- Id 1: Result type
-- Id 2: Pointer, must point to scalar integer type
-- Id 3: Value
atomicUMax :: Id -> Id -> ExecutionScope -> [MemorySemantics] -> Id
           -> Builder Id
atomicUMax = sixOp OpAtomicUMax

-- Id 1: Result type
-- Id 2: Pointer, must point to scalar integer type
-- Id 3: Value
atomicAnd :: Id -> Id -> ExecutionScope -> [MemorySemantics] -> Id -> Builder Id
atomicAnd = sixOp OpAtomicAnd

-- Id 1: Result type
-- Id 2: Pointer, must point to scalar integer type
-- Id 3: Value
atomicOr :: Id -> Id -> ExecutionScope -> [MemorySemantics] -> Id -> Builder Id
atomicOr = sixOp OpAtomicOr

-- Id 1: Result type
-- Id 2: Pointer, must point to scalar integer type
-- Id 3: Value
atomicXor :: Id -> Id -> ExecutionScope -> [MemorySemantics] -> Id -> Builder Id
atomicXor = sixOp OpAtomicXor

-- Id 1: Result type
-- Id 2: Pointer, must point to scalar integer type
-- Id 3: Value
atomicIMin :: Id -> Id -> ExecutionScope -> [MemorySemantics] -> Id
           -> Builder Id
atomicIMin = sixOp OpAtomicIMin

-- Id 1: Result type
-- Id 2: Pointer, must point to scalar integer type
-- Id 3: Value
atomicIMax :: Id -> Id -> ExecutionScope -> [MemorySemantics] -> Id
           -> Builder Id
atomicIMax = sixOp OpAtomicIMax

emitVertex :: Builder ()
emitVertex = zeroOp OpEmitVertex

endPrimitive :: Builder ()
endPrimitive = zeroOp OpEndPrimitive

-- Id 1: Stream, must be a constant scalar integer type
emitStreamVertex :: Id -> Builder ()
emitStreamVertex v = zeroOp (OpEmitStreamVertex v)

-- Id 1: Stream, must be a constant scalar integer type
endStreamPrimitive :: Id -> Builder ()
endStreamPrimitive v = zeroOp (OpEndStreamPrimitive v)

controlBarrier :: ExecutionScope -> Builder ()
controlBarrier es = zeroOp (OpControlBarrier es)

memoryBarrier :: ExecutionScope -> [MemorySemantics] -> Builder ()
memoryBarrier es ms = zeroOp (OpMemoryBarrier es ms)

-- Id 1: Result type
-- Id 2: Destination pointer
-- Id 3: Source pointer
-- Id 4: Number of elements, must be 32/64 bit integer depending on addressing
--       model
-- Id 5: Stride, must be 32/64 bit integer depending on addressing model
-- Id 6: Event
asyncGroupCopy :: Id -> ExecutionScope -> Id -> Id -> Id -> Id -> Id
               -> Builder Id
asyncGroupCopy = eightOp OpAsyncGroupCopy

-- Id 1: Result type
-- Id 2: Number of events, must be 32 bit integer type
-- Id 3: Events list, must be a pointer to an event type
waitGroupEvents :: Id -> ExecutionScope -> Id -> Id -> Builder Id
waitGroupEvents = fiveOp OpWaitGroupEvents

-- Id 1: Result type, must be scalar boolean type
-- Id 2: Predicate, must be scalar boolean type
groupAll :: Id -> ExecutionScope -> Id -> Builder Id
groupAll = fourOp OpGroupAll

-- Id 1: Result type, must be scalar boolean type
-- Id 2: Predicate, must be scalar boolean type
groupAny :: Id -> ExecutionScope -> Id -> Builder Id
groupAny = fourOp OpGroupAny

-- Id 1: Result type
-- Id 2: Value
-- Id 3: LocalId, must be integer type. Can be scalar, vec2, or vec3
-- Value and Result type must be one of:
--   32/64 bit integer, 16/32/64 bit float
groupBroadcast :: Id -> ExecutionScope -> Id -> Id -> Builder Id
groupBroadcast = fiveOp OpGroupBroadcast

-- Id 1: Result type
-- Id 2: Value to add to group
groupIAdd :: Id -> ExecutionScope -> GroupOperation -> Id -> Builder Id
groupIAdd = fiveOp OpGroupIAdd

-- Id 1: Result type
-- Id 2: Value to add to group
groupFAdd :: Id -> ExecutionScope -> GroupOperation -> Id -> Builder Id
groupFAdd = fiveOp OpGroupFAdd

-- Id 1: Result type
-- Id 2: Value to min to group
groupFMin :: Id -> ExecutionScope -> GroupOperation -> Id -> Builder Id
groupFMin = fiveOp OpGroupFMin

-- Id 1: Result type
-- Id 2: Value to min to group
groupUMin :: Id -> ExecutionScope -> GroupOperation -> Id -> Builder Id
groupUMin = fiveOp OpGroupUMin

-- Id 1: Result type
-- Id 2: Value to min to group
groupSMin :: Id -> ExecutionScope -> GroupOperation -> Id -> Builder Id
groupSMin = fiveOp OpGroupSMin

-- Id 1: Result type
-- Id 2: Value to max to group
groupFMax :: Id -> ExecutionScope -> GroupOperation -> Id -> Builder Id
groupFMax = fiveOp OpGroupFMax

-- Id 1: Result type
-- Id 2: Value to max to group
groupUMax :: Id -> ExecutionScope -> GroupOperation -> Id -> Builder Id
groupUMax = fiveOp OpGroupUMax

-- Id 1: Result type
-- Id 2: Value to max to group
groupSMax :: Id -> ExecutionScope -> GroupOperation -> Id -> Builder Id
groupSMax = fiveOp OpGroupSMax

-- Id 1: Result type
-- Id 2: q (see docs)
-- Id 3: Number of events, must be 32 bit integer
-- Id 4: Wait events, must be pointer to device events
-- Id 5: Retained events, must be pointer to device events
enqueueMarker :: Id -> Id -> Id -> Id -> Id -> Builder Id
enqueueMarker = sixOp OpEnqueueMarker

-- Id 1: Result type, must be 32 bit integer
-- Id 2: q (see docs)
-- Id 3: ND range, must be struct built with buildNDRange
-- Id 4: Number of events, must be signed 32 bit integer
-- Id 5: Wait events, must be pointer to device events
-- Id 6: Retained events, must be pointer to device events
-- Id 7: Invoke, must be function type (see docs)
-- Id 8: Param
-- Id 9: Param size
-- Id 10: Param align
-- Ids: Local size, optional list of 32 bit integers
enqueueKernel :: Id -> Id -> KernelEnqueueFlags -> Id -> Id -> Id -> Id -> Id
              -> Id -> Id -> Id -> [Id] -> Builder Id
enqueueKernel t q kef ndr ne we re i p ps pa ls = do -- WTF
    i' <- genId
    writeInstruction (addOpInstruction
        (OpEnqueueKernel t i' q kef ndr ne we re i p ps pa ls))
    return i'

-- Id 1: Result type, must be 32 bit integer
-- Id 2: ND range, must be struct built with buildNDrange
-- Id 3: Invoke, must be function type (see docs)
getKernelNDrangeSubGroupCount :: Id -> Id -> Id -> Builder Id
getKernelNDrangeSubGroupCount = fourOp OpGetKernelNDrangeSubGroupCount

-- Id 1: Result type, must be 32 bit integer
-- Id 2: ND range, must be struct built with buildNDrange
-- Id 3: Invoke, must be function type (see docs)
getKernelNDrangeMaxSubGroupCount :: Id -> Id -> Id -> Builder Id
getKernelNDrangeMaxSubGroupCount = fourOp OpGetKernelNDrangeMaxSubGroupCount

-- Id 1: Result type, must be 32 bit integer
-- Id 2: Invoke, must be function type (see docs)
getKernelWorkGroupSize :: Id -> Id -> Builder Id
getKernelWorkGroupSize = threeOp OpGetKernelWorkGroupSize

-- Id 1: Result type, must be 32 bit integer
-- Id 2: Invoke, must be function type (see docs)
getKernelPreferredWorkGroupSizeMultiple :: Id -> Id -> Builder Id
getKernelPreferredWorkGroupSizeMultiple =
    threeOp OpGetKernelPreferredWorkGroupSizeMultiple

-- Id: Event, must be created by enqueueKernel, enqueueMarker, createUserEvent
retainEvent :: Id -> Builder ()
retainEvent e = zeroOp (OpRetainEvent e)

-- Id: Event, must be created by enqueueKernel, enqueueMarker, createUserEvent
releaseEvent :: Id -> Builder ()
releaseEvent e = zeroOp (OpReleaseEvent e)

-- Id: Result type, must be device event
createUserEvent :: Id -> Builder Id
createUserEvent = twoOp OpCreateUserEvent

-- Id 1: Result type, must be boolean type
-- Id 2: Event, must be device event type
isValidEvent :: Id -> Id -> Builder Id
isValidEvent = threeOp OpIsValidEvent

-- Id 1: Event, must be device event type created by createUserEvent
-- Id 2: Status, must be 32 bit integer
setUserEventStatus :: Id -> Id -> Builder ()
setUserEventStatus e s = zeroOp (OpSetUserEventStatus e s)

-- Id 1: Event, must be device event created by enqueueKernel or enqueueMarker
-- Id 2: Value (see docs)
captureEventProfilingInfo :: Id -> [KernelProfilingInfo] -> Id -> Builder ()
captureEventProfilingInfo e i v = zeroOp (OpCaptureEventProfilingInfo e i v)

-- Id: Result type, must be queue
getDefaultQueue :: Id -> Builder Id
getDefaultQueue = twoOp OpGetDefaultQueue

-- Id 1: Result type, must be type struct (see docs)
-- Id 2: Global work size
-- Id 3: Local work size
-- Id 4: Global work offset
buildNDRange :: Id -> Id -> Id -> Id -> Builder Id
buildNDRange = fiveOp OpBuildNDRange

-- Id 1: Result type
-- Id 2: Pipe, must be pipe type with ReadOnly access qualifier
-- Id 3: Pointer, must have generic storage class
readPipe :: Id -> Id -> Id -> Builder Id
readPipe = fourOp OpReadPipe

-- Id 1: Result type, must be 32 bit integer
-- Id 2: Pipe, must be pipe type with WriteOnly access qualifier
-- Id 3: Pointer, must have generic storage class
writePipe :: Id -> Id -> Id -> Builder Id
writePipe = fourOp OpWritePipe

-- Id 1: Result type, must be 32 bit integer
-- Id 2: Pipe, must be pipe type with ReadOnly access qualifier
-- Id 3: Reserve ID, must have type reserve ID
-- Id 4: Index, must be 32 bit integer
-- Id 5: Pointer, must have generic storage class
reservedReadPipe :: Id -> Id -> Id -> Id -> Id -> Builder Id
reservedReadPipe = sixOp OpReservedReadPipe

-- Id 1: Result type, must be 32 bit integer
-- Id 2: Pipe, must be pipe type with WriteOnly accesss qualifier
-- Id 3: Reserve ID, must have type reserve ID
-- Id 4: Index, must be 32 bit integer
-- Id 5: Pointer, must have generic storage class
reservedWritePipe :: Id -> Id -> Id -> Id -> Id -> Builder Id
reservedWritePipe = sixOp OpReservedWritePipe

-- Id 1: Result type
-- Id 2: Pipe
-- Id 3: Number of packets
reserveReadPipePackets :: Id -> Id -> Id -> Builder Id
reserveReadPipePackets = fourOp OpReserveReadPipePackets

-- Id 1: Result type
-- Id 2: Pipe
-- Id 3: Number of packets
reserveWritePipePackets :: Id -> Id -> Id -> Builder Id
reserveWritePipePackets = fourOp OpReserveWritePipePackets

-- Id 1: Pipe with ReadOnly access qualifier
-- Id 2: Reserve ID
commitReadPipe :: Id -> Id -> Builder ()
commitReadPipe p r = zeroOp (OpCommitReadPipe p r)

-- Id 1: Pipe with WriteOnly access qualifier
-- Id 2: Reserve ID
commitWritePipe :: Id -> Id -> Builder ()
commitWritePipe p r = zeroOp (OpCommitWritePipe p r)

-- Id 1: Result type, must be boolean type
-- Id 2: Reserve ID
isValidReserveId :: Id -> Id -> Builder Id
isValidReserveId = threeOp OpIsValidReserveId

-- Id 1: Result type, must be 32 bit integer
-- Id 2: Pipe with either ReadOnly or WriteOnly access qualifier
getNumPipePackets :: Id -> Id -> Builder Id
getNumPipePackets = threeOp OpGetNumPipePackets

-- Id 1: Result type, must be 32 bit integer
-- Id 2: Pipe with either ReadOnly or WriteOnly access qualifier
getMaxPipePackets :: Id -> Id -> Builder Id
getMaxPipePackets = threeOp OpGetMaxPipePackets

-- Id 1: Result type, must be reserve ID
-- Id 2: Pipe with ReadOnly access qualifier
-- Id 3: Number of packets, must be 32 bit integer
groupReserveReadPipePackets :: Id -> ExecutionScope -> Id -> Id -> Builder Id
groupReserveReadPipePackets = fiveOp OpGroupReservedReadPipePackets

-- Id 1: Result type, must be reserve ID
-- Id 2: Pipe with ReadOnly access qualifier
-- Id 3: Number of packets, must be 32 bit integer
groupReserveWritePipePackets :: Id -> ExecutionScope -> Id -> Id -> Builder Id
groupReserveWritePipePackets = fiveOp OpGroupReservedWritePipePackets

-- Id 1: Pipe with ReadOnly access qualifier
-- Id 2: Reserve ID
groupCommitReadPipe :: ExecutionScope -> Id -> Id -> Builder ()
groupCommitReadPipe es p r = zeroOp (OpGroupCommitReadPipe es p r)

-- Id 1: Pipe with ReadOnly access qualifier
-- Id 2: Reserve ID
groupCommitWritePipe :: ExecutionScope -> Id -> Id -> Builder ()
groupCommitWritePipe es p r = zeroOp (OpGroupCommitWritePipe es p r)
