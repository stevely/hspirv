{-
 - Builder.hs
 - By Steven Smith
 -}

module SpirV.Builder
( module SpirV.Builder
, R.buildModule
, R.nop
, R.source
, R.sourceExtension
, R.compileFlag
, R.extension
, R.memoryModel
, R.store
, R.copyMemory
, R.copyMemorySized
, R.functionEnd
, R.kill
, R.return_
, R.returnValue
, R.unreachable
, R.lifetimeStart
, R.lifetimeStop
, R.atomicInit
, R.atomicStore
, R.emitVertex
, R.endPrimitive
, R.emitStreamVertex
, R.endStreamPrimitive
, R.controlBarrier
, R.memoryBarrier
, R.retainEvent
, R.releaseEvent
, R.setUserEventStatus
, R.captureEventProfilingInfo
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

import Data.Bits
import Data.Int
import Data.Text (Text)
import Data.Word
import Unsafe.Coerce (unsafeCoerce)

import SpirV.Builder.Types
import SpirV.Instructions

import qualified SpirV.Builder.Raw as R

-- Id: Result type
undef :: TypeId -> Builder TypeId
undef (TypeId i) = fmap TypeId (R.undef i)

extInstImport :: Text -> Builder ExtSet
extInstImport t = fmap ExtSet (R.extInstImport t)

-- Id 1: Result type
-- Id 2: Set, result of ExtInstImport instruction
-- Word32: Enumerant of instruction to execute within extended instruction set
-- Ids: Operands to the instruction to execute
extInst :: TypeId -> ExtSet -> Word32 -> [Id] -> Builder Id
extInst (TypeId i) (ExtSet s) e is = R.extInst i s e is

-- String is used solely with Line, in order to provide a string containing the
-- filename this module was generated from
string :: Text -> Builder FileId
string t = fmap FileId (R.string t)

name :: IsId id => Text -> Builder id -> Builder id
name t b = do
    i <- b
    R.name (toId i) t
    return i

-- Word32: Line number
-- Word32: Column number
line :: IsId id => FileId -> Word32 -> Word32 -> Builder id -> Builder id
line (FileId f) l c b = do
    i <- b
    R.line (toId i) f l c
    return i

decorationGroup :: Builder DecorationGroup
decorationGroup = fmap DecorationGroup R.decorationGroup

-- Id: Target to decorate, can be a DecorationGroup
decorate :: IsId id => Decoration -> Builder id -> Builder id
decorate d b = do
    i <- b
    R.decorate (toId i) d
    return i

-- Id: Target to decorate, can be a DecorationGroup
-- Word32: Number of the member to decorate
memberDecorate :: IsId id => id -> Word32 -> Decoration -> Builder ()
memberDecorate i = R.memberDecorate (toId i)

groupDecorate :: DecorationGroup -> [Id] -> Builder ()
groupDecorate (DecorationGroup i) = R.groupDecorate i

groupMemberDecorate :: DecorationGroup -> [Id] -> Builder ()
groupMemberDecorate (DecorationGroup i) = R.groupMemberDecorate i

typeVoid :: Builder TypeId
typeVoid = fmap TypeId R.typeVoid

typeBool :: Builder TypeId
typeBool = fmap TypeId R.typeBool

-- Word32: Width, in bits (aka 32 for 32 bit integer)
typeInt :: Word32 -> Signedness -> Builder TypeId
typeInt w s = fmap TypeId (R.typeInt w s)

-- Word32: Width, in bits (aka 32 for 32 bit float)
typeFloat :: Word32 -> Builder TypeId
typeFloat w = fmap TypeId (R.typeFloat w)

-- Id: Type of components
-- Word32: Number of components, must be >= 2
typeVector :: TypeId -> Word32 -> Builder TypeId
typeVector (TypeId i) w = fmap TypeId (R.typeVector i w)

-- Id: Type of columns, must be vector type
-- Word32: Number of columns, must be >= 2
typeMatrix :: TypeId -> Word32 -> Builder TypeId
typeMatrix (TypeId i) w = fmap TypeId (R.typeMatrix i w)

-- Id: Type of components when sampled through this sampler
typeSampler :: TypeId -> Dim -> SamplerContent -> ArrayedContent
            -> DepthComparison -> MultiSampled -> Maybe AccessQualifier
            -> Builder TypeId
typeSampler (TypeId t) d sc ac dc ms aq =
    fmap TypeId (R.typeSampler t d sc ac dc ms aq)

typeFilter :: Builder TypeId
typeFilter = fmap TypeId R.typeFilter

-- Array types require a constant instruction to hold the length of the array.
-- This introduces an unsigned int type, creates the constant for the length,
-- then declares the array type
-- Id: Type of elements
-- Word32: Length of array
typeArray :: TypeId -> Word32 -> Builder TypeId
typeArray (TypeId t) l = do
    uint32 <- R.typeInt 32 Unsigned
    i <- R.constant uint32 [l]
    fmap TypeId (R.typeArray t i)

-- Id: Type of elements
typeRuntimeArray :: TypeId -> Builder TypeId
typeRuntimeArray (TypeId i) = fmap TypeId (R.typeRuntimeArray i)

-- Ids: List of types for each component in the struct, in order
typeStruct :: [TypeId] -> Builder TypeId
typeStruct ts = fmap TypeId (R.typeStruct (fmap toId ts))

-- Ids: List of types for each component in the struct, in order
-- Texts: Names of struct components
typeStructNamed :: [(TypeId, Text)] -> Builder TypeId
typeStructNamed its = do
    struct <- R.typeStruct (map (runTypeId . fst) its)
    mapM_ (go struct) (zipWith (\(_,t) n -> (n,t)) its [0..])
    return (TypeId struct)
  where
    go struct (n,t) = R.memberName struct n t

-- Text: Name of opaque type
typeOpaque :: Text -> Builder TypeId
typeOpaque t = fmap TypeId (R.typeOpaque t)

-- Id: Type of object being pointed to
typePointer :: StorageClass -> TypeId -> Builder TypeId
typePointer sc (TypeId i) = fmap TypeId (R.typePointer sc i)

-- Id: Return type
-- Ids: Types of parameters, in order
typeFunction :: TypeId -> [TypeId] -> Builder TypeId
typeFunction (TypeId i) ts = fmap TypeId (R.typeFunction i (fmap toId ts))

-- Id: Type of data in pipe
typePipe :: TypeId -> AccessQualifier -> Builder TypeId
typePipe (TypeId i) aq = fmap TypeId (R.typePipe i aq)

constantTrue :: Builder Id
constantTrue = do
    bool <- R.typeBool
    R.constantTrue bool

constantFalse :: Builder Id
constantFalse = do
    bool <- R.typeBool
    R.constantFalse bool

constantInt32 :: Int32 -> Builder Id
constantInt32 i = do
    int32 <- R.typeInt 32 Signed
    R.constant int32 [fromIntegral i]

constantInt64 :: Int64 -> Builder Id
constantInt64 i = do
    int64 <- R.typeInt 64 Signed
    R.constant int64 [lowOrder, highOrder]
  where
    i' = fromIntegral i :: Word64
    lowOrder = fromIntegral (i' .&. 0xFFFF)
    highOrder = fromIntegral (shiftR i' 32)

constantWord32 :: Word32 -> Builder Id
constantWord32 w = do
    uint32 <- R.typeInt 32 Unsigned
    R.constant uint32 [w]

constantWord64 :: Word64 -> Builder Id
constantWord64 w = do
    uint64 <- R.typeInt 64 Unsigned
    R.constant uint64 [lowOrder, highOrder]
  where
    lowOrder = fromIntegral (w .&. 0xFFFF)
    highOrder = fromIntegral (shiftR w 32)

constantFloat :: Float -> Builder Id
constantFloat f = do
    float32 <- R.typeFloat 32
    R.constant float32 [floatToWord32 f]
  where
    floatToWord32 :: Float -> Word32
    floatToWord32 = unsafeCoerce

constantDouble :: Double -> Builder Id
constantDouble d = do
    float64 <- R.typeFloat 64
    R.constant float64 [lowOrder, highOrder]
  where
    doubleToWord64 :: Double -> Word64
    doubleToWord64 = unsafeCoerce
    d' = doubleToWord64 d
    lowOrder = fromIntegral (d' .&. 0xFFFF)
    highOrder = fromIntegral (shiftR d' 32)

-- Id: Return type (must be a composite type)
-- Ids: Constants for the constituents of the composite value
constantComposite :: TypeId -> [Id] -> Builder Id
constantComposite (TypeId i) = R.constantComposite i

-- Id: Return type (must be sampler type)
constantSampler :: TypeId -> SamplerAddressingMode -> SamplerParam
                -> SamplerFilterMode -> Builder Id
constantSampler (TypeId i) = R.constantSampler i

-- Id: Return type (must be pointer type)
constantNullPointer :: TypeId -> Builder Id
constantNullPointer (TypeId i) = R.constantNullPointer i

specConstantTrue :: Builder Id
specConstantTrue = do
    bool <- R.typeBool
    R.specConstantTrue bool

specConstantFalse :: Builder Id
specConstantFalse = do
    bool <- R.typeBool
    R.specConstantFalse bool

specConstantInt32 :: Int32 -> Builder Id
specConstantInt32 i = do
    int32 <- R.typeInt 32 Signed
    R.specConstant int32 [fromIntegral i]

specConstantInt64 :: Int64 -> Builder Id
specConstantInt64 i = do
    int64 <- R.typeInt 64 Signed
    R.specConstant int64 [lowOrder, highOrder]
  where
    i' = fromIntegral i :: Word64
    lowOrder = fromIntegral (i' .&. 0xFFFF)
    highOrder = fromIntegral (shiftR i' 32)

specConstantWord32 :: Word32 -> Builder Id
specConstantWord32 w = do
    uint32 <- R.typeInt 32 Unsigned
    R.specConstant uint32 [w]

specConstantWord64 :: Word64 -> Builder Id
specConstantWord64 w = do
    uint64 <- R.typeInt 64 Unsigned
    R.specConstant uint64 [lowOrder, highOrder]
  where
    lowOrder = fromIntegral (w .&. 0xFFFF)
    highOrder = fromIntegral (shiftR w 32)

specConstantFloat :: Float -> Builder Id
specConstantFloat f = do
    float32 <- R.typeFloat 32
    R.specConstant float32 [floatToWord32 f]
  where
    floatToWord32 :: Float -> Word32
    floatToWord32 = unsafeCoerce

specConstantDouble :: Double -> Builder Id
specConstantDouble d = do
    float64 <- R.typeFloat 64
    R.specConstant float64 [lowOrder, highOrder]
  where
    doubleToWord64 :: Double -> Word64
    doubleToWord64 = unsafeCoerce
    d' = doubleToWord64 d
    lowOrder = fromIntegral (d' .&. 0xFFFF)
    highOrder = fromIntegral (shiftR d' 32)

-- Id: Return type (must be a composite type)
-- Ids: Constants for the constituents of the composite value
specConstantComposite :: TypeId -> [Id] -> Builder Id
specConstantComposite (TypeId i) = R.specConstantComposite i

-- Variables always go through a pointer type, but everything except the
-- variable declaration works in terms of the type the pointer points to.
-- In addition, the pointer type declaration requires specifying the storage
-- of the pointer, which is used again in the variable declaration.
variable :: TypeId -> StorageClass -> Maybe Id -> Builder Id
variable (TypeId t) sc ini = do
    ptr_t <- R.typePointer sc t
    R.variable ptr_t sc ini

-- Id 1: Result type (must be pointer type)
-- Id 2: Number of objects to allocate
variableArray :: TypeId -> StorageClass -> Id -> Builder Id
variableArray (TypeId i) = R.variableArray i

-- Id 1: Result type
-- Id 2: Pointer to load through (must be pointer type)
load :: TypeId -> Id -> MemoryAccess -> Builder Id
load (TypeId i) = R.load i

-- Id 1: Result type
-- Id 2: Pointer to base object (must be pointer type)
-- Ids: Indices of structure to walk down to reach result value
-- The storage class should be the same as the storage class the base object was
-- declared as.
accessChain :: TypeId -> StorageClass -> Id -> [Id] -> Builder Id
accessChain (TypeId t) sc i is = do
    ptr_t <- R.typePointer sc t
    R.accessChain ptr_t i is

-- Id 1: Result type
-- Id 2: Pointer to base object (must be pointer type)
-- Ids: Indices of structure to walk down to reach result value
-- The storage class should be the same as the storage class the base object was
-- declared as.
inBoundsAccessChain :: TypeId -> StorageClass -> Id -> [Id] -> Builder Id
inBoundsAccessChain (TypeId t) sc i is = do
    ptr_t <- R.typePointer sc t
    R.inBoundsAccessChain ptr_t i is

-- Id 1: Result type
-- Id 2: Structure which has a member that is an array
-- Word32: TODO
arrayLength :: TypeId -> Id -> Word32 -> Builder Id
arrayLength (TypeId i) = R.arrayLength i

-- Id 1: Result type
-- Id 2: Pointer to variable of type sampler
-- Id 3: Which texel coordinate to use
-- Id 4: Which texel sample to use
imagePointer :: TypeId -> Id -> Id -> Id -> Builder Id
imagePointer (TypeId i) = R.imagePointer i

-- Id: A pointer that must point to Generic
-- There's supposed to be a result type as well, but it must be a 32-bit
-- integer, so we simply define it here behind the scenes
genericPtrMemSemantics :: Id -> Builder Id
genericPtrMemSemantics p = do
    uint32 <- R.typeInt 32 Unsigned
    R.genericPtrMemSemantics uint32 p

-- Id 1: Result type from calling this function
-- Id 2: The function type for this function (the return type must be the same
--       as Id 1)
function :: TypeId -> [FunctionControl] -> TypeId -> Builder Id
function (TypeId r) fcm (TypeId f) = R.function r fcm f

-- Entry point functions have to have void as the return type and take no
-- parameters. If we take the execution model as a parameter then we can remove
-- a lot of boilerplate.
entryPointFunction :: ExecutionModel -> Builder Id
entryPointFunction em = do
    void <- R.typeVoid
    fn_t <- R.typeFunction void []
    fn <- R.function void [] fn_t
    R.entryPoint em fn
    return fn

-- Id: Type of this function parameter
functionParameter :: TypeId -> Builder Id
functionParameter (TypeId i) = R.functionParameter i

-- Id 1: Result type from calling the given function
-- Id 2: The function to call
-- Ids: The function parameters
functionCall :: TypeId -> Id -> [Id] -> Builder Id
functionCall (TypeId i) = R.functionCall i

-- Id 1: Result type, must be a sampler type with both texture and filter
-- Id 2: Sampler with a texture and no filter
-- Id 3: Filter
sampler :: TypeId -> Id -> Id -> Builder Id
sampler (TypeId i) = R.sampler i

-- Id 1: Result type
-- Id 2: Sampler
-- Id 3: Texture coordinate, floating point scalar or vector
-- Maybe Id: Bias for implicit level of detail
textureSample :: TypeId -> Id -> Id -> Maybe Id -> Builder Id
textureSample (TypeId i) = R.textureSample i

-- Id 1: Result type
-- Id 2: Sampler, must be cube-arrayed depth-comparison type
-- Id 3: Texture coordinate, vector containing (u, v, w, array layer)
-- Id 4: Depth-comparison reference value
textureSampleDref :: TypeId -> Id -> Id -> Id -> Builder Id
textureSampleDref (TypeId i) = R.textureSampleDref i

-- Id 1: Result type
-- Id 2: Sampler
-- Id 3: Texture coordinate, floating point scalar or vector
-- Id 4: Level of detail to use when sampling
textureSampleLod :: TypeId -> Id -> Id -> Id -> Builder Id
textureSampleLod (TypeId i) = R.textureSampleLod i

-- Id 1: Result type
-- Id 2: Sampler
-- Id 3: Texture coordinate, floating point vector of 4 components
-- Maybe Id: Bias for implicit level of detail
textureSampleProj :: TypeId -> Id -> Id -> Maybe Id -> Builder Id
textureSampleProj (TypeId i) = R.textureSampleProj i

-- Id 1: Result type
-- Id 2: Sampler
-- Id 3: Texture coordinate, floating point scalar or vector
-- Id 4: dx, explicit derivative to use in calculating level of detail
-- Id 5: dy, explicit derivative to use in calculating level of detail
textureSampleGrad :: TypeId -> Id -> Id -> Id -> Id -> Builder Id
textureSampleGrad (TypeId i) = R.textureSampleGrad i

-- Id 1: Result type
-- Id 2: Sampler
-- Id 3: Texture coordinate, floating point scalar or vector
-- Id 4: Offset added before texel lookup
-- Maybe Id: Bias for implicit level of detail
textureSampleOffset :: TypeId -> Id -> Id -> Id -> Maybe Id -> Builder Id
textureSampleOffset (TypeId i) = R.textureSampleOffset i

-- Id 1: Result type
-- Id 2: Sampler
-- Id 3: Texture coordinate, floating point vector of 4 components
-- Id 4: Level of detail to use when sampling
textureSampleProjLod :: TypeId -> Id -> Id -> Id -> Builder Id
textureSampleProjLod (TypeId i) = R.textureSampleProjLod i

-- Id 1: Result type
-- Id 2: Sampler
-- Id 3: Texture coordinate, floating point vector of 4 components
-- Id 4: dx, explicit derivative to use in calculating level of detail
-- Id 5: dy, explicit derivative to use in calculating level of detail
textureSampleProjGrad :: TypeId -> Id -> Id -> Id -> Id -> Builder Id
textureSampleProjGrad (TypeId i) = R.textureSampleProjGrad i

-- Id 1: Result type
-- Id 2: Sampler
-- Id 3: Texture coordinate, floating point scalar or vector
-- Id 4: Level of detail to use when sampling
-- Id 5: Offset added before texel lookup
textureSampleLodOffset :: TypeId -> Id -> Id -> Id -> Id -> Builder Id
textureSampleLodOffset (TypeId i) = R.textureSampleLodOffset i

-- Id 1: Result type
-- Id 2: Sampler
-- Id 3: Texture coordinate, floating point vector of 4 components
-- Id 4: Offset added before texel lookup
-- Maybe Id: Bias for implicit level of detail
textureSampleProjOffset :: TypeId -> Id -> Id -> Id -> Maybe Id -> Builder Id
textureSampleProjOffset (TypeId i) = R.textureSampleProjOffset i

-- Id 1: Result type
-- Id 2: Sampler
-- Id 3: Texture coordinate, floating point scalar or vector
-- Id 4: dx, explicit derivative to use in calculating level of detail
-- Id 5: dy, explicit derivative to use in calculating level of detail
-- Id 6: Offset added before texel lookup
textureSampleGradOffset :: TypeId -> Id -> Id -> Id -> Id -> Id -> Builder Id
textureSampleGradOffset (TypeId i) = R.textureSampleGradOffset i

-- Id 1: Result type
-- Id 2: Sampler
-- Id 3: Texture coordinate, floating point vector of 4 components
-- Id 4: Level of detail to use when sampling
-- Id 5: Offset added before texel lookup
textureSampleProjLodOffset :: TypeId -> Id -> Id -> Id -> Id -> Builder Id
textureSampleProjLodOffset (TypeId i) = R.textureSampleProjLodOffset i

-- Id 1: Result type
-- Id 2: Sampler
-- Id 3: Texture coordinate, floating point vector of 4 components
-- Id 4: dx, explicit derivative to use in calculating level of detail
-- Id 5: dy, explicit derivative to use in calculating level of detail
-- Id 6: Offset added before texel lookup
textureSampleProjGradOffset :: TypeId -> Id -> Id -> Id -> Id -> Id
                            -> Builder Id
textureSampleProjGradOffset (TypeId i) = R.textureSampleProjGradOffset i

-- Id 1: Result type
-- Id 2: Sampler, cannot have dim of cube or buffer, no depth-comparison
-- Id 3: Texture coordinate, integer scalar or vector
-- Id 4: Level of detail to use when sampling
textureFetchTexelLod :: TypeId -> Id -> Id -> Id -> Builder Id
textureFetchTexelLod (TypeId i) = R.textureFetchTexelLod i

-- Id 1: Result type
-- Id 2: Sampler, cannot have dim of cube or buffer, no depth-comparison
-- Id 3: Texture coordinate, integer scalar or vector
-- Id 4: Offset added before texel lookup
textureFetchTexelOffset :: TypeId -> Id -> Id -> Id -> Builder Id
textureFetchTexelOffset (TypeId i) = R.textureFetchTexelOffset i

-- Id 1: Result type
-- Id 2: Sampler, must be multi-sample texture
-- Id 3: Texture coordinate, integer scalar or vector
-- Id 4: Sample number to return
textureFetchTexelSample :: TypeId -> Id -> Id -> Id -> Builder Id
textureFetchTexelSample (TypeId i) = R.textureFetchTexelSample i

-- Id 1: Result type
-- Id 2: Sampler, must have dim of buffer
-- Id 3: Scalar integer index into the buffer
textureFetchTexel :: TypeId -> Id -> Id -> Builder Id
textureFetchTexel (TypeId i) = R.textureFetchTexel i

-- Id 1: Result type
-- Id 2: Sampler, must have dim of 2D, rect, or cube
-- Id 3: Texture coordinate, floating point scalar or vector
-- Id 4: Component number that will be gathered from all 4 texels. Must be 0-3
textureGather :: TypeId -> Id -> Id -> Id -> Builder Id
textureGather (TypeId i) = R.textureGather i

-- Id 1: Result type
-- Id 2: Sampler, must have dim of 2D, rect, or cube
-- Id 3: Texture coordinate, floating point scalar or vector
-- Id 4: Component number that will be gathered from all 4 texels. Must be 0-3
-- Id 5: Offset added before texel lookup
textureGatherOffset :: TypeId -> Id -> Id -> Id -> Id -> Builder Id
textureGatherOffset (TypeId i) = R.textureGatherOffset i

-- Id 1: Result type
-- Id 2: Sampler, must have dim of 2D, rect, or cube
-- Id 3: Texture coordinate, floating point scalar or vector
-- Id 4: Component number that will be fathered from all 4 texels. Must be 0-3
-- Id 5: Offset added before texel lookup. Must be constant array of size 4 of
--       vectors of 2 integers
textureGatherOffsets :: TypeId -> Id -> Id -> Id -> Id -> Builder Id
textureGatherOffsets (TypeId i) = R.textureGatherOffsets i

-- Id 1: Result type, must have base type of integer
-- Id 2: Sampler, must have dim of 1D, 2D, 3D, or cube
-- Id 3: Level of detail, used to calculate which mipmap level to query
textureQuerySizeLod :: TypeId -> Id -> Id -> Builder Id
textureQuerySizeLod (TypeId i) = R.textureQuerySizeLod i

-- Id 1: Result type, must have base type of integer
-- Id 2: Sampler, must have type of rect, buffer, multisampled 2D
textureQuerySize :: TypeId -> Id -> Builder Id
textureQuerySize (TypeId i) = R.textureQuerySize i

-- Id 1: Result type, must be 2 component floating point vector
-- Id 2: Sampler, must have dim of 1D, 2D, 3D, or cube
-- Id 3: Texture coordinate, floating point scalar or vector
textureQueryLod :: TypeId -> Id -> Id -> Builder Id
textureQueryLod (TypeId i) = R.textureQueryLod i

-- Id 1: Result type, must be a scalar integer
-- Id 2: Sampler, must have dim of 1D, 2D, 3D, or cube
textureQueryLevels :: TypeId -> Id -> Builder Id
textureQueryLevels (TypeId i) = R.textureQueryLevels i

-- Id 1: Result type, must be a scalar integer
-- Id 2: Sampler, must have dim of 2D and be a multisample texture
textureQuerySamples :: TypeId -> Id -> Builder Id
textureQuerySamples (TypeId i) = R.textureQuerySamples i

-- Id 1: Result type, must be unsigned int
-- Id 2: Float value to convert
convertFToU :: TypeId -> Id -> Builder Id
convertFToU (TypeId i) = R.convertFToU i

-- Id 1: Result type, must be signed int
-- Id 2: Float value to convert
convertFToS :: TypeId -> Id -> Builder Id
convertFToS (TypeId i) = R.convertFToS i

-- Id 1: Result type, must be float value
-- Id 2: Signed value to convert
convertSToF :: TypeId -> Id -> Builder Id
convertSToF (TypeId i) = R.convertSToF i

-- Id 1: Result type, must be float value
-- Id 2: Unsigned value to convert
convertUToF :: TypeId -> Id -> Builder Id
convertUToF (TypeId i) = R.convertUToF i

-- Id 1: Result type, must be unsigned value
-- Id 2: Unsigned value to change width
uConvert :: TypeId -> Id -> Builder Id
uConvert (TypeId i) = R.uConvert i

-- Id 1: Result type, must be signed value
-- Id 2: Signed value to change width
sConvert :: TypeId -> Id -> Builder Id
sConvert (TypeId i) = R.sConvert i

-- Id 1: Result type, must be float value
-- Id 2: Float value to change width
fConvert :: TypeId -> Id -> Builder Id
fConvert (TypeId i) = R.fConvert i

-- Id 1: Result type, must be unsigned value
-- Id 2: Pointer value to cast to unsigned
convertPtrToU :: TypeId -> Id -> Builder Id
convertPtrToU (TypeId i) = R.convertPtrToU i

-- Id 1: Result type, must be pointer
-- Id 2: Unsigned value to cast to pointer
convertUToPtr :: TypeId -> Id -> Builder Id
convertUToPtr (TypeId i) = R.convertUToPtr i

-- Id 1: Result type, must be same type as source pointer
-- Id 2: Source pointer, must have storage class of generic
ptrCastToGeneric :: TypeId -> Id -> Builder Id
ptrCastToGeneric (TypeId i) = R.ptrCastToGeneric i

-- Id 1: Result type, must be same type as source pointer
-- Id 2: Source pointer, must point to WorkgroupLocal, WorkgroupGlobal, Private
genericCastToPtr :: TypeId -> Id -> Builder Id
genericCastToPtr (TypeId i) = R.genericCastToPtr i

-- Id 1: Result type, must have same width as operand
-- Id 2: Operand, must be numeric or pointer type
bitcast :: TypeId -> Id -> Builder Id
bitcast (TypeId i) = R.bitcast i

-- Id 1: Result type, must point to WorkgroupLocal, WorkgroupGlobal, Private
-- Id 2: Source pointer, must point to Generic
genericCastToPtrExplicit :: TypeId -> Id -> StorageClass -> Builder Id
genericCastToPtrExplicit (TypeId i) = R.genericCastToPtrExplicit i

-- Id 1: Result type, must be signed integer scalar or vector
-- Id 2: Signed integer value to convert
satConvertSToU :: TypeId -> Id -> Builder Id
satConvertSToU (TypeId i) = R.satConvertSToU i

-- Id 1: Result type, must be unsigned integer scalar or vector
-- Id 2: Unsigned integer value to convert
satConvertUToS :: TypeId -> Id -> Builder Id
satConvertUToS (TypeId i) = R.satConvertUToS i

-- Id 1: Result type, must be same type as given vector
-- Id 2: Vector to read from, must be vector type
-- Id 3: Index, must be scalar integer type
vectorExtractDynamic :: TypeId -> Id -> Id -> Builder Id
vectorExtractDynamic (TypeId i) = R.vectorExtractDynamic i

-- Id 1: Result type, must be same type as given vector
-- Id 2: Vector to insert into, must be vector type
-- Id 3: Component to write into given vector
-- Id 4: Index of vector to insert value
vectorInsertDynamic :: TypeId -> Id -> Id -> Id -> Builder Id
vectorInsertDynamic (TypeId i) = R.vectorInsertDynamic i

-- Id 1: Result type, must be same type as given vector
-- Id 2: First vector
-- Id 3: Second vector
-- Word32s: List of indices to use to construct new vector from the given
--          vectors concatenated together
vectorShuffle :: TypeId -> Id -> Id -> [Word32] -> Builder Id
vectorShuffle (TypeId i) = R.vectorShuffle i

-- Id 1: Result type, must be a composite type
-- Ids: Components of the composite to construct
compositeConstruct :: TypeId -> [Id] -> Builder Id
compositeConstruct (TypeId i) = R.compositeConstruct i

-- Id 1: Result type, must be the type of the component being extracted
-- Id 2: Composite to extract from
-- Word32s: Indexes to walk to reach the component to extract
compositeExtract :: TypeId -> Id -> [Word32] -> Builder Id
compositeExtract (TypeId i) = R.compositeExtract i

-- Id 1: Result type, must be the type of the component to insert
-- Id 2: Object to insert into the given composite
-- Id 3: Composite to insert into
-- Words32s: Indexes to walk to reach the component to insert
compositeInsert :: TypeId -> Id -> Id -> [Word32] -> Builder Id
compositeInsert (TypeId i) = R.compositeInsert i

-- Id 1: Result type, must be the same as the given object's type
-- Id 2: Object to copy
copyObject :: TypeId -> Id -> Builder Id
copyObject (TypeId i) = R.copyObject i

-- Id 1: Result type, must be the type of the given matrix transposed
-- Id 2: Matrix to transpose
transpose :: TypeId -> Id -> Builder Id
transpose (TypeId i) = R.transpose i

-- Id 1: Result type, must be signed integer
-- Id 2: Signed value to negate
sNegate :: TypeId -> Id -> Builder Id
sNegate (TypeId i) = R.sNegate i

-- Id 1: Result type, must be float
-- Id 2: Float value to negate
fNegate :: TypeId -> Id -> Builder Id
fNegate (TypeId i) = R.fNegate i

-- Id 1: Result type, must be integer type
-- Id 2: Operand to complement the bits of
bNot :: TypeId -> Id -> Builder Id
bNot (TypeId i) = R.bNot i

-- Id 1: Result type, must be integer type
-- Id 2: First operand
-- Id 3: Second operand
iAdd :: TypeId -> Id -> Id -> Builder Id
iAdd (TypeId i) = R.iAdd i

-- Id 1: Result type, must be float type
-- Id 2: First operand
-- Id 3: Second operand
fAdd :: TypeId -> Id -> Id -> Builder Id
fAdd (TypeId i) = R.fAdd i

-- Id 1: Result type, must be integer type
-- Id 2: First operand
-- Id 3: Second operand
iSub :: TypeId -> Id -> Id -> Builder Id
iSub (TypeId i) = R.iSub i

-- Id 1: Result type, must be float type
-- Id 2: First operand
-- Id 3: Second operand
fSub :: TypeId -> Id -> Id -> Builder Id
fSub (TypeId i) = R.fSub i

-- Id 1: Result type, must be integer type
-- Id 2: First operand
-- Id 3: Second operand
iMul :: TypeId -> Id -> Id -> Builder Id
iMul (TypeId i) = R.iMul i

-- Id 1: Result type, must be float type
-- Id 2: First operand
-- Id 3: Second operand
fMul :: TypeId -> Id -> Id -> Builder Id
fMul (TypeId i) = R.fMul i

-- Id 1: Result type, must be unsigned integer type
-- Id 2: First operand
-- Id 3: Second operand
uDiv :: TypeId -> Id -> Id -> Builder Id
uDiv (TypeId i) = R.uDiv i

-- Id 1: Result type, must be signed integer type
-- Id 2: First operand
-- Id 3: Second operand
sDiv :: TypeId -> Id -> Id -> Builder Id
sDiv (TypeId i) = R.sDiv i

-- Id 1: Result type, must be float type
-- Id 2: First operand
-- Id 3: Second operand
fDiv :: TypeId -> Id -> Id -> Builder Id
fDiv (TypeId i) = R.fDiv i

-- Id 1: Result type, must be unsigned integer type
-- Id 2: First operand
-- Id 3: Second operand
uMod :: TypeId -> Id -> Id -> Builder Id
uMod (TypeId i) = R.uMod i

-- Id 1: Result type, must be signed integer type
-- Id 2: First operand
-- Id 3: Second operand
sRem :: TypeId -> Id -> Id -> Builder Id
sRem (TypeId i) = R.sRem i

-- Id 1: Result type, must be signed integer type
-- Id 2: First operand
-- Id 3: Second operand
sMod :: TypeId -> Id -> Id -> Builder Id
sMod (TypeId i) = R.sMod i

-- Id 1: Result type, must be float type
-- Id 2: First operand
-- Id 3: Second operand
fRem :: TypeId -> Id -> Id -> Builder Id
fRem (TypeId i) = R.fRem i

-- Id 1: Result type, must be float type
-- Id 2: First operand
-- Id 3: Second operand
fMod :: TypeId -> Id -> Id -> Builder Id
fMod (TypeId i) = R.fMod i

-- Id 1: Result type, must be same vector type as first operand
-- Id 2: Vector, must be floating point vector
-- Id 3: Scalar, must be floating point value
vectorTimesScalar :: TypeId -> Id -> Id -> Builder Id
vectorTimesScalar (TypeId i) = R.vectorTimesScalar i

-- Id 1: Result type, must be same matrix type as first operand
-- Id 2: Matrix, must be floating point matrix
-- Id 3: Scalar, must be floating point value
matrixTimesScalar :: TypeId -> Id -> Id -> Builder Id
matrixTimesScalar (TypeId i) = R.matrixTimesScalar i

-- Id 1: Result type, must be vector type with same number of columns as matrix
-- Id 2: Vector, must be floating point vector
-- Id 3: Matrix, must be floating point matrix
vectorTimesMatrix :: TypeId -> Id -> Id -> Builder Id
vectorTimesMatrix (TypeId i) = R.vectorTimesMatrix i

-- Id 1: Result type, must be vector type with same number of rows as matrix
-- Id 2: Matrix, must be floating point matrix
-- Id 3: Vector, must be floating point vector
matrixTimesVector :: TypeId -> Id -> Id -> Builder Id
matrixTimesVector (TypeId i) = R.matrixTimesVector i

-- Id 1: Result type, must be matrix type
-- Id 2: Matrix, must be floating point matrix
-- Id 3: Matrix, must be floating point matrix
matrixTimesMatrix :: TypeId -> Id -> Id -> Builder Id
matrixTimesMatrix (TypeId i) = R.matrixTimesMatrix i

-- Id 1: Result type, must be matrix type
-- Id 2: Vector, must be floating point vector
-- Id 3: Vector, must be floating point vector
outerProduct :: TypeId -> Id -> Id -> Builder Id
outerProduct (TypeId i) = R.outerProduct i

-- Id 1: Result type, must be floating point type
-- Id 2: Vector, must be floating point vector
-- Id 3: Vector, must be floating point vector
dot :: TypeId -> Id -> Id -> Builder Id
dot (TypeId i) = R.dot i

-- Id 1: Result type, must be integer type
-- Id 2: First operand
-- Id 3: Second operand
shiftRightLogical :: TypeId -> Id -> Id -> Builder Id
shiftRightLogical (TypeId i) = R.shiftRightLogical i

-- Id 1: Result type, must be integer type
-- Id 2: First operand
-- Id 3: Second operand
shiftRightArithmetic :: TypeId -> Id -> Id -> Builder Id
shiftRightArithmetic (TypeId i) = R.shiftRightArithmetic i

-- Id 1: Result type, must be integer type
-- Id 2: First operand
-- Id 3: Second operand
shiftLeftLogical :: TypeId -> Id -> Id -> Builder Id
shiftLeftLogical (TypeId i) = R.shiftLeftLogical i

-- Id 1: Result type, must be integer type
-- Id 2: First operand
-- Id 3: Second operand
bitwiseOr :: TypeId -> Id -> Id -> Builder Id
bitwiseOr (TypeId i) = R.bitwiseOr i

-- Id 1: Result type, must be integer type
-- Id 2: First operand
-- Id 3: Second operand
bitwiseXor :: TypeId -> Id -> Id -> Builder Id
bitwiseXor (TypeId i) = R.bitwiseXor i

-- Id 1: Result type, must be integer type
-- Id 2: First operand
-- Id 3: Second operand
bitwiseAnd :: TypeId -> Id -> Id -> Builder Id
bitwiseAnd (TypeId i) = R.bitwiseAnd i

-- Id: Operand, must be boolean vector type
vAny :: Id -> Builder Id
vAny i = do
    bool <- R.typeBool
    R.vAny bool i

-- Id: Operand, must be boolean vector type
vAll :: Id -> Builder Id
vAll i = do
    bool <- R.typeBool
    R.vAll bool i

-- Id 1: Result type, must be boolean type
-- Id 2: Operand, must be floating point type
fIsNaN :: TypeId -> Id -> Builder Id
fIsNaN (TypeId i) = R.fIsNaN i

-- Id 1: Result type, must be boolean type
-- Id 2: Operand, must be floating point type
isInf :: TypeId -> Id -> Builder Id
isInf (TypeId i) = R.isInf i

-- Id 1: Result type, must be boolean type
-- Id 2: Operand, must be floating point type
isFinite :: TypeId -> Id -> Builder Id
isFinite (TypeId i) = R.isFinite i

-- Id 1: Result type, must be boolean type
-- Id 2: Operand, must be floating point type
isNormal :: TypeId -> Id -> Builder Id
isNormal (TypeId i) = R.isNormal i

-- Id 1: Result type, must be boolean type
-- Id 2: Operand, must be numeric type
signBitSet :: TypeId -> Id -> Builder Id
signBitSet (TypeId i) = R.signBitSet i

-- Id 1: Result type, must be boolean type
-- Id 2: First operand, must be numeric type
-- Id 3: Second operand, must be numeric type
lessOrGreater :: TypeId -> Id -> Id -> Builder Id
lessOrGreater (TypeId i) = R.lessOrGreater i

-- Id 1: Result type, must be boolean type
-- Id 2: First operand, must be numeric type
-- Id 3: Second operand, must be numeric type
ordered :: TypeId -> Id -> Id -> Builder Id
ordered (TypeId i) = R.ordered i

-- Id 1: Result type, must be boolean type
-- Id 2: First operand, must be numeric type
-- Id 3: Second operand, must be numeric type
unordered :: TypeId -> Id -> Id -> Builder Id
unordered (TypeId i) = R.unordered i

-- Id 1: Result type, must be boolean type
-- Id 2: First operand, must be numeric type
-- Id 3: Second operand, must be numeric type
logicalOr :: TypeId -> Id -> Id -> Builder Id
logicalOr (TypeId i) = R.logicalOr i

-- Id 1: Result type, must be boolean type
-- Id 2: First operand, must be numeric type
-- Id 3: Second operand, must be numeric type
logicalXor :: TypeId -> Id -> Id -> Builder Id
logicalXor (TypeId i) = R.logicalXor i

-- Id 1: Result type, must be boolean type
-- Id 2: First operand, must be numeric type
-- Id 3: Second operand, must be numeric type
logicalAnd :: TypeId -> Id -> Id -> Builder Id
logicalAnd (TypeId i) = R.logicalAnd i

-- Id 1: Result type, must be same type as both operands
-- Id 2: Condition to select upon, must be boolean type
-- Id 3: First operand
-- Id 4: Second operand
select :: TypeId -> Id -> Id -> Id -> Builder Id
select (TypeId i) = R.select i

-- Id 1: Result type, must be boolean type
-- Id 2: First operand
-- Id 3: Second operand
iEqual :: TypeId -> Id -> Id -> Builder Id
iEqual (TypeId i) = R.iEqual i

-- Id 1: Result type, must be boolean type
-- Id 2: First operand
-- Id 3: Second operand
fOrdEqual :: TypeId -> Id -> Id -> Builder Id
fOrdEqual (TypeId i) = R.fOrdEqual i

-- Id 1: Result type, must be boolean type
-- Id 2: First operand
-- Id 3: Second operand
fUnordEqual :: TypeId -> Id -> Id -> Builder Id
fUnordEqual (TypeId i) = R.fUnordEqual i

-- Id 1: Result type, must be boolean type
-- Id 2: First operand
-- Id 3: Second operand
iNotEqual :: TypeId -> Id -> Id -> Builder Id
iNotEqual (TypeId i) = R.iNotEqual i

-- Id 1: Result type, must be boolean type
-- Id 2: First operand
-- Id 3: Second operand
fOrdNotEqual :: TypeId -> Id -> Id -> Builder Id
fOrdNotEqual (TypeId i) = R.fOrdNotEqual i

-- Id 1: Result type, must be boolean type
-- Id 2: First operand
-- Id 3: Second operand
fUnordNotEqual :: TypeId -> Id -> Id -> Builder Id
fUnordNotEqual (TypeId i) = R.fUnordNotEqual i

-- Id 1: Result type, must be boolean type
-- Id 2: First operand
-- Id 3: Second operand
uLessThan :: TypeId -> Id -> Id -> Builder Id
uLessThan (TypeId i) = R.uLessThan i

-- Id 1: Result type, must be boolean type
-- Id 2: First operand
-- Id 3: Second operand
sLessThan :: TypeId -> Id -> Id -> Builder Id
sLessThan (TypeId i) = R.sLessThan i

-- Id 1: Result type, must be boolean type
-- Id 2: First operand
-- Id 3: Second operand
fOrdLessThan :: TypeId -> Id -> Id -> Builder Id
fOrdLessThan (TypeId i) = R.fOrdLessThan i

-- Id 1: Result type, must be boolean type
-- Id 2: First operand
-- Id 3: Second operand
fUnordLessThan :: TypeId -> Id -> Id -> Builder Id
fUnordLessThan (TypeId i) = R.fUnordLessThan i

-- Id 1: Result type, must be boolean type
-- Id 2: First operand
-- Id 3: Second operand
uGreaterThan :: TypeId -> Id -> Id -> Builder Id
uGreaterThan (TypeId i) = R.uGreaterThan i

-- Id 1: Result type, must be boolean type
-- Id 2: First operand
-- Id 3: Second operand
sGreaterThan :: TypeId -> Id -> Id -> Builder Id
sGreaterThan (TypeId i) = R.sGreaterThan i

-- Id 1: Result type, must be boolean type
-- Id 2: First operand
-- Id 3: Second operand
fOrdGreaterThan :: TypeId -> Id -> Id -> Builder Id
fOrdGreaterThan (TypeId i) = R.fOrdGreaterThan i

-- Id 1: Result type, must be boolean type
-- Id 2: First operand
-- Id 3: Second operand
fUnordGreaterThan :: TypeId -> Id -> Id -> Builder Id
fUnordGreaterThan (TypeId i) = R.fUnordGreaterThan i

-- Id 1: Result type, must be boolean type
-- Id 2: First operand
-- Id 3: Second operand
uLessThanEqual :: TypeId -> Id -> Id -> Builder Id
uLessThanEqual (TypeId i) = R.uLessThanEqual i

-- Id 1: Result type, must be boolean type
-- Id 2: First operand
-- Id 3: Second operand
sLessThanEqual :: TypeId -> Id -> Id -> Builder Id
sLessThanEqual (TypeId i) = R.sLessThanEqual i

-- Id 1: Result type, must be boolean type
-- Id 2: First operand
-- Id 3: Second operand
fOrdLessThanEqual :: TypeId -> Id -> Id -> Builder Id
fOrdLessThanEqual (TypeId i) = R.fOrdLessThanEqual i

-- Id 1: Result type, must be boolean type
-- Id 2: First operand
-- Id 3: Second operand
fUnordLessThanEqual :: TypeId -> Id -> Id -> Builder Id
fUnordLessThanEqual (TypeId i) = R.fUnordLessThanEqual i

-- Id 1: Result type, must be boolean type
-- Id 2: First operand
-- Id 3: Second operand
uGreaterThanEqual :: TypeId -> Id -> Id -> Builder Id
uGreaterThanEqual (TypeId i) = R.uGreaterThanEqual i

-- Id 1: Result type, must be boolean type
-- Id 2: First operand
-- Id 3: Second operand
sGreaterThanEqual :: TypeId -> Id -> Id -> Builder Id
sGreaterThanEqual (TypeId i) = R.sGreaterThanEqual i

-- Id 1: Result type, must be boolean type
-- Id 2: First operand
-- Id 3: Second operand
fOrdGreaterThanEqual :: TypeId -> Id -> Id -> Builder Id
fOrdGreaterThanEqual (TypeId i) = R.fOrdGreaterThanEqual i

-- Id 1: Result type, must be boolean type
-- Id 2: First operand
-- Id 3: Second operand
fUnordGreaterThanEqual :: TypeId -> Id -> Id -> Builder Id
fUnordGreaterThanEqual (TypeId i) = R.fUnordGreaterThanEqual i

-- Id 1: Result type, must be float type
-- Id 2: Operand
dPdx :: TypeId -> Id -> Builder Id
dPdx (TypeId i) = R.dPdx i

-- Id 1: Result type, must be float type
-- Id 2: Operand
dPdy :: TypeId -> Id -> Builder Id
dPdy (TypeId i) = R.dPdy i

-- Id 1: Result type, must be float type
-- Id 2: Operand
fWidth :: TypeId -> Id -> Builder Id
fWidth (TypeId i) = R.fWidth i

-- Id 1: Result type, must be float type
-- Id 2: Operand
dPdxFine :: TypeId -> Id -> Builder Id
dPdxFine (TypeId i) = R.dPdxFine i

-- Id 1: Result type, must be float type
-- Id 2: Operand
dPdyFine :: TypeId -> Id -> Builder Id
dPdyFine (TypeId i) = R.dPdyFine i

-- Id 1: Result type, must be float type
-- Id 2: Operand
fWidthFine :: TypeId -> Id -> Builder Id
fWidthFine (TypeId i) = R.fWidthFine i

-- Id 1: Result type, must be float type
-- Id 2: Operand
dPdxCoarse :: TypeId -> Id -> Builder Id
dPdxCoarse (TypeId i) = R.dPdxCoarse i

-- Id 1: Result type, must be float type
-- Id 2: Operand
dPdyCoarse :: TypeId -> Id -> Builder Id
dPdyCoarse (TypeId i) = R.dPdyCoarse i

-- Id 1: Result type, must be float type
-- Id 2: Operand
fWidthCoarse :: TypeId -> Id -> Builder Id
fWidthCoarse (TypeId i) = R.fWidthCoarse i

-- Id: Result type
-- Id 1s: Variable, must have same type as result
-- Id 2s: Parent block
phi :: TypeId -> [(Id, LabelId)] -> Builder Id
phi (TypeId i) ils = R.phi i (map (\(i',l) -> (i', toId l)) ils)

-- Id: Label of merge block
loopMerge :: LabelId -> [LoopControl] -> Builder ()
loopMerge (LabelId i) = R.loopMerge i

-- Id: Label of merge block
selectionMerge :: LabelId -> [SelectionControl] -> Builder ()
selectionMerge (LabelId i) = R.selectionMerge i

label :: Builder LabelId
label = fmap (LabelId) R.label

-- Id: Target label to branch to
branch :: LabelId -> Builder ()
branch (LabelId i) = R.branch i

-- Id 1: Conditional, must be a boolean type
-- Id 2: Label to take if the condition is true, must be in current function
-- Id 3: Label to take if the condition is false, must be in current function
-- Maybe (Word32, Word32): Branch weights
branchConditional :: Id -> LabelId -> LabelId -> Maybe (Word32, Word32)
                  -> Builder ()
branchConditional b (LabelId l1) (LabelId l2) = R.branchConditional b l1 l2

-- Id 1: Selector value, must be scalar integer type
-- Id 2: Default label
-- Word32s: Scalar integer literal
-- Ids: Corresponding label
switch :: Id -> LabelId -> [(Word32, LabelId)] -> Builder ()
switch i (LabelId l) wls = R.switch i l (map (\(w,l') -> (w, toId l')) wls)

-- Id 1: Result type
-- Id 2: Pointer to load
atomicLoad :: TypeId -> Id -> ExecutionScope -> [MemorySemantics] -> Builder Id
atomicLoad (TypeId i) = R.atomicLoad i

-- Id 1: Result type
-- Id 2: Pointer
-- Id 3: Value to exchange
atomicExchange :: TypeId -> Id -> ExecutionScope -> [MemorySemantics] -> Id
               -> Builder Id
atomicExchange (TypeId i) = R.atomicExchange i

-- Id 1: Result type
-- Id 2: Pointer
-- Id 3: Value
-- Id 4: Comparator
atomicCompareExchange :: TypeId -> Id -> ExecutionScope -> [MemorySemantics]
                      -> Id -> Id -> Builder Id
atomicCompareExchange (TypeId i) = R.atomicCompareExchange i

-- Id 1: Result type
-- Id 2: Pointer
-- Id 3: Value
-- Id 4: Comparator
atomicCompareExchangeWeak :: TypeId -> Id -> ExecutionScope -> [MemorySemantics]
                          -> Id -> Id -> Builder Id
atomicCompareExchangeWeak (TypeId i) = R.atomicCompareExchangeWeak i

-- Id 1: Result type
-- Id 2: Pointer, must point to scalar integer type
atomicIIncrement :: TypeId -> Id -> ExecutionScope -> [MemorySemantics]
                 -> Builder Id
atomicIIncrement (TypeId i) = R.atomicIIncrement i

-- Id 1: Result type
-- Id 2: Pointer, must point to scalar integer type
atomicIDecrement :: TypeId -> Id -> ExecutionScope -> [MemorySemantics]
                 -> Builder Id
atomicIDecrement (TypeId i) = R.atomicIDecrement i

-- Id 1: Result type
-- Id 2: Pointer, must point to scalar integer type
-- Id 3: Value
atomicIAdd :: TypeId -> Id -> ExecutionScope -> [MemorySemantics] -> Id
           -> Builder Id
atomicIAdd (TypeId i) = R.atomicIAdd i

-- Id 1: Result type
-- Id 2: Pointer, must point to scalar integer type
-- Id 3: Value
atomicISub :: TypeId -> Id -> ExecutionScope -> [MemorySemantics] -> Id
           -> Builder Id
atomicISub (TypeId i) = R.atomicISub i

-- Id 1: Result type
-- Id 2: Pointer, must point to scalar integer type
-- Id 3: Value
atomicUMin :: TypeId -> Id -> ExecutionScope -> [MemorySemantics] -> Id
           -> Builder Id
atomicUMin (TypeId i) = R.atomicUMin i

-- Id 1: Result type
-- Id 2: Pointer, must point to scalar integer type
-- Id 3: Value
atomicUMax :: TypeId -> Id -> ExecutionScope -> [MemorySemantics] -> Id
           -> Builder Id
atomicUMax (TypeId i) = R.atomicUMax i

-- Id 1: Result type
-- Id 2: Pointer, must point to scalar integer type
-- Id 3: Value
atomicAnd :: TypeId -> Id -> ExecutionScope -> [MemorySemantics] -> Id
          -> Builder Id
atomicAnd (TypeId i) = R.atomicAnd i

-- Id 1: Result type
-- Id 2: Pointer, must point to scalar integer type
-- Id 3: Value
atomicOr :: TypeId -> Id -> ExecutionScope -> [MemorySemantics] -> Id
         -> Builder Id
atomicOr (TypeId i) = R.atomicOr i

-- Id 1: Result type
-- Id 2: Pointer, must point to scalar integer type
-- Id 3: Value
atomicXor :: TypeId -> Id -> ExecutionScope -> [MemorySemantics] -> Id
          -> Builder Id
atomicXor (TypeId i) = R.atomicXor i

-- Id 1: Result type
-- Id 2: Pointer, must point to scalar integer type
-- Id 3: Value
atomicIMin :: TypeId -> Id -> ExecutionScope -> [MemorySemantics] -> Id
           -> Builder Id
atomicIMin (TypeId i) = R.atomicIMin i

-- Id 1: Result type
-- Id 2: Pointer, must point to scalar integer type
-- Id 3: Value
atomicIMax :: TypeId -> Id -> ExecutionScope -> [MemorySemantics] -> Id
           -> Builder Id
atomicIMax (TypeId i) = R.atomicIMax i

-- Id 1: Result type
-- Id 2: Destination pointer
-- Id 3: Source pointer
-- Id 4: Number of elements, must be 32/64 bit integer depending on addressing
--       model
-- Id 5: Stride, must be 32/64 bit integer depending on addressing model
-- Id 6: Event
asyncGroupCopy :: TypeId -> ExecutionScope -> Id -> Id -> Id -> Id -> Id
               -> Builder Id
asyncGroupCopy (TypeId i) = R.asyncGroupCopy i

-- Id 1: Result type
-- Id 2: Number of events, must be 32 bit integer type
-- Id 3: Events list, must be a pointer to an event type
waitGroupEvents :: TypeId -> ExecutionScope -> Id -> Id -> Builder Id
waitGroupEvents (TypeId i) = R.waitGroupEvents i

-- Id: Predicate, must be scalar boolean type
groupAll :: ExecutionScope -> Id -> Builder Id
groupAll es p = do
    bool <- R.typeBool
    R.groupAll bool es p

-- Id: Predicate, must be scalar boolean type
groupAny :: ExecutionScope -> Id -> Builder Id
groupAny es p = do
    bool <- R.typeBool
    R.groupAny bool es p

-- Id 1: Result type
-- Id 2: Value
-- Id 3: LocalId, must be integer type. Can be scalar, vec2, or vec3
-- Value and Result type must be one of:
--   32/64 bit integer, 16/32/64 bit float
groupBroadcast :: TypeId -> ExecutionScope -> Id -> Id -> Builder Id
groupBroadcast (TypeId i) = R.groupBroadcast i

-- Id 1: Result type
-- Id 2: Value to add to group
groupIAdd :: TypeId -> ExecutionScope -> GroupOperation -> Id -> Builder Id
groupIAdd (TypeId i) = R.groupIAdd i

-- Id 1: Result type
-- Id 2: Value to add to group
groupFAdd :: TypeId -> ExecutionScope -> GroupOperation -> Id -> Builder Id
groupFAdd (TypeId i) = R.groupFAdd i

-- Id 1: Result type
-- Id 2: Value to min to group
groupFMin :: TypeId -> ExecutionScope -> GroupOperation -> Id -> Builder Id
groupFMin (TypeId i) = R.groupFMin i

-- Id 1: Result type
-- Id 2: Value to min to group
groupUMin :: TypeId -> ExecutionScope -> GroupOperation -> Id -> Builder Id
groupUMin (TypeId i) = R.groupUMin i

-- Id 1: Result type
-- Id 2: Value to min to group
groupSMin :: TypeId -> ExecutionScope -> GroupOperation -> Id -> Builder Id
groupSMin (TypeId i) = R.groupSMin i

-- Id 1: Result type
-- Id 2: Value to max to group
groupFMax :: TypeId -> ExecutionScope -> GroupOperation -> Id -> Builder Id
groupFMax (TypeId i) = R.groupFMax i

-- Id 1: Result type
-- Id 2: Value to max to group
groupUMax :: TypeId -> ExecutionScope -> GroupOperation -> Id -> Builder Id
groupUMax (TypeId i) = R.groupUMax i

-- Id 1: Result type
-- Id 2: Value to max to group
groupSMax :: TypeId -> ExecutionScope -> GroupOperation -> Id -> Builder Id
groupSMax (TypeId i) = R.groupSMax i

-- Id 1: Result type
-- Id 2: q (see docs)
-- Id 3: Number of events, must be 32 bit integer
-- Id 4: Wait events, must be pointer to device events
-- Id 5: Retained events, must be pointer to device events
enqueueMarker :: TypeId -> Id -> Id -> Id -> Id -> Builder Id
enqueueMarker (TypeId i) = R.enqueueMarker i

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
enqueueKernel :: TypeId -> Id -> KernelEnqueueFlags -> NDRangeId -> Id -> Id
              -> Id -> Id -> Id -> Id -> Id -> [Id] -> Builder Id
enqueueKernel (TypeId i) q kef (NDRangeId n) = R.enqueueKernel i q kef n

-- Id 1: Result type, must be 32 bit integer
-- Id 2: ND range, must be struct built with buildNDrange
-- Id 3: Invoke, must be function type (see docs)
getKernelNDrangeSubGroupCount :: TypeId -> NDRangeId -> Id -> Builder Id
getKernelNDrangeSubGroupCount (TypeId i) (NDRangeId n) =
    R.getKernelNDrangeSubGroupCount i n

-- Id 1: Result type, must be 32 bit integer
-- Id 2: ND range, must be struct built with buildNDrange
-- Id 3: Invoke, must be function type (see docs)
getKernelNDrangeMaxSubGroupCount :: TypeId -> NDRangeId -> Id -> Builder Id
getKernelNDrangeMaxSubGroupCount (TypeId i) (NDRangeId n) =
    R.getKernelNDrangeMaxSubGroupCount i n

-- Id 1: Result type, must be 32 bit integer
-- Id 2: Invoke, must be function type (see docs)
getKernelWorkGroupSize :: TypeId -> Id -> Builder Id
getKernelWorkGroupSize (TypeId i) = R.getKernelWorkGroupSize i

-- Id 1: Result type, must be 32 bit integer
-- Id 2: Invoke, must be function type (see docs)
getKernelPreferredWorkGroupSizeMultiple :: TypeId -> Id -> Builder Id
getKernelPreferredWorkGroupSizeMultiple (TypeId i) =
    R.getKernelPreferredWorkGroupSizeMultiple i

createUserEvent :: Builder Id
createUserEvent = do
    event <- R.typeDeviceEvent
    R.createUserEvent event

-- Id: Event, must be device event type
isValidEvent :: Id -> Builder Id
isValidEvent e = do
    bool <- R.typeBool
    R.isValidEvent bool e

getDefaultQueue :: Builder Id
getDefaultQueue = do
    queue <- R.typeQueue
    R.getDefaultQueue queue

-- Id 1: Global work size
-- Id 2: Local work size
-- Id 3: Global work offset
-- All Ids must be 32-bit integer scalars or arrays with 2-3 items
-- Addressing mode must be Physical32 to use this instruction
buildNDRange32 :: Id -> Id -> Id -> Builder NDRangeId
buildNDRange32 gws lws gwo = do
    uint32 <- R.typeInt 32 Unsigned
    i_3 <- R.constant uint32 [3]
    arr <- R.typeArray uint32 i_3
    struct <- R.typeStruct [uint32, arr, arr, arr]
    r <- R.buildNDRange struct gws lws gwo
    return (NDRangeId r)

-- Id 1: Global work size
-- Id 2: Local work size
-- Id 3: Global work offset
-- All Ids must be 64-bit integer scalars or arrays with 2-3 items
-- Addressing mode must be Physical64 to use this instruction
buildNDRange64 :: Id -> Id -> Id -> Builder NDRangeId
buildNDRange64 gws lws gwo = do
    uint32 <- R.typeInt 32 Unsigned
    uint64 <- R.typeInt 64 Unsigned
    i_3 <- R.constant uint32 [3]
    arr <- R.typeArray uint64 i_3
    struct <- R.typeStruct [uint32, arr, arr, arr]
    r <- R.buildNDRange struct gws lws gwo
    return (NDRangeId r)

-- Id 1: Result type
-- Id 2: Pipe, must be pipe type with ReadOnly access qualifier
-- Id 3: Pointer, must have generic storage class
readPipe :: TypeId -> Id -> Id -> Builder Id
readPipe (TypeId i) = R.readPipe i

-- Id 1: Result type, must be 32 bit integer
-- Id 2: Pipe, must be pipe type with WriteOnly access qualifier
-- Id 3: Pointer, must have generic storage class
writePipe :: TypeId -> Id -> Id -> Builder Id
writePipe (TypeId i) = R.writePipe i

-- Id 1: Result type, must be 32 bit integer
-- Id 2: Pipe, must be pipe type with ReadOnly access qualifier
-- Id 3: Reserve ID, must have type reserve ID
-- Id 4: Index, must be 32 bit integer
-- Id 5: Pointer, must have generic storage class
reservedReadPipe :: TypeId -> Id -> ReserveId -> Id -> Id -> Builder Id
reservedReadPipe (TypeId i) p (ReserveId r) = R.reservedReadPipe i p r

-- Id 1: Result type, must be 32 bit integer
-- Id 2: Pipe, must be pipe type with WriteOnly accesss qualifier
-- Id 3: Reserve ID, must have type reserve ID
-- Id 4: Index, must be 32 bit integer
-- Id 5: Pointer, must have generic storage class
reservedWritePipe :: TypeId -> Id -> ReserveId -> Id -> Id -> Builder Id
reservedWritePipe (TypeId i) p (ReserveId r) = R.reservedWritePipe i p r

-- Id 1: Result type
-- Id 2: Pipe
-- Id 3: Number of packets
reserveReadPipePackets :: Id -> Id -> Builder ReserveId
reserveReadPipePackets p n = do
    reserveId <- R.typeReserveId
    r <- R.reserveReadPipePackets reserveId p n
    return (ReserveId r)

-- Id 1: Result type
-- Id 2: Pipe
-- Id 3: Number of packets
reserveWritePipePackets :: Id -> Id -> Builder ReserveId
reserveWritePipePackets p n = do
    reserveId <- R.typeReserveId
    r <- R.reserveWritePipePackets reserveId p n
    return (ReserveId r)

-- Id 1: Pipe with ReadOnly access qualifier
-- Id 2: Reserve ID
commitReadPipe :: Id -> ReserveId -> Builder ()
commitReadPipe i (ReserveId r) = R.commitReadPipe i r

-- Id 1: Pipe with WriteOnly access qualifier
-- Id 2: Reserve ID
commitWritePipe :: Id -> ReserveId -> Builder ()
commitWritePipe i (ReserveId r) = R.commitWritePipe i r

-- Id: Reserve ID
isValidReserveId :: ReserveId -> Builder Id
isValidReserveId (ReserveId i) = do
    bool <- R.typeBool
    R.isValidReserveId bool i

-- Id 1: Result type, must be 32 bit integer
-- Id 2: Pipe with either ReadOnly or WriteOnly access qualifier
getNumPipePackets :: TypeId -> Id -> Builder Id
getNumPipePackets (TypeId i) = R.getNumPipePackets i

-- Id 1: Result type, must be 32 bit integer
-- Id 2: Pipe with either ReadOnly or WriteOnly access qualifier
getMaxPipePackets :: TypeId -> Id -> Builder Id
getMaxPipePackets (TypeId i) = R.getMaxPipePackets i

-- Id 1: Pipe with ReadOnly access qualifier
-- Id 2: Number of packets, must be 32 bit integer
groupReserveReadPipePackets :: ExecutionScope -> Id -> Id -> Builder ReserveId
groupReserveReadPipePackets es p n = do
    reserveId <- R.typeReserveId
    r <- R.groupReserveReadPipePackets reserveId es p n
    return (ReserveId r)

-- Id 1: Pipe with ReadOnly access qualifier
-- Id 2: Number of packets, must be 32 bit integer
groupReserveWritePipePackets :: ExecutionScope -> Id -> Id -> Builder ReserveId
groupReserveWritePipePackets es p n = do
    reserveId <- R.typeReserveId
    r <- R.groupReserveReadPipePackets reserveId es p n
    return (ReserveId r)

-- Id 1: Pipe with ReadOnly access qualifier
-- Id 2: Reserve ID
groupCommitReadPipe :: ExecutionScope -> Id -> ReserveId -> Builder ()
groupCommitReadPipe es i (ReserveId r) = R.groupCommitReadPipe es i r

-- Id 1: Pipe with ReadOnly access qualifier
-- Id 2: Reserve ID
groupCommitWritePipe :: ExecutionScope -> Id -> ReserveId -> Builder ()
groupCommitWritePipe es i (ReserveId r) = R.groupCommitWritePipe es i r
