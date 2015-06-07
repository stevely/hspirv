{-
 - Instructions.hs
 - By Steven Smith
 -}

module SpirV.Instructions where

import Data.Word
import Data.ByteString (ByteString)

import qualified Data.ByteString as BS

newtype Id = Id { getId :: Word32 }
    deriving (Eq, Ord)

data Instruction = 
      OpNop
    | OpUndef Id Id
    | OpSource Word32 Word32 -- Source language, version number
    | OpSourceExtension ByteString -- Extension
    | OpName Id ByteString
    | OpMemberName Id Word32 ByteString
    | OpString Id ByteString
    | OpLine Id Id Word32 Word32 -- Row, column
    | OpDecorationGroup Id
    | OpDecorate Id Decoration
    | OpMemberDecorate Id Word32 Decoration
    | OpGroupDecorate Id [Id]
    | OpGroupMemberDecorate Id [Id]
    | OpExtension ByteString
    | OpExtInstImport Id ByteString
    | OpExtInst Id Id Id Word32 [Id]
    | OpMemoryModel AddressingModel MemoryModel
    | OpEntryPoint ExecutionModel Id
    | OpExecutionMode Id ExecutionMode
    | OpCompileFlag ByteString
    | OpTypeVoid Id
    | OpTypeBool Id
    | OpTypeInt Id Word32 Signedness -- Width
    | OpTypeFloat Id Word32 -- Width
    | OpTypeVector Id Id Word32 -- Component count
    | OpTypeMatrix Id Id Word32 -- Column type, count
    | OpTypeSampler Id Id Dim SamplerContent ArrayedContent DepthComparison
        MultiSampled (Maybe AccessQualifier) -- Might deviate from spec?
    | OpTypeFilter Id
    | OpTypeArray Id Id Id -- Length (3) must come from const instruction
    | OpTypeRuntimeArray Id Id
    | OpTypeStruct Id [Id]
    | OpTypeOpaque Id ByteString
    | OpTypePointer Id StorageClass Id
    | OpTypeFunction Id Id [Id]
    | OpTypeEvent Id
    | OpTypeDeviceEvent Id
    | OpTypeReserveId Id
    | OpTypeQueue Id
    | OpTypePipe Id Id AccessQualifier
    | OpConstantTrue Id Id
    | OpConstantFalse Id Id
    | OpConstant Id Id [Word32]
    | OpConstantComposite Id Id [Id]
    | OpConstantSampler Id Id SamplerAddressingMode SamplerParam SamplerFilterMode
    | OpConstantNullPointer Id Id
    | OpConstantNullObject Id Id
    | OpSpecConstantTrue Id Id
    | OpSpecConstantFalse Id Id
    | OpSpecConstant Id Id [Word32]
    | OpSpecConstantComposite Id Id [Id]
    | OpVariable Id Id StorageClass (Maybe Id) -- Initializer must be from constant
    | OpVariableArray Id Id StorageClass Id
    | OpLoad Id Id Id MemoryAccess
    | OpStore Id Id MemoryAccess
    | OpCopyMemory Id Id MemoryAccess
    | OpCopyMemorySized Id Id Id MemoryAccess
    | OpAccessChain Id Id Id [Id]
    | OpInBoundsAccessChain Id Id Id [Id]
    | OpArrayLength Id Id Id Word32
    | OpImagePointer Id Id Id Id Id
    | OpGenericPtrMemSemantics Id Id Id
    | OpFunction Id Id [FunctionControl] Id
    | OpFunctionParameter Id Id
    | OpFunctionEnd
    | OpFunctionCall Id Id Id [Id]
    | OpSampler Id Id Id Id
    | OpTextureSample Id Id Id Id (Maybe Id)
    | OpTextureSampleDref Id Id Id Id Id
    | OpTextureSampleLod Id Id Id Id Id
    | OpTextureSampleProj Id Id Id Id (Maybe Id)
    | OpTextureSampleGrad Id Id Id Id Id Id
    | OpTextureSampleOffset Id Id Id Id Id (Maybe Id)
    | OpTextureSampleProjLod Id Id Id Id Id
    | OpTextureSampleProjGrad Id Id Id Id Id Id
    | OpTextureSampleLodOffset Id Id Id Id Id Id
    | OpTextureSampleProjOffset Id Id Id Id Id (Maybe Id)
    | OpTextureSampleGradOffset Id Id Id Id Id Id Id
    | OpTextureSampleProjLodOffset Id Id Id Id Id Id
    | OpTextureSampleProjGradOffset Id Id Id Id Id Id Id
    | OpTextureFetchTexelLod Id Id Id Id Id
    | OpTextureFetchTexelOffset Id Id Id Id Id
    | OpTextureFetchTexelSample Id Id Id Id Id
    | OpTextureFetchTexel Id Id Id Id
    | OpTextureGather Id Id Id Id Id
    | OpTextureGatherOffset Id Id Id Id Id Id
    | OpTextureGatherOffsets Id Id Id Id Id Id
    | OpTextureQuerySizeLod Id Id Id Id
    | OpTextureQuerySize Id Id Id
    | OpTextureQueryLod Id Id Id Id
    | OpTextureQueryLevels Id Id Id
    | OpTextureQuerySamples Id Id Id
    | OpConvertFToU Id Id Id
    | OpConvertFToS Id Id Id
    | OpConvertSToF Id Id Id
    | OpConvertUToF Id Id Id
    | OpUConvert Id Id Id
    | OpSConvert Id Id Id
    | OpFConvert Id Id Id
    | OpConvertPtrToU Id Id Id
    | OpConvertUToPtr Id Id Id
    | OpPtrCastToGeneric Id Id Id
    | OpGenericCastToPtr Id Id Id
    | OpBitcast Id Id Id
    | OpGenericCastToPtrExplicit Id Id Id StorageClass
    | OpSatConvertSToU Id Id Id
    | OpSatConvertUToS Id Id Id
    | OpVectorExtractDynamic Id Id Id Id
    | OpVectorInsertDynamic Id Id Id Id Id
    | OpVectorShuffle Id Id Id Id [Word32]
    | OpCompositeConstruct Id Id [Id]
    | OpCompositeExtract Id Id Id [Word32]
    | OpCompositeInsert Id Id Id Id [Word32]
    | OpCopyObject Id Id Id
    | OpTranspose Id Id Id
    | OpSNegate Id Id Id
    | OpFNegate Id Id Id
    | OpNot Id Id Id
    | OpIAdd Id Id Id Id
    | OpFAdd Id Id Id Id
    | OpISub Id Id Id Id
    | OpFSub Id Id Id Id
    | OpIMul Id Id Id Id
    | OpFMul Id Id Id Id
    | OpUDiv Id Id Id Id
    | OpSDiv Id Id Id Id
    | OpFDiv Id Id Id Id
    | OpUMod Id Id Id Id
    | OpSRem Id Id Id Id
    | OpSMod Id Id Id Id
    | OpFRem Id Id Id Id
    | OpFMod Id Id Id Id
    | OpVectorTimesScalar Id Id Id Id
    | OpMatrixTimesScalar Id Id Id Id
    | OpVectorTimesMatrix Id Id Id Id
    | OpMatrixTimesVector Id Id Id Id
    | OpMatrixTimesMatrix Id Id Id Id
    | OpOuterProduct Id Id Id Id
    | OpDot Id Id Id Id
    | OpShiftRightLogical Id Id Id Id
    | OpShiftRightArithmetic Id Id Id Id
    | OpShiftLeftLogical Id Id Id Id
    | OpBitwiseOr Id Id Id Id
    | OpBitwiseXor Id Id Id Id
    | OpBitwiseAnd Id Id Id Id
    | OpAny Id Id Id
    | OpAll Id Id Id
    | OpIsNaN Id Id Id
    | OpIsInf Id Id Id
    | OpIsFinite Id Id Id
    | OpIsNormal Id Id Id
    | OpSignBitSet Id Id Id
    | OpLessOrGreater Id Id Id Id
    | OpOrdered Id Id Id Id
    | OpUnordered Id Id Id Id
    | OpLogicalOr Id Id Id Id
    | OpLogicalXor Id Id Id Id
    | OpLogicalAnd Id Id Id Id
    | OpSelect Id Id Id Id Id
    | OpIEqual Id Id Id Id
    | OpFOrdEqual Id Id Id Id
    | OpFUnordEqual Id Id Id Id
    | OpINotEqual Id Id Id Id
    | OpFOrdNotEqual Id Id Id Id
    | OpFUnordNotEqual Id Id Id Id
    | OpULessThan Id Id Id Id
    | OpSLessThan Id Id Id Id
    | OpFOrdLessThan Id Id Id Id
    | OpFUnordLessThan Id Id Id Id
    | OpUGreaterThan Id Id Id Id
    | OpSGreaterThan Id Id Id Id
    | OpFOrdGreaterThan Id Id Id Id
    | OpFUnordGreaterThan Id Id Id Id
    | OpULessThanEqual Id Id Id Id
    | OpSLessThanEqual Id Id Id Id
    | OpFOrdLessThanEqual Id Id Id Id
    | OpFUnordLessThanEqual Id Id Id Id
    | OpUGreaterThanEqual Id Id Id Id
    | OpSGreaterThanEqual Id Id Id Id
    | OpFOrdGreaterThanEqual Id Id Id Id
    | OpFUnordGreaterThanEqual Id Id Id Id
    | OpDPdx Id Id Id
    | OpDPdy Id Id Id
    | OpFwidth Id Id Id
    | OpDPdxFine Id Id Id
    | OpDPdyFine Id Id Id
    | OpFwidthFine Id Id Id
    | OpDPdxCoarse Id Id Id
    | OpDPdyCoarse Id Id Id
    | OpFwidthCoarse Id Id Id
    | OpPhi Id Id [(Id, Id)]
    | OpLoopMerge Id [LoopControl]
    | OpSelectionMerge Id [SelectionControl]
    | OpLabel Id
    | OpBranch Id
    | OpBranchConditional Id Id Id (Maybe (Word32, Word32))
    | OpSwitch Id Id [(Word32, Id)]
    | OpKill
    | OpReturn
    | OpReturnValue Id
    | OpUnreachable
    | OpLifetimeStart Id Word32
    | OpLifetimeStop Id Word32
    | OpAtomicInit Id Id
    | OpAtomicLoad Id Id Id ExecutionScope [MemorySemantics]
    | OpAtomicStore Id ExecutionScope [MemorySemantics] Id
    | OpAtomicExchange Id Id Id ExecutionScope [MemorySemantics] Id
    | OpAtomicCompareExchange Id Id Id ExecutionScope [MemorySemantics] Id Id
    | OpAtomicCompareExchangeWeak Id Id Id ExecutionScope [MemorySemantics] Id Id
    | OpAtomicIIncrement Id Id Id ExecutionScope [MemorySemantics]
    | OpAtomicIDecrement Id Id Id ExecutionScope [MemorySemantics]
    | OpAtomicIAdd Id Id Id ExecutionScope [MemorySemantics] Id
    | OpAtomicISub Id Id Id ExecutionScope [MemorySemantics] Id
    | OpAtomicUMin Id Id Id ExecutionScope [MemorySemantics] Id
    | OpAtomicUMax Id Id Id ExecutionScope [MemorySemantics] Id
    | OpAtomicAnd Id Id Id ExecutionScope [MemorySemantics] Id
    | OpAtomicOr Id Id Id ExecutionScope [MemorySemantics] Id
    | OpAtomicXor Id Id Id ExecutionScope [MemorySemantics] Id
    | OpAtomicIMin Id Id Id ExecutionScope [MemorySemantics] Id
    | OpAtomicIMax Id Id Id ExecutionScope [MemorySemantics] Id
    | OpEmitVertex
    | OpEndPrimitive
    | OpEmitStreamVertex Id -- must be constant scalar int
    | OpEndStreamPrimitive Id -- must be constant scalar int
    | OpControlBarrier ExecutionScope
    | OpMemoryBarrier ExecutionScope [MemorySemantics]
    | OpAsyncGroupCopy Id Id ExecutionScope Id Id Id Id Id
    | OpWaitGroupEvents Id Id ExecutionScope Id Id
    | OpGroupAll Id Id ExecutionScope Id
    | OpGroupAny Id Id ExecutionScope Id
    | OpGroupBroadcast Id Id ExecutionScope Id Id
    | OpGroupIAdd Id Id ExecutionScope GroupOperation Id
    | OpGroupFAdd Id Id ExecutionScope GroupOperation Id
    | OpGroupFMin Id Id ExecutionScope GroupOperation Id
    | OpGroupUMin Id Id ExecutionScope GroupOperation Id
    | OpGroupSMin Id Id ExecutionScope GroupOperation Id
    | OpGroupFMax Id Id ExecutionScope GroupOperation Id
    | OpGroupUMax Id Id ExecutionScope GroupOperation Id
    | OpGroupSMax Id Id ExecutionScope GroupOperation Id
    | OpEnqueueMarker Id Id Id Id Id Id
    | OpEnqueueKernel Id Id Id KernelEnqueueFlags Id Id Id Id Id Id Id Id [Id]
    | OpGetKernelNDrangeSubGroupCount Id Id Id Id
    | OpGetKernelNDrangeMaxSubGroupCount Id Id Id Id
    | OpGetKernelWorkGroupSize Id Id Id
    | OpGetKernelPreferredWorkGroupSizeMultiple Id Id Id
    | OpRetainEvent Id
    | OpReleaseEvent Id
    | OpCreateUserEvent Id Id
    | OpIsValidEvent Id Id Id
    | OpSetUserEventStatus Id Id
    | OpCaptureEventProfilingInfo Id [KernelProfilingInfo] Id
    | OpGetDefaultQueue Id Id
    | OpBuildNDRange Id Id Id Id Id
    | OpReadPipe Id Id Id Id
    | OpWritePipe Id Id Id Id
    | OpReservedReadPipe Id Id Id Id Id Id
    | OpReservedWritePipe Id Id Id Id Id Id
    | OpReserveReadPipePackets Id Id Id Id
    | OpReserveWritePipePackets Id Id Id Id
    | OpCommitReadPipe Id Id
    | OpCommitWritePipe Id Id
    | OpIsValidReserveId Id Id Id
    | OpGetNumPipePackets Id Id Id
    | OpGetMaxPipePackets Id Id Id
    | OpGroupReservedReadPipePackets Id Id ExecutionScope Id Id
    | OpGroupReservedWritePipePackets Id Id ExecutionScope Id Id
    | OpGroupCommitReadPipe ExecutionScope Id Id
    | OpGroupCommitWritePipe ExecutionScope Id Id

-- Used by OpTypeInt
data Signedness = Signed | Unsigned
    deriving (Show, Eq, Ord, Enum)

-- Used by OpTypeSampler
data SamplerContent = TextureNoFilter
                    | Image
                    | TextureAndFilter
    deriving (Show, Eq, Ord, Enum)

-- Used by OpTypeSampler
data ArrayedContent = NonArrayedContent | ArrayedContent
    deriving (Show, Eq, Ord, Enum)

-- Used by OpTypeSampler
data DepthComparison = NoDepthComparison | DepthComparison
    deriving (Show, Eq, Ord, Enum)

-- Used by OpTypeSampler
data MultiSampled = SingleSampled | MultiSampled
    deriving (Show, Eq, Ord, Enum)

-- Used by OpConstantSampler
data SamplerParam = Nonparametric
                  | Parametric
    deriving (Show, Eq, Ord, Enum)

data ExecutionModel = Vertex
                    | TessellationControl
                    | TessellationEvaluation
                    | Geometry
                    | Fragment
                    | GLCompute
                    | Kernel
    deriving (Show, Eq, Ord, Enum)

data AddressingModel = Logical
                     | Physical32
                     | Physical64
    deriving (Show, Eq, Ord, Enum)

data MemoryModel = Simple
                 | GLSL450
                 | OpenCL1'2
                 | OpenCL2'0
                 | OpenCL2'1
    deriving (Show, Eq, Ord, Enum)

data ExecutionMode = Invocations Word32
                   | SpacingEqual
                   | SpacingFractionalEqual
                   | SpacingFractionalOdd
                   | VertexOrderCw
                   | VertexOrderCcw
                   | PixelCenterImage
                   | OriginUpperLeft
                   | EarlyFragmentTests
                   | PointMode
                   | Xfb
                   | DepthReplacing
                   | DepthAny
                   | DepthGreater
                   | DepthLess
                   | DepthUnchanged
                   | LocalSize Word32 Word32 Word32 -- x y z
                   | LocalSizeHint Word32 Word32 Word32 -- x y z
                   | InputPoints
                   | InputLines
                   | InputLinesAdjacency
                   | InputTriangles
                   | InputTrianglesAdjacency
                   | InputQuads
                   | InputIsolines
                   | OutputVertices Word32
                   | OutputPoints
                   | OutputLineStrip
                   | OutputTriangleStrip
                   | VecTypeHint Id
                   | ContractionOff

data StorageClass = StorageUniformConstant
                  | StorageInput
                  | StorageUniform
                  | StorageOutput
                  | StorageWorkgroupLocal
                  | StorageWorkgroupGlobal
                  | StoragePrivateGlobal
                  | StorageFunction
                  | StorageGeneric
                  | StoragePrivate
                  | StorageAtomicCounter
    deriving (Show, Eq, Ord, Enum)

data Dim = Dim1D
         | Dim2D
         | Dim3D
         | DimCube
         | DimRect
         | DimBuffer
    deriving (Show, Eq, Ord, Enum)

-- Used by OpConstantSampler
data SamplerAddressingMode = None
                           | ClampEdge
                           | Clamp
                           | Repeat
                           | RepeatMirrored
    deriving (Show, Eq, Ord, Enum)

data SamplerFilterMode = Nearest
                       | Linear
    deriving (Show, Eq, Ord, Enum)

data FPFastMathMode = NotNaN
                    | NotInf
                    | NSZ
                    | AllowRecip
                    | Fast
    deriving (Show, Eq, Ord, Enum)

data FPRoundingMode = RTE
                    | RTZ
                    | RTP
                    | RTN
    deriving (Show, Eq, Ord, Enum)

data LinkageType = Export
                 | Import
    deriving (Show, Eq, Ord, Enum)

data AccessQualifier = ReadOnly
                     | WriteOnly
                     | ReadWrite
    deriving (Show, Eq, Ord, Enum)

data FunctionParameterAttribute = Zext
                                | Sext
                                | ByVal
                                | Sret
                                | NoAlias
                                | NoCapture
                                | SVM
                                | NoWrite
                                | NoReadWrite
    deriving (Show, Eq, Ord, Enum)

data Decoration = PrecisionLow
                | PrecisionMedium
                | PrecisionHigh
                | Block
                | BufferBlock
                | RowMajor
                | ColMajor
                | GLSLShared
                | GLSLStd140
                | GLSLStd430
                | GLSLPacked
                | Smooth
                | Noperspective
                | Flat
                | Patch
                | Centroid
                | Sample
                | Invariant
                | Restrict
                | Aliased
                | Volatile
                | Constant
                | Coherent
                | Nonwritable
                | Nonreadable
                | Uniform
                | NoStaticUse
                | CPacked
                | FPSaturatedConversion
                | Stream Word32
                | Location Word32
                | Component Word32
                | Index Word32
                | Binding Word32
                | DescriptorSet Word32
                | Offset Word32
                | Alignment Word32
                | XfbBuffer Word32
                | Stride Word32
                | Built'In Word32
                | FuncParamAttr FunctionParameterAttribute
                | FPRoundingMode FPRoundingMode
                | FPFastMathMode [FPFastMathMode]
                | LinkageType LinkageType
                | SpecId Word32

data BuiltIn = BuiltInPosition
             | BuiltInPointSize
             | BuiltInClipVertex
             | BuiltInClipDistance
             | BuiltInCullDistance
             | BuiltInVertexId
             | BuiltInInstanceId
             | BuiltInPrimitiveId
             | BuiltInInvocationId
             | BuiltInLayer
             | BuiltInViewportIndex
             | BuiltInTessLevelOuter
             | BuiltInTessLevelInner
             | BuiltInTessCoord
             | BuiltInPatchVertices
             | BuiltInFragCoord
             | BuiltInPointCoord
             | BuiltInFrontFacing
             | BuiltInSampleId
             | BuiltInSamplePosition
             | BuiltInSampleMask
             | BuiltInFragColor
             | BuiltInFragDepth
             | BuiltInHelperInvocation
             | BuiltInNumWorkgroups
             | BuiltInWorkgroupSize
             | BuiltInWorkgroupId
             | BuiltInLocalInvocationId
             | BuiltInGlobalInvocationId
             | BuiltInLocalInvocationIndex
             | BuiltInWorkDim
             | BuiltInGlobalSize
             | BuiltInEnqueuedWorkgroupSize
             | BuiltInGlobalOffset
             | BuiltInGlobalLinearId
             | BuiltInWorkgroupLinearId
             | BuiltInSubgroupSize
             | BuiltInSubgroupMaxSize
             | BuiltInNumSubgroups
             | BuiltInNumEnqueuedSubgroups
             | BuiltInSubgroupId
             | BuiltInSubgroupLocalInvocationId
    deriving (Show, Eq, Ord, Enum)

data SelectionControl = SelectionFlatten
                      | SelectionDontFlatten
    deriving (Show, Eq, Ord, Enum)

data LoopControl = LoopUnroll
                 | LoopDontUnroll
    deriving (Show, Eq, Ord, Enum)

data FunctionControl = FunctionControlInline
                     | FunctionControlDontInline
                     | FunctionControlPure
                     | FunctionControlConst
    deriving (Show, Eq, Ord, Enum)

data MemorySemantics = Relaxed
                     | SequentiallyConsistent
                     | Acquire
                     | Release
                     | UniformMemory
                     | SubgroupMemory
                     | WorkgroupLocalMemory
                     | WorkgroupGlobalMemory
                     | AtomicCounterMemory
                     | ImageMemory
    deriving (Show, Eq, Ord, Enum)

data MemoryAccess = MemoryNormal
                  | MemoryVolatile
                  | MemoryAligned Word32
                  | MemoryVolatileAligned Word32

data ExecutionScope = CrossDevice
                    | Device
                    | Workgroup
                    | Subgroup
    deriving (Show, Eq, Ord, Enum)

data GroupOperation = Reduce
                    | InclusiveScan
                    | ExclusiveScan
    deriving (Show, Eq, Ord, Enum)

data KernelEnqueueFlags = NoWait
                        | WaitKernel
                        | WaitWorkGroup
    deriving (Show, Eq, Ord, Enum)

data KernelProfilingInfo = CmdExeTime
    deriving (Show, Eq, Ord, Enum)

-- Instruction meta-data
-- Since there are so many different instructions we clump up all needed
-- meta-data into a single aggregate data type so that there's only ever one
-- function that needs to pattern match on every constructor for Instruction.

data InstructionMeta = InstructionMeta {
    resultId :: Maybe Id,
    wordCount :: Word32,
    opCode :: Word32
}

getResultId :: Instruction -> Maybe Id
getResultId = resultId . instMetaData

getWordCount :: Instruction -> Word32
getWordCount = wordCount . instMetaData

getOpCode :: Instruction -> Word32
getOpCode = opCode . instMetaData

instMetaData :: Instruction -> InstructionMeta
instMetaData inst = case inst of
    OpNop                                           -> im Nothing   1   0
    OpUndef _ i                                     -> im (Just i)  3  45
    OpSource _ _                                    -> im Nothing   3   1
    OpSourceExtension b                             -> im Nothing  (1 + bc b)   2
    OpName _ b                                      -> im Nothing  (2 + bc b)  54
    OpMemberName _ _ b                              -> im Nothing  (3 + bc b)  55
    OpString i b                                    -> im (Just i) (2 + bc b)  56
    OpLine _ _ _ _                                  -> im Nothing   5  57
    OpDecorationGroup i                             -> im (Just i)  2  49
    OpDecorate _ d                                  -> im Nothing  (3 + dc d)  50
    OpMemberDecorate _ _ d                          -> im Nothing  (4 + dc d)  51
    OpGroupDecorate _ s                             -> im Nothing  (2 + ln s)  52
    OpGroupMemberDecorate _ s                       -> im Nothing  (2 + ln s)  53
    OpExtension b                                   -> im Nothing  (1 + bc b)   3
    OpExtInstImport i b                             -> im (Just i) (2 + bc b)   4
    OpExtInst _ i _ _ s                             -> im (Just i) (5 + ln s)  44
    OpMemoryModel _ _                               -> im Nothing   3   5
    OpEntryPoint _ _                                -> im Nothing   3   6
    OpExecutionMode _ m                             -> im Nothing  (3 + mc m)   7
    OpCompileFlag b                                 -> im Nothing  (1 + bc b) 218
    OpTypeVoid i                                    -> im (Just i)  2   8
    OpTypeBool i                                    -> im (Just i)  2   9
    OpTypeInt i _ _                                 -> im (Just i)  4  10
    OpTypeFloat i _                                 -> im (Just i)  3  11
    OpTypeVector i _ _                              -> im (Just i)  4  12
    OpTypeMatrix i _ _                              -> im (Just i)  4  13
    OpTypeSampler i _ _ _ _ _ _ j                   -> im (Just i) (8 + ij j)  14
    OpTypeFilter i                                  -> im (Just i)  2  15
    OpTypeArray i _ _                               -> im (Just i)  4  16
    OpTypeRuntimeArray i _                          -> im (Just i)  3  17
    OpTypeStruct i s                                -> im (Just i) (2 + ln s)  18
    OpTypeOpaque i b                                -> im (Just i) (2 + bc b)  19
    OpTypePointer i _ _                             -> im (Just i)  4  20
    OpTypeFunction i _ s                            -> im (Just i) (3 + ln s)  21
    OpTypeEvent i                                   -> im (Just i)  2  22
    OpTypeDeviceEvent i                             -> im (Just i)  2  23
    OpTypeReserveId i                               -> im (Just i)  2  24
    OpTypeQueue i                                   -> im (Just i)  2  25
    OpTypePipe i _ _                                -> im (Just i)  4  26
    OpConstantTrue _ i                              -> im (Just i)  3  27
    OpConstantFalse _ i                             -> im (Just i)  3  28
    OpConstant _ i s                                -> im (Just i) (3 + ln s)  29
    OpConstantComposite _ i s                       -> im (Just i) (3 + ln s)  30
    OpConstantSampler _ i _ _ _                     -> im (Just i)  6  31
    OpConstantNullPointer _ i                       -> im (Just i)  3  32
    OpConstantNullObject _ i                        -> im (Just i)  3  33
    OpSpecConstantTrue _ i                          -> im (Just i)  3  34
    OpSpecConstantFalse _ i                         -> im (Just i)  3  35
    OpSpecConstant _ i s                            -> im (Just i) (3 + ln s)  36
    OpSpecConstantComposite _ i s                   -> im (Just i) (3 + ln s)  37
    OpVariable _ i _ j                              -> im (Just i) (4 + ij j)  38
    OpVariableArray _ i _ _                         -> im (Just i)  5  39
    OpLoad _ i _ m                                  -> im (Just i) (4 + ma m)  46
    OpStore _ _ m                                   -> im Nothing  (3 + ma m)  47
    OpCopyMemory _ _ m                              -> im Nothing  (3 + ma m)  65
    OpCopyMemorySized _ _ _ m                       -> im Nothing  (4 + ma m)  66
    OpAccessChain _ i _ s                           -> im (Just i) (4 + ln s)  93
    OpInBoundsAccessChain _ i _ s                   -> im (Just i) (4 + ln s)  94
    OpArrayLength _ i _ _                           -> im (Just i)  5 121
    OpImagePointer _ i _ _ _                        -> im (Just i)  6 190
    OpGenericPtrMemSemantics _ i _                  -> im (Just i)  4 233
    OpFunction _ i _ _                              -> im (Just i)  5  40
    OpFunctionParameter _ i                         -> im (Just i)  3  41
    OpFunctionEnd                                   -> im Nothing   1  42
    OpFunctionCall _ i _ s                          -> im (Just i) (4 + ln s)  43
    OpSampler _ i _ _                               -> im (Just i)  5  67
    OpTextureSample _ i _ _ j                       -> im (Just i) (5 + ij j)  68
    OpTextureSampleDref _ i _ _ _                   -> im (Just i)  6  69
    OpTextureSampleLod _ i _ _ _                    -> im (Just i)  6  70
    OpTextureSampleProj _ i _ _ j                   -> im (Just i) (5 + ij j)  71
    OpTextureSampleGrad _ i _ _ _ _                 -> im (Just i)  7  72
    OpTextureSampleOffset _ i _ _ _ j               -> im (Just i) (6 + ij j)  73
    OpTextureSampleProjLod _ i _ _ _                -> im (Just i)  6  74
    OpTextureSampleProjGrad _ i _ _ _ _             -> im (Just i)  7  75
    OpTextureSampleLodOffset _ i _ _ _ _            -> im (Just i)  7  76
    OpTextureSampleProjOffset _ i _ _ _ j           -> im (Just i) (6 + ij j)  77
    OpTextureSampleGradOffset _ i _ _ _ _ _         -> im (Just i)  8  78
    OpTextureSampleProjLodOffset _ i _ _ _ _        -> im (Just i)  7  79
    OpTextureSampleProjGradOffset _ i _ _ _ _ _     -> im (Just i)  8  80
    OpTextureFetchTexelLod _ i _ _ _                -> im (Just i)  6  81
    OpTextureFetchTexelOffset _ i _ _ _             -> im (Just i)  6  82
    OpTextureFetchTexelSample _ i _ _ _             -> im (Just i)  6  83
    OpTextureFetchTexel _ i _ _                     -> im (Just i)  5  84
    OpTextureGather _ i _ _ _                       -> im (Just i)  6  85
    OpTextureGatherOffset _ i _ _ _ _               -> im (Just i)  7  86
    OpTextureGatherOffsets _ i _ _ _ _              -> im (Just i)  7  87
    OpTextureQuerySizeLod _ i _ _                   -> im (Just i)  5  88
    OpTextureQuerySize _ i _                        -> im (Just i)  4  89
    OpTextureQueryLod _ i _ _                       -> im (Just i)  5  90
    OpTextureQueryLevels _ i _                      -> im (Just i)  4  91
    OpTextureQuerySamples _ i _                     -> im (Just i)  4  92
    OpConvertFToU _ i _                             -> im (Just i)  4 100
    OpConvertFToS _ i _                             -> im (Just i)  4 101
    OpConvertSToF _ i _                             -> im (Just i)  4 102
    OpConvertUToF _ i _                             -> im (Just i)  4 103
    OpUConvert _ i _                                -> im (Just i)  4 104
    OpSConvert _ i _                                -> im (Just i)  4 105
    OpFConvert _ i _                                -> im (Just i)  4 106
    OpConvertPtrToU _ i _                           -> im (Just i)  4 107
    OpConvertUToPtr _ i _                           -> im (Just i)  4 108
    OpPtrCastToGeneric _ i _                        -> im (Just i)  4 109
    OpGenericCastToPtr _ i _                        -> im (Just i)  4 110
    OpBitcast _ i _                                 -> im (Just i)  4 111
    OpGenericCastToPtrExplicit _ i _ _              -> im (Just i)  5 232
    OpSatConvertSToU _ i _                          -> im (Just i)  4 263
    OpSatConvertUToS _ i _                          -> im (Just i)  4 264
    OpVectorExtractDynamic _ i _ _                  -> im (Just i)  5  58
    OpVectorInsertDynamic _ i _ _ _                 -> im (Just i)  6  59
    OpVectorShuffle _ i _ _ s                       -> im (Just i) (5 + ln s)  60
    OpCompositeConstruct _ i s                      -> im (Just i) (3 + ln s)  61
    OpCompositeExtract _ i _ s                      -> im (Just i) (4 + ln s)  62
    OpCompositeInsert _ i _ _ s                     -> im (Just i) (5 + ln s)  63
    OpCopyObject _ i _                              -> im (Just i)  4  64
    OpTranspose _ i _                               -> im (Just i)  4 112
    OpSNegate _ i _                                 -> im (Just i)  4  95
    OpFNegate _ i _                                 -> im (Just i)  4  96
    OpNot _ i _                                     -> im (Just i)  4  97
    OpIAdd _ i _ _                                  -> im (Just i)  5 122
    OpFAdd _ i _ _                                  -> im (Just i)  5 123
    OpISub _ i _ _                                  -> im (Just i)  5 124
    OpFSub _ i _ _                                  -> im (Just i)  5 125
    OpIMul _ i _ _                                  -> im (Just i)  5 126
    OpFMul _ i _ _                                  -> im (Just i)  5 127
    OpUDiv _ i _ _                                  -> im (Just i)  5 128
    OpSDiv _ i _ _                                  -> im (Just i)  5 129
    OpFDiv _ i _ _                                  -> im (Just i)  5 130
    OpUMod _ i _ _                                  -> im (Just i)  5 131
    OpSRem _ i _ _                                  -> im (Just i)  5 132
    OpSMod _ i _ _                                  -> im (Just i)  5 133
    OpFRem _ i _ _                                  -> im (Just i)  5 134
    OpFMod _ i _ _                                  -> im (Just i)  5 135
    OpVectorTimesScalar _ i _ _                     -> im (Just i)  5 136
    OpMatrixTimesScalar _ i _ _                     -> im (Just i)  5 137
    OpVectorTimesMatrix _ i _ _                     -> im (Just i)  5 138
    OpMatrixTimesVector _ i _ _                     -> im (Just i)  5 139
    OpMatrixTimesMatrix _ i _ _                     -> im (Just i)  5 140
    OpOuterProduct _ i _ _                          -> im (Just i)  5 141
    OpDot _ i _ _                                   -> im (Just i)  5 142
    OpShiftRightLogical _ i _ _                     -> im (Just i)  5 143
    OpShiftRightArithmetic _ i _ _                  -> im (Just i)  5 144
    OpShiftLeftLogical _ i _ _                      -> im (Just i)  5 145
    OpBitwiseOr _ i _ _                             -> im (Just i)  5 149
    OpBitwiseXor _ i _ _                            -> im (Just i)  5 150
    OpBitwiseAnd _ i _ _                            -> im (Just i)  5 151
    OpAny _ i _                                     -> im (Just i)  4  98
    OpAll _ i _                                     -> im (Just i)  4  99
    OpIsNaN _ i _                                   -> im (Just i)  4 113
    OpIsInf _ i _                                   -> im (Just i)  4 114
    OpIsFinite _ i _                                -> im (Just i)  4 115
    OpIsNormal _ i _                                -> im (Just i)  4 116
    OpSignBitSet _ i _                              -> im (Just i)  4 117
    OpLessOrGreater _ i _ _                         -> im (Just i)  5 118
    OpOrdered _ i _ _                               -> im (Just i)  5 119
    OpUnordered _ i _ _                             -> im (Just i)  5 120
    OpLogicalOr _ i _ _                             -> im (Just i)  5 146
    OpLogicalXor _ i _ _                            -> im (Just i)  5 147
    OpLogicalAnd _ i _ _                            -> im (Just i)  5 148
    OpSelect _ i _ _ _                              -> im (Just i)  6 152
    OpIEqual _ i _ _                                -> im (Just i)  5 153
    OpFOrdEqual _ i _ _                             -> im (Just i)  5 154
    OpFUnordEqual _ i _ _                           -> im (Just i)  5 155
    OpINotEqual _ i _ _                             -> im (Just i)  5 156
    OpFOrdNotEqual _ i _ _                          -> im (Just i)  5 157
    OpFUnordNotEqual _ i _ _                        -> im (Just i)  5 158
    OpULessThan _ i _ _                             -> im (Just i)  5 159
    OpSLessThan _ i _ _                             -> im (Just i)  5 160
    OpFOrdLessThan _ i _ _                          -> im (Just i)  5 161
    OpFUnordLessThan _ i _ _                        -> im (Just i)  5 162
    OpUGreaterThan _ i _ _                          -> im (Just i)  5 163
    OpSGreaterThan _ i _ _                          -> im (Just i)  5 164
    OpFOrdGreaterThan _ i _ _                       -> im (Just i)  5 165
    OpFUnordGreaterThan _ i _ _                     -> im (Just i)  5 166
    OpULessThanEqual _ i _ _                        -> im (Just i)  5 167
    OpSLessThanEqual _ i _ _                        -> im (Just i)  5 168
    OpFOrdLessThanEqual _ i _ _                     -> im (Just i)  5 169
    OpFUnordLessThanEqual _ i _ _                   -> im (Just i)  5 170
    OpUGreaterThanEqual _ i _ _                     -> im (Just i)  5 171
    OpSGreaterThanEqual _ i _ _                     -> im (Just i)  5 172
    OpFOrdGreaterThanEqual _ i _ _                  -> im (Just i)  5 173
    OpFUnordGreaterThanEqual _ i _ _                -> im (Just i)  5 174
    OpDPdx _ i _                                    -> im (Just i)  4 175
    OpDPdy _ i _                                    -> im (Just i)  4 176
    OpFwidth _ i _                                  -> im (Just i)  4 177
    OpDPdxFine _ i _                                -> im (Just i)  4 178
    OpDPdyFine _ i _                                -> im (Just i)  4 179
    OpFwidthFine _ i _                              -> im (Just i)  4 180
    OpDPdxCoarse _ i _                              -> im (Just i)  4 181
    OpDPdyCoarse _ i _                              -> im (Just i)  4 182
    OpFwidthCoarse _ i _                            -> im (Just i)  4 183
    OpPhi _ i s                                     -> im (Just i) (3 + (ln s * 2))  48
    OpLoopMerge _ _                                 -> im Nothing   3 206
    OpSelectionMerge _ _                            -> im Nothing   3 207
    OpLabel i                                       -> im (Just i)  2 208
    OpBranch _                                      -> im Nothing   2 209
    OpBranchConditional _ _ _ j                     -> im Nothing  (4 + (ij j * 2)) 210
    OpSwitch _ _ s                                  -> im Nothing  (3 + (ln s * 2)) 211
    OpKill                                          -> im Nothing   1 212
    OpReturn                                        -> im Nothing   1 213
    OpReturnValue _                                 -> im Nothing   2 214
    OpUnreachable                                   -> im Nothing   1 215
    OpLifetimeStart _ _                             -> im Nothing   3 216
    OpLifetimeStop _ _                              -> im Nothing   3 217
    OpAtomicInit _ _                                -> im Nothing   3 191
    OpAtomicLoad _ i _ _ _                          -> im (Just i)  6 192
    OpAtomicStore _ _ _ _                           -> im Nothing   5 193
    OpAtomicExchange _ i _ _ _ _                    -> im (Just i)  7 194
    OpAtomicCompareExchange _ i _ _ _ _ _           -> im (Just i)  8 195
    OpAtomicCompareExchangeWeak _ i _ _ _ _ _       -> im (Just i)  8 196
    OpAtomicIIncrement _ i _ _ _                    -> im (Just i)  6 197
    OpAtomicIDecrement _ i _ _ _                    -> im (Just i)  6 198
    OpAtomicIAdd _ i _ _ _ _                        -> im (Just i)  7 199
    OpAtomicISub _ i _ _ _ _                        -> im (Just i)  7 200
    OpAtomicUMin _ i _ _ _ _                        -> im (Just i)  7 201
    OpAtomicUMax _ i _ _ _ _                        -> im (Just i)  7 202
    OpAtomicAnd _ i _ _ _ _                         -> im (Just i)  7 203
    OpAtomicOr _ i _ _ _ _                          -> im (Just i)  7 204
    OpAtomicXor _ i _ _ _ _                         -> im (Just i)  7 205
    OpAtomicIMin _ i _ _ _ _                        -> im (Just i)  7 265
    OpAtomicIMax _ i _ _ _ _                        -> im (Just i)  7 266
    OpEmitVertex                                    -> im Nothing   1 184
    OpEndPrimitive                                  -> im Nothing   1 185
    OpEmitStreamVertex _                            -> im Nothing   2 186
    OpEndStreamPrimitive _                          -> im Nothing   2 187
    OpControlBarrier _                              -> im Nothing   2 188
    OpMemoryBarrier _ _                             -> im Nothing   3 189
    OpAsyncGroupCopy _ i _ _ _ _ _ _                -> im (Just i)  9 219
    OpWaitGroupEvents _ i _ _ _                     -> im (Just i)  6 220
    OpGroupAll _ i _ _                              -> im (Just i)  5 221
    OpGroupAny _ i _ _                              -> im (Just i)  5 222
    OpGroupBroadcast _ i _ _ _                      -> im (Just i)  6 223
    OpGroupIAdd _ i _ _ _                           -> im (Just i)  6 224
    OpGroupFAdd _ i _ _ _                           -> im (Just i)  6 225
    OpGroupFMin _ i _ _ _                           -> im (Just i)  6 226
    OpGroupUMin _ i _ _ _                           -> im (Just i)  6 227
    OpGroupSMin _ i _ _ _                           -> im (Just i)  6 228
    OpGroupFMax _ i _ _ _                           -> im (Just i)  6 229
    OpGroupUMax _ i _ _ _                           -> im (Just i)  6 230
    OpGroupSMax _ i _ _ _                           -> im (Just i)  6 231
    OpEnqueueMarker _ i _ _ _ _                     -> im (Just i)  7 249
    OpEnqueueKernel _ i _ _ _ _ _ _ _ _ _ _ s       -> im (Just i) (13 + ln s) 250
    OpGetKernelNDrangeSubGroupCount _ i _ _         -> im (Just i)  5 251
    OpGetKernelNDrangeMaxSubGroupCount _ i _ _      -> im (Just i)  5 252
    OpGetKernelWorkGroupSize _ i _                  -> im (Just i)  4 253
    OpGetKernelPreferredWorkGroupSizeMultiple _ i _ -> im (Just i)  4 254
    OpRetainEvent _                                 -> im Nothing   2 255
    OpReleaseEvent _                                -> im Nothing   2 256
    OpCreateUserEvent _ i                           -> im (Just i)  3 257
    OpIsValidEvent _ i _                            -> im (Just i)  4 258
    OpSetUserEventStatus _ _                        -> im Nothing   3 259
    OpCaptureEventProfilingInfo _ _ _               -> im Nothing   4 260
    OpGetDefaultQueue _ i                           -> im (Just i)  3 261
    OpBuildNDRange _ i _ _ _                        -> im (Just i)  6 262
    OpReadPipe _ i _ _                              -> im (Just i)  5 234
    OpWritePipe _ i _ _                             -> im (Just i)  5 235
    OpReservedReadPipe _ i _ _ _ _                  -> im (Just i)  7 236
    OpReservedWritePipe _ i _ _ _ _                 -> im (Just i)  7 237
    OpReserveReadPipePackets _ i _ _                -> im (Just i)  5 238
    OpReserveWritePipePackets _ i _ _               -> im (Just i)  5 239
    OpCommitReadPipe _ _                            -> im Nothing   3 240
    OpCommitWritePipe _ _                           -> im Nothing   3 241
    OpIsValidReserveId _ i _                        -> im (Just i)  4 242
    OpGetNumPipePackets _ i _                       -> im (Just i)  4 243
    OpGetMaxPipePackets _ i _                       -> im (Just i)  4 244
    OpGroupReservedReadPipePackets _ i _ _ _        -> im (Just i)  6 245
    OpGroupReservedWritePipePackets _ i _ _ _       -> im (Just i)  6 246
    OpGroupCommitReadPipe _ _ _                     -> im Nothing   4 247
    OpGroupCommitWritePipe _ _ _                    -> im Nothing   4 248
  where
    im = InstructionMeta
    -- We need to divide by 4, rounding up, to determine how many Word32s we
    -- need to pack all the Word8s of the ByteString. We also need 1 extra Word8
    -- because SPIR-V strings are null-terminated while ByteStrings are not.
    -- This is why we add 4 instead of 3 to have our divide round up.
    bc s = fromIntegral (BS.length s + 4) `div` 4
    dc (Stream _) = 1
    dc (Location _) = 1
    dc (Component _) = 1
    dc (Index _) = 1
    dc (Binding _) = 1
    dc (DescriptorSet _) = 1
    dc (Offset _) = 1
    dc (Alignment _) = 1
    dc (XfbBuffer _) = 1
    dc (Stride _) = 1
    dc (Built'In _) = 1
    dc (FuncParamAttr _) = 1
    dc (FPRoundingMode _) = 1
    dc (FPFastMathMode _) = 1
    dc (LinkageType _) = 1
    dc (SpecId _) = 1
    dc _ = 0
    ln = fromIntegral . length
    mc (Invocations _) = 1
    mc (LocalSize _ _ _) = 3
    mc (LocalSizeHint _ _ _) = 3
    mc (OutputVertices _) = 1
    mc (VecTypeHint _) = 1
    mc _ = 0
    ij = maybe 0 (const 1)
    ma MemoryNormal = 0
    ma MemoryVolatile = 1
    ma (MemoryAligned _) = 2
    ma (MemoryVolatileAligned _) = 2
