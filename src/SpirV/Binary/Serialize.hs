{-
 - Serialize.hs
 - By Steven Smith
 -}

module SpirV.Binary.Serialize where

import Data.ByteString (ByteString)
import Data.Vector.Storable (Vector)
import Data.Word
import Data.Bits
import System.Endian
import Data.Foldable (toList)

import SpirV.Instructions
import SpirV.Builder.Types
import SpirV.Builder.Types.Internal

import qualified Data.ByteString as BS
import qualified Data.Vector.Storable as V

byteStringToWords :: ByteString -> [Word32]
byteStringToWords = map (toLE32 . collapse) . chunk . BS.unpack
  where
    chunk (x:y:z:w:tl) = (x,y,z,w) : chunk tl
    chunk [x,y,z] = (x,y,z,0) : []
    chunk [x,y] = (x,y,0,0) : []
    chunk [x] = (x,0,0,0) : []
    chunk [] = (0,0,0,0) : []
    collapse :: (Word8, Word8, Word8, Word8) -> Word32
    collapse (x,y,z,w) = fromIntegral (shiftL x 24)
        .|. fromIntegral (shiftL y 16)
        .|. fromIntegral (shiftL z 8)
        .|. fromIntegral w

addOpCode :: Instruction -> [Word32] -> [Word32]
addOpCode i ws = (shiftL opLen 16 .|. getOpCode i) : ws
  where
    opLen = fromIntegral (1 + length ws)

class BitMask a => BitMaskExtra a where
    extrasToWords :: a -> [Word32]

instance BitMaskExtra ExecutionMode where
    extrasToWords em = case em of
        Invocations a       -> [bm, a]
        LocalSize x y z     -> [bm, x, y, z]
        LocalSizeHint x y z -> [bm, x, y, z]
        OutputVertices a    -> [bm, a]
        VecTypeHint a       -> [bm, toBitMask a]
        _                   -> [bm]
      where
        bm = toBitMask em

instance BitMaskExtra Decoration where
    extrasToWords d = case d of
        Stream a         -> [bm, a]
        Location a       -> [bm, a]
        Component a      -> [bm, a]
        Index a          -> [bm, a]
        Binding a        -> [bm, a]
        DescriptorSet a  -> [bm, a]
        Offset a         -> [bm, a]
        Alignment a      -> [bm, a]
        XfbBuffer a      -> [bm, a]
        Stride a         -> [bm, a]
        Built'In a       -> [bm, a]
        FuncParamAttr a  -> [bm, toBitMask a]
        FPRoundingMode a -> [bm, toBitMask a]
        FPFastMathMode a -> [bm, toBitMask a]
        LinkageType a    -> [bm, toBitMask a]
        SpecId a         -> [bm, a]
        _                -> [bm]
      where
        bm = toBitMask d

instance BitMaskExtra MemoryAccess where
    extrasToWords ma = case ma of
        MemoryAligned a         -> [bm, a]
        MemoryVolatileAligned a -> [bm, a]
        _                       -> [bm]
      where
        bm = toBitMask ma

encodeModule :: SpirVModule -> Vector Word32
encodeModule m = V.fromList (smn : svn : gmn : ib : (is >>= encodeInstruction))
  where
    smn = spirVMagicNumber m
    svn = spirVVersionNumber m
    gmn = genMagicNumber m
    ib  = idBound m
    is  = toList (instructionStream m)

encodeInstruction :: Instruction -> [Word32]
encodeInstruction i = addOpCode i $ case i of
    OpNop                          -> []
    OpUndef a b                    -> [bm a, bm b]
    OpSource a b                   -> [a, b]
    OpSourceExtension bs           -> bw bs
    OpName a bs                    -> bm a : bw bs
    OpMemberName a b bs            -> bm a : b : bw bs
    OpString a bs                  -> bm a : bw bs
    OpLine a b c d                 -> [bm a, bm b, c, d]
    OpDecorationGroup a            -> [bm a]
    OpDecorate a d                 -> bm a : bme d
    OpMemberDecorate a b d         -> bm a : b : bme d
    OpGroupDecorate a bs           -> bm a : map bm bs
    OpGroupMemberDecorate a bs     -> bm a : map bm bs
    OpExtension bs                 -> bw bs
    OpExtInstImport a bs           -> bm a : bw bs
    OpExtInst a b c d es           -> bm a : bm b : bm c : d : map bm es
    OpMemoryModel a b              -> [bm a, bm b]
    OpEntryPoint a b               -> [bm a, bm b]
    OpExecutionMode a b            -> bm a : bme b
    OpCompileFlag bs               -> bw bs
    OpTypeVoid a                   -> [bm a]
    OpTypeBool a                   -> [bm a]
    OpTypeInt a b c                -> [bm a, b, bm c]
    OpTypeFloat a b                -> [bm a, b]
    OpTypeVector a b c             -> [bm a, bm b, c]
    OpTypeMatrix a b c             -> [bm a, bm b, c]
    OpTypeSampler a b c d e f g h
        -> bm a : bm b : bm c : bm d : bm e : bm f : bm g : bmm h
    OpTypeFilter a                 -> [bm a]
    OpTypeArray a b c              -> [bm a, bm b, bm c]
    OpTypeRuntimeArray a b         -> [bm a, bm b]
    OpTypeStruct a bs              -> bm a : map bm bs
    OpTypeOpaque a bs              -> bm a : bw bs
    OpTypePointer a b c            -> [bm a, bm b, bm c]
    OpTypeFunction a b cs          -> bm a : bm b : map bm cs
    OpTypeEvent a                  -> [bm a]
    OpTypeDeviceEvent a            -> [bm a]
    OpTypeReserveId a              -> [bm a]
    OpTypeQueue a                  -> [bm a]
    OpTypePipe a b c               -> [bm a, bm b, bm c]
    OpConstantTrue a b             -> [bm a, bm b]
    OpConstantFalse a b            -> [bm a, bm b]
    OpConstant a b cs              -> bm a : bm b : cs
    OpConstantComposite a b cs     -> bm a : bm b : map bm cs
    OpConstantSampler a b c d e    -> [bm a, bm b, bm c, bm d, bm e]
    OpConstantNullPointer a b      -> [bm a, bm b]
    OpConstantNullObject a b       -> [bm a, bm b]
    OpSpecConstantTrue a b         -> [bm a, bm b]
    OpSpecConstantFalse a b        -> [bm a, bm b]
    OpSpecConstant a b cs          -> bm a : bm b : cs
    OpSpecConstantComposite a b cs -> bm a : bm b : map bm cs
    OpVariable a b c d             -> bm a : bm b : bm c : bmm d
    OpVariableArray a b c d        -> [bm a, bm b, bm c, bm d]
    OpLoad a b c d                 -> bm a : bm b : bm c : bme d
    OpStore a b c                  -> bm a : bm b : bme c
    OpCopyMemory a b c             -> bm a : bm b : bme c
    OpCopyMemorySized a b c d      -> bm a : bm b : bm c : bme d
    OpAccessChain a b c ds         -> bm a : bm b : bm c : map bm ds
    OpInBoundsAccessChain a b c ds -> bm a : bm b : bm c : map bm ds
    OpArrayLength a b c d          -> [bm a, bm b, bm c, d]
    OpImagePointer a b c d e       -> [bm a, bm b, bm c, bm d, bm e]
    OpGenericPtrMemSemantics a b c -> [bm a, bm b, bm c]
    OpFunction a b cs d            -> [bm a, bm b, bm cs, bm d]
    OpFunctionParameter a b        -> [bm a, bm b]
    OpFunctionEnd                  -> []
    OpFunctionCall a b c ds        -> bm a : bm b : bm c : map bm ds
    OpSampler a b c d              -> [bm a, bm b, bm c, bm d]
    OpTextureSample a b c d e      -> bm a : bm b : bm c : bm d : bmm e
    OpTextureSampleDref a b c d e  -> [bm a, bm b, bm c, bm d, bm e]
    OpTextureSampleLod a b c d e   -> [bm a, bm b, bm c, bm d, bm e]
    OpTextureSampleProj a b c d e  -> bm a : bm b : bm c : bm d : bmm e
    OpTextureSampleGrad a b c d e f -> [bm a, bm b, bm c, bm d, bm e, bm f]
    OpTextureSampleOffset a b c d e f
        -> bm a : bm b : bm c : bm d : bm e : bmm f
    OpTextureSampleProjLod a b c d e -> [bm a, bm b, bm c, bm d, bm e]
    OpTextureSampleProjGrad a b c d e f
        -> [bm a, bm b, bm c, bm d, bm e, bm f]
    OpTextureSampleLodOffset a b c d e f
        -> [bm a, bm b, bm c, bm d, bm e, bm f]
    OpTextureSampleProjOffset a b c d e f
        -> bm a : bm b : bm c : bm d : bm e : bmm f
    OpTextureSampleGradOffset a b c d e f g
        -> [bm a, bm b, bm c, bm d, bm e, bm f, bm g]
    OpTextureSampleProjLodOffset a b c d e f
        -> [bm a, bm b, bm c, bm d, bm e, bm f]
    OpTextureSampleProjGradOffset a b c d e f g
        -> [bm a, bm b, bm c, bm d, bm e, bm f, bm g]
    OpTextureFetchTexelLod a b c d e -> [bm a, bm b, bm c, bm d, bm d, bm e]
    OpTextureFetchTexelOffset a b c d e -> [bm a, bm b, bm c, bm d, bm e]
    OpTextureFetchTexelSample a b c d e -> [bm a, bm b, bm c, bm d, bm e]
    OpTextureFetchTexel a b c d    -> [bm a, bm b, bm c, bm d]
    OpTextureGather a b c d e      -> [bm a, bm b, bm c, bm d, bm e]
    OpTextureGatherOffset a b c d e f -> [bm a, bm b, bm c, bm d, bm e, bm f]
    OpTextureGatherOffsets a b c d e f -> [bm a, bm b, bm c, bm d, bm e, bm f]
    OpTextureQuerySizeLod a b c d  -> [bm a, bm b, bm c, bm d]
    OpTextureQuerySize a b c       -> [bm a, bm b, bm c]
    OpTextureQueryLod a b c d      -> [bm a, bm b, bm c, bm d]
    OpTextureQueryLevels a b c     -> [bm a, bm b, bm c]
    OpTextureQuerySamples a b c    -> [bm a, bm b, bm c]
    OpConvertFToU a b c            -> [bm a, bm b, bm c]
    OpConvertFToS a b c            -> [bm a, bm b, bm c]
    OpConvertSToF a b c            -> [bm a, bm b, bm c]
    OpConvertUToF a b c            -> [bm a, bm b, bm c]
    OpUConvert a b c               -> [bm a, bm b, bm c]
    OpSConvert a b c               -> [bm a, bm b, bm c]
    OpFConvert a b c               -> [bm a, bm b, bm c]
    OpConvertPtrToU a b c          -> [bm a, bm b, bm c]
    OpConvertUToPtr a b c          -> [bm a, bm b, bm c]
    OpPtrCastToGeneric a b c       -> [bm a, bm b, bm c]
    OpGenericCastToPtr a b c       -> [bm a, bm b, bm c]
    OpBitcast a b c                -> [bm a, bm b, bm c]
    OpGenericCastToPtrExplicit a b c d -> [bm a, bm b, bm c, bm d]
    OpSatConvertSToU a b c         -> [bm a, bm b, bm c]
    OpSatConvertUToS a b c         -> [bm a, bm b, bm c]
    OpVectorExtractDynamic a b c d -> [bm a, bm b, bm c, bm d]
    OpVectorInsertDynamic a b c d e -> [bm a, bm b, bm c, bm d, bm e]
    OpVectorShuffle a b c d es     -> bm a : bm b : bm c : bm d : es
    OpCompositeConstruct a b cs    -> bm a : bm b : map bm cs
    OpCompositeExtract a b c ds    -> bm a : bm b : bm c : ds
    OpCompositeInsert a b c d es   -> bm a : bm b : bm c : bm d : es
    OpCopyObject a b c             -> [bm a, bm b, bm c]
    OpTranspose a b c              -> [bm a, bm b, bm c]
    OpSNegate a b c                -> [bm a, bm b, bm c]
    OpFNegate a b c                -> [bm a, bm b, bm c]
    OpNot a b c                    -> [bm a, bm b, bm c]
    OpIAdd a b c d                 -> [bm a, bm b, bm c, bm d]
    OpFAdd a b c d                 -> [bm a, bm b, bm c, bm d]
    OpISub a b c d                 -> [bm a, bm b, bm c, bm d]
    OpFSub a b c d                 -> [bm a, bm b, bm c, bm d]
    OpIMul a b c d                 -> [bm a, bm b, bm c, bm d]
    OpFMul a b c d                 -> [bm a, bm b, bm c, bm d]
    OpUDiv a b c d                 -> [bm a, bm b, bm c, bm d]
    OpSDiv a b c d                 -> [bm a, bm b, bm c, bm d]
    OpFDiv a b c d                 -> [bm a, bm b, bm c, bm d]
    OpUMod a b c d                 -> [bm a, bm b, bm c, bm d]
    OpSRem a b c d                 -> [bm a, bm b, bm c, bm d]
    OpSMod a b c d                 -> [bm a, bm b, bm c, bm d]
    OpFRem a b c d                 -> [bm a, bm b, bm c, bm d]
    OpFMod a b c d                 -> [bm a, bm b, bm c, bm d]
    OpVectorTimesScalar a b c d    -> [bm a, bm b, bm c, bm d]
    OpMatrixTimesScalar a b c d    -> [bm a, bm b, bm c, bm d]
    OpVectorTimesMatrix a b c d    -> [bm a, bm b, bm c, bm d]
    OpMatrixTimesVector a b c d    -> [bm a, bm b, bm c, bm d]
    OpMatrixTimesMatrix a b c d    -> [bm a, bm b, bm c, bm d]
    OpOuterProduct a b c d         -> [bm a, bm b, bm c, bm d]
    OpDot a b c d                  -> [bm a, bm b, bm c, bm d]
    OpShiftRightLogical a b c d    -> [bm a, bm b, bm c, bm d]
    OpShiftRightArithmetic a b c d -> [bm a, bm b, bm c, bm d]
    OpShiftLeftLogical a b c d     -> [bm a, bm b, bm c, bm d]
    OpBitwiseOr a b c d            -> [bm a, bm b, bm c, bm d]
    OpBitwiseXor a b c d           -> [bm a, bm b, bm c, bm d]
    OpBitwiseAnd a b c d           -> [bm a, bm b, bm c, bm d]
    OpAny a b c                    -> [bm a, bm b, bm c]
    OpAll a b c                    -> [bm a, bm b, bm c]
    OpIsNaN a b c                  -> [bm a, bm b, bm c]
    OpIsInf a b c                  -> [bm a, bm b, bm c]
    OpIsFinite a b c               -> [bm a, bm b, bm c]
    OpIsNormal a b c               -> [bm a, bm b, bm c]
    OpSignBitSet a b c             -> [bm a, bm b, bm c]
    OpLessOrGreater a b c d        -> [bm a, bm b, bm c, bm d]
    OpOrdered a b c d              -> [bm a, bm b, bm c, bm d]
    OpUnordered a b c d            -> [bm a, bm b, bm c, bm d]
    OpLogicalOr a b c d            -> [bm a, bm b, bm c, bm d]
    OpLogicalXor a b c d           -> [bm a, bm b, bm c, bm d]
    OpLogicalAnd a b c d           -> [bm a, bm b, bm c, bm d]
    OpSelect a b c d e             -> [bm a, bm b, bm c, bm d, bm e]
    OpIEqual a b c d               -> [bm a, bm b, bm c, bm d]
    OpFOrdEqual a b c d            -> [bm a, bm b, bm c, bm d]
    OpFUnordEqual a b c d          -> [bm a, bm b, bm c, bm d]
    OpINotEqual a b c d            -> [bm a, bm b, bm c, bm d]
    OpFOrdNotEqual a b c d         -> [bm a, bm b, bm c, bm d]
    OpFUnordNotEqual a b c d       -> [bm a, bm b, bm c, bm d]
    OpULessThan a b c d            -> [bm a, bm b, bm c, bm d]
    OpSLessThan a b c d            -> [bm a, bm b, bm c, bm d]
    OpFOrdLessThan a b c d         -> [bm a, bm b, bm c, bm d]
    OpFUnordLessThan a b c d       -> [bm a, bm b, bm c, bm d]
    OpUGreaterThan a b c d         -> [bm a, bm b, bm c, bm d]
    OpSGreaterThan a b c d         -> [bm a, bm b, bm c, bm d]
    OpFOrdGreaterThan a b c d      -> [bm a, bm b, bm c, bm d]
    OpFUnordGreaterThan a b c d    -> [bm a, bm b, bm c, bm d]
    OpULessThanEqual a b c d       -> [bm a, bm b, bm c, bm d]
    OpSLessThanEqual a b c d       -> [bm a, bm b, bm c, bm d]
    OpFOrdLessThanEqual a b c d    -> [bm a, bm b, bm c, bm d]
    OpFUnordLessThanEqual a b c d  -> [bm a, bm b, bm c, bm d]
    OpUGreaterThanEqual a b c d    -> [bm a, bm b, bm c, bm d]
    OpSGreaterThanEqual a b c d    -> [bm a, bm b, bm c, bm d]
    OpFOrdGreaterThanEqual a b c d -> [bm a, bm b, bm c, bm d]
    OpFUnordGreaterThanEqual a b c d -> [bm a, bm b, bm c, bm d]
    OpDPdx a b c                   -> [bm a, bm b, bm c]
    OpDPdy a b c                   -> [bm a, bm b, bm c]
    OpFwidth a b c                 -> [bm a, bm b, bm c]
    OpDPdxFine a b c               -> [bm a, bm b, bm c]
    OpDPdyFine a b c               -> [bm a, bm b, bm c]
    OpFwidthFine a b c             -> [bm a, bm b, bm c]
    OpDPdxCoarse a b c             -> [bm a, bm b, bm c]
    OpDPdyCoarse a b c             -> [bm a, bm b, bm c]
    OpFwidthCoarse a b c           -> [bm a, bm b, bm c]
    OpPhi a b cs              -> bm a : bm b : (cs >>= (\(d,e) -> [bm d, bm e]))
    OpLoopMerge a bs               -> [bm a, bm bs]
    OpSelectionMerge a bs          -> [bm a, bm bs]
    OpLabel a                      -> [bm a]
    OpBranch a                     -> [bm a]
    OpBranchConditional a b c ds
        -> bm a : bm b : bm c : maybe [] (\(e,f) -> [e,f]) ds
    OpSwitch a b cs -> bm a : bm b : (cs >>= (\(d,e) -> [d, bm e]))
    OpKill                         -> []
    OpReturn                       -> []
    OpReturnValue a                -> [bm a]
    OpUnreachable                  -> []
    OpLifetimeStart a b            -> [bm a, b]
    OpLifetimeStop a b             -> [bm a, b]
    OpAtomicInit a b               -> [bm a, bm b]
    OpAtomicLoad a b c d es        -> [bm a, bm b, bm c, bm d, bm es]
    OpAtomicStore a b cs d         -> [bm a, bm b, bm cs, bm d]
    OpAtomicExchange a b c d es f  -> [bm a, bm b, bm c, bm d, bm es, bm f]
    OpAtomicCompareExchange a b c d es f g
        -> [bm a, bm b, bm c, bm d, bm es, bm f, bm g]
    OpAtomicCompareExchangeWeak a b c d es f g
        -> [bm a, bm b, bm c, bm d, bm es, bm f, bm g]
    OpAtomicIIncrement a b c d es  -> [bm a, bm b, bm c, bm d, bm es]
    OpAtomicIDecrement a b c d es  -> [bm a, bm b, bm c, bm d, bm es]
    OpAtomicIAdd a b c d es f      -> [bm a, bm b, bm c, bm d, bm es, bm f]
    OpAtomicISub a b c d es f      -> [bm a, bm b, bm c, bm d, bm es, bm f]
    OpAtomicUMin a b c d es f      -> [bm a, bm b, bm c, bm d, bm es, bm f]
    OpAtomicUMax a b c d es f      -> [bm a, bm b, bm c, bm d, bm es, bm f]
    OpAtomicAnd a b c d es f       -> [bm a, bm b, bm c, bm d, bm es, bm f]
    OpAtomicOr a b c d es f        -> [bm a, bm b, bm c, bm d, bm es, bm f]
    OpAtomicXor a b c d es f       -> [bm a, bm b, bm c, bm d, bm es, bm f]
    OpAtomicIMin a b c d es f      -> [bm a, bm b, bm c, bm d, bm es, bm f]
    OpAtomicIMax a b c d es f      -> [bm a, bm b, bm c, bm d, bm es, bm f]
    OpEmitVertex                   -> []
    OpEndPrimitive                 -> []
    OpEmitStreamVertex a           -> [bm a]
    OpEndStreamPrimitive a         -> [bm a]
    OpControlBarrier a             -> [bm a]
    OpMemoryBarrier a bs           -> [bm a, bm bs]
    OpAsyncGroupCopy a b c d e f g h
        -> [bm a, bm b, bm c, bm d, bm e, bm f, bm g, bm h]
    OpWaitGroupEvents a b c d e    -> [bm a, bm b, bm c, bm d, bm e]
    OpGroupAll a b c d             -> [bm a, bm b, bm c, bm d]
    OpGroupAny a b c d             -> [bm a, bm b, bm c, bm d]
    OpGroupBroadcast a b c d e     -> [bm a, bm b, bm c, bm d, bm e]
    OpGroupIAdd a b c d e          -> [bm a, bm b, bm c, bm d, bm e]
    OpGroupFAdd a b c d e          -> [bm a, bm b, bm c, bm d, bm e]
    OpGroupFMin a b c d e          -> [bm a, bm b, bm c, bm d, bm e]
    OpGroupUMin a b c d e          -> [bm a, bm b, bm c, bm d, bm e]
    OpGroupSMin a b c d e          -> [bm a, bm b, bm c, bm d, bm e]
    OpGroupFMax a b c d e          -> [bm a, bm b, bm c, bm d, bm e]
    OpGroupUMax a b c d e          -> [bm a, bm b, bm c, bm d, bm e]
    OpGroupSMax a b c d e          -> [bm a, bm b, bm c, bm d, bm e]
    OpEnqueueMarker a b c d e f    -> [bm a, bm b, bm c, bm d, bm e, bm f]
    OpEnqueueKernel a b c d e f g h i' j k l ms
        -> bm a : bm b : bm c : bm d : bm e : bm f : bm g : bm h : bm i' : bm j
                : bm k : bm l : map bm ms
    OpGetKernelNDrangeSubGroupCount a b c d -> [bm a, bm b, bm c, bm d]
    OpGetKernelNDrangeMaxSubGroupCount a b c d -> [bm a, bm b, bm c, bm d]
    OpGetKernelWorkGroupSize a b c -> [bm a, bm b, bm c]
    OpGetKernelPreferredWorkGroupSizeMultiple a b c -> [bm a, bm b, bm c]
    OpRetainEvent a                -> [bm a]
    OpReleaseEvent a               -> [bm a]
    OpCreateUserEvent a b          -> [bm a, bm b]
    OpIsValidEvent a b c           -> [bm a, bm b, bm c]
    OpSetUserEventStatus a b       -> [bm a, bm b]
    OpCaptureEventProfilingInfo a bs c -> [bm a, bm bs, bm c]
    OpGetDefaultQueue a b          -> [bm a, bm b]
    OpBuildNDRange a b c d e       -> [bm a, bm b, bm c, bm d, bm e]
    OpReadPipe a b c d             -> [bm a, bm b, bm c, bm d]
    OpWritePipe a b c d            -> [bm a, bm b, bm c, bm d]
    OpReservedReadPipe a b c d e f -> [bm a, bm b, bm c, bm d, bm e, bm f]
    OpReservedWritePipe a b c d e f -> [bm a, bm b, bm c, bm d, bm e, bm f]
    OpReserveReadPipePackets a b c d -> [bm a, bm b, bm c, bm d]
    OpReserveWritePipePackets a b c d -> [bm a, bm b, bm c, bm d]
    OpCommitReadPipe a b           -> [bm a, bm b]
    OpCommitWritePipe a b          -> [bm a, bm b]
    OpIsValidReserveId a b c       -> [bm a, bm b, bm c]
    OpGetNumPipePackets a b c      -> [bm a, bm b, bm c]
    OpGetMaxPipePackets a b c      -> [bm a, bm b, bm c]
    OpGroupReservedReadPipePackets a b c d e
        -> [bm a, bm b, bm c, bm d, bm e]
    OpGroupReservedWritePipePackets a b c d e
        -> [bm a, bm b, bm c, bm d, bm e]
    OpGroupCommitReadPipe a b c    -> [bm a, bm b, bm c]
    OpGroupCommitWritePipe a b c   -> [bm a, bm b, bm c]
  where
    bm :: BitMask a => a -> Word32
    bm = toBitMask
    bme :: BitMaskExtra a => a -> [Word32]
    bme = extrasToWords
    bw = byteStringToWords
    bmm :: BitMask a => Maybe a -> [Word32]
    bmm = maybe [] (\a -> [toBitMask a])
