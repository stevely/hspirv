{-
 - PrettyPrint.hs
 - By Steven Smith
 -}
{-# LANGUAGE OverloadedStrings #-}

module SpirV.PrettyPrint
( printModule
, prettyPrintModule
, prettyPrintBuilder
)
where

import Data.ByteString (ByteString)
import Data.List (intersperse)
import Data.Text (replace)
import Data.Text.Lazy (justifyRight)
import Data.Text.Lazy.Builder
import Data.Text.Lazy.Builder.Int
import Data.Text.Encoding
import Data.Monoid

import SpirV.Builder.Types hiding (Builder)
import SpirV.Instructions

import qualified Data.Foldable as F
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.IO as LIO

-- Helper functions

printModule :: SpirVModule -> IO ()
printModule = LIO.putStrLn . toLazyText . prettyPrintModule

prettyPrintModule :: SpirVModule -> Builder
prettyPrintModule m =
    "# Bound: "
        <> decimal (idBound m)
        <> F.foldr go mempty (instructionStream m)
  where
    go a b = "\n" <> prettyPrintBuilder a <> b

resultIdFmt :: Id -> Builder
resultIdFmt =
    (<> ": ") . fromLazyText . justifyRight 8 ' ' . toLazyText . decimal . getId

noResultIdFmt :: Builder
noResultIdFmt = fromLazyText (LT.replicate 10 " ")

idFmt :: Id -> Builder
idFmt i = "<" <> decimal (getId i) <> ">"

showFmt :: Show a => a -> Builder
showFmt = fromString . show

bsFmt :: ByteString -> Builder
bsFmt s = "\"" <> go s <> "\""
  where
    go = fromText . replace "\"" "\\\"" . replace "\\" "\\\\" . decodeUtf8

-- Some of the SPIR-V enums don't admit trivial Show instances through
-- "deriving". We provide specialized pretty-printing functions here for such
-- data types

prettyPrintDecoration :: Decoration -> Builder
prettyPrintDecoration dec = case dec of
    PrecisionLow -> "PrecisionLow"
    PrecisionMedium -> "PrecisionMedium"
    PrecisionHigh -> "PrecisionHigh"
    Block -> "Block"
    BufferBlock -> "BufferBlock"
    RowMajor -> "RowMajor"
    ColMajor -> "ColMajor"
    GLSLShared -> "GLSLShared"
    GLSLStd140 -> "GLSLStd140"
    GLSLStd430 -> "GLSLStd430"
    GLSLPacked -> "GLSLPacked"
    Smooth -> "Smooth"
    Noperspective -> "Noperspective"
    Flat -> "Flat"
    Patch -> "Patch"
    Centroid -> "Centroid"
    Sample -> "Sample"
    Invariant -> "Invariant"
    Restrict -> "Restrict"
    Aliased -> "Aliased"
    Volatile -> "Volatile"
    Constant -> "Constant"
    Coherent -> "Coherent"
    Nonwritable -> "Nonwritable"
    Nonreadable -> "Nonreadable"
    Uniform -> "Uniform"
    NoStaticUse -> "NoStaticUse"
    CPacked -> "CPacked"
    FPSaturatedConversion -> "FPSaturatedConversion"
    Stream a -> "Stream " <> showFmt a
    Location a -> "Location " <> showFmt a
    Component a -> "Component " <> showFmt a
    Index a -> "Index " <> showFmt a
    Binding a -> "Binding " <> showFmt a
    DescriptorSet a -> "DescriptorSet " <> showFmt a
    Offset a -> "Offset " <> showFmt a
    Alignment a -> "Alignment " <> showFmt a
    XfbBuffer a -> "XfbBuffer " <> showFmt a
    Stride a -> "Stride " <> showFmt a
    Built'In a -> "Built-in " <> showFmt a
    FuncParamAttr a -> "FuncParamAttr " <> showFmt a
    FPRoundingMode a -> "FPRoundingMode " <> showFmt a
    FPFastMathMode a -> "FPFastMathMode " <> showFmt a
    LinkageType a -> "LinkageType " <> showFmt a
    SpecId a -> "SpecId " <> showFmt a

prettyPrintExecutionMode :: ExecutionMode -> Builder
prettyPrintExecutionMode em = case em of
    Invocations a -> "Invocations " <> showFmt a
    SpacingEqual -> "SpacingEqual"
    SpacingFractionalEqual -> "SpacingFractionalEqual"
    SpacingFractionalOdd -> "SpacingFractionalOdd"
    VertexOrderCw -> "VertexOrderCw"
    VertexOrderCcw -> "VertexOrderCcw"
    PixelCenterImage -> "PixelCenterImage"
    OriginUpperLeft -> "OriginUpperLeft"
    EarlyFragmentTests -> "EarlyFragmentsTests"
    PointMode -> "PointMode"
    Xfb -> "Xfb"
    DepthReplacing -> "DepthReplacing"
    DepthAny -> "DepthAny"
    DepthGreater -> "DepthGreater"
    DepthLess -> "DepthLess"
    DepthUnchanged -> "DepthUnchanged"
    LocalSize x y z -> "LocalSize <" <> showFmt x <> ", " <> showFmt y <>
        ", " <> showFmt z <> ">"
    LocalSizeHint x y z -> "LocalSizeHint <" <> showFmt x <> ", " <>
        showFmt y <> ", " <> showFmt z <> ">"
    InputPoints -> "InputPoints"
    InputLines -> "InputLines"
    InputLinesAdjacency -> "InputLinesAdjacency"
    InputTriangles -> "InputTriangles"
    InputTrianglesAdjacency -> "InputTrianglesAdjacency"
    InputQuads -> "InputQuads"
    InputIsolines -> "InputIsolines"
    OutputVertices a -> "OutputVertices " <> showFmt a
    OutputPoints -> "OutputPoints"
    OutputLineStrip -> "OutputLineStrip"
    OutputTriangleStrip -> "OutputTriangleStrip"
    VecTypeHint i' -> "VecTypeHint " <> idFmt i'
    ContractionOff -> "ContractionOff"

prettyPrintMemoryAccess :: MemoryAccess -> Builder
prettyPrintMemoryAccess ma = case ma of
    MemoryNormal -> "MemoryNormal"
    MemoryVolatile -> "MemoryVolatile"
    MemoryAligned a -> "MemoryAligned " <> showFmt a
    MemoryVolatileAligned a -> "MemoryVolatileAligned " <> showFmt a

prettyPrintBuilder :: Instruction -> Builder
prettyPrintBuilder inst = case inst of
    OpNop -> nf <> "Nop"
    OpUndef a i' -> rf i' <> "Undef" <> df a
    OpSource a b -> nf <> "Source" <> sf a <> sf b
    OpSourceExtension b' -> nf <> "SourceExtension" <> bf b'
    OpName a b' -> nf <> "Name" <> df a <> bf b'
    OpMemberName a b b' -> nf <> "MemberName" <> df a <> sf b <> bf b'
    OpString i' b' -> rf i' <> "String" <> bf b'
    OpLine a b c d -> nf <> "Line" <> df a <> df b <> " (" <> showFmt c <> ", "
        <> showFmt d <> ")"
    OpDecorationGroup i' -> rf i' <> "DecorationGroup"
    OpDecorate a d' -> nf <> "Decorate" <> df a <> ppd d'
    OpMemberDecorate a b d' -> nf <> "MemberDecorate" <> df a <> sf b <> ppd d'
    OpGroupDecorate a bs -> nf <> "GroupDecorate" <> df a <> dsf bs
    OpGroupMemberDecorate a bs -> nf <> "GroupMemberDecorate" <> df a <> dsf bs
    OpExtension b' -> nf <> "Extension" <> bf b'
    OpExtInstImport i' b' -> rf i' <> "ExtInstImport" <> bf b'
    OpExtInst a i' b c ds -> rf i' <> "ExtInst" <> df a <> df b <> sf c
        <> dsf ds
    OpMemoryModel a b -> nf <> "MemoryModel" <> sf a <> sf b
    OpEntryPoint a b -> nf <> "EntryPoint" <> sf a <> df b
    OpExecutionMode a b -> nf <> "ExecutionMode" <> df a <> ppem b
    OpCompileFlag b' -> nf <> "CompileFlag" <> bf b'
    OpTypeVoid i' -> rf i' <> "TypeVoid"
    OpTypeBool i' -> rf i' <> "TypeBool"
    OpTypeInt i' a b -> rf i' <> "TypeInt" <> sf a <> sf b
    OpTypeFloat i' a -> rf i' <> "TypeFloat" <> sf a
    OpTypeVector i' a b -> rf i' <> "TypeVector" <> df a <> sf b
    OpTypeMatrix i' a b -> rf i' <> "TypeMatrix" <> df a <> sf b
    OpTypeSampler i' a b c d e f j -> rf i' <> "TypeSampler" <> df a <> sf b
        <> sf c <> sf d <> sf e <> sf f <> (mf sf) j
    OpTypeFilter i' -> rf i' <> "TypeFilter"
    OpTypeArray i' a b -> rf i' <> "TypeArray" <> df a <> df b
    OpTypeRuntimeArray i' a -> rf i' <> "TypeRuntimeArray" <> df a
    OpTypeStruct i' as -> rf i' <> "TypeStruct" <> dsf as
    OpTypeOpaque i' b' -> rf i' <> "TypeOpaque" <> bf b'
    OpTypePointer i' a b -> rf i' <> "TypePointer" <> sf a <> df b
    OpTypeFunction i' a bs -> rf i' <> "TypeFunction" <> df a <> dsf bs
    OpTypeEvent i' -> rf i' <> "TypeEvent"
    OpTypeDeviceEvent i' -> rf i' <> "TypeDeviceEvent"
    OpTypeReserveId i' -> rf i' <> "TypeReserveId"
    OpTypeQueue i' -> rf i' <> "TypeQueue"
    OpTypePipe i' a b -> rf i' <> "TypePipe" <> df a <> sf b
    OpConstantTrue a i' -> rf i' <> "ConstantTrue" <> df a
    OpConstantFalse a i' -> rf i' <> "ConstantFalse" <> df a
    OpConstant a i' bs -> rf i' <> "Constant" <> df a <> ssf bs
    OpConstantComposite a i' bs -> rf i' <> "ConstantComposite" <> df a
        <> dsf bs
    OpConstantSampler a i' b c d -> rf i' <> "ConstantSampler" <> df a <> sf b
        <> sf c <> sf d
    OpConstantNullPointer a i' -> rf i' <> "ConstantNullPointer" <> df a
    OpConstantNullObject a i' -> rf i' <> "ConstantNullObject" <> df a
    OpSpecConstantTrue a i' -> rf i' <> "SpecConstantTrue" <> df a
    OpSpecConstantFalse a i' -> rf i' <> "SpecConstantFalse" <> df a
    OpSpecConstant a i' bs -> rf i' <> "SpecConstant" <> df a <> ssf bs
    OpSpecConstantComposite a i' bs -> rf i' <> "SpecConstantComposite" <> df a
        <> dsf bs
    OpVariable a i' b j -> rf i' <> "Variable" <> df a <> sf b <> (mf df) j
    OpVariableArray a i' b c -> rf i' <> "VariableArray" <> df a <> sf b <> df c
    OpLoad a i' b c -> rf i' <> "Load" <> df a <> df b <> ppma c
    OpStore a b c -> nf <> "Store" <> df a <> df b <> ppma c
    OpCopyMemory a b c -> nf <> "CopyMemory" <> df a <> df b <> ppma c
    OpCopyMemorySized a b c d -> nf <> "CopyMemorySized" <> df a <> df b <> df c
        <> ppma d
    OpAccessChain a i' b cs -> rf i' <> "AccessChain" <> df a <> df b <> dsf cs
    OpInBoundsAccessChain a i' b cs -> rf i' <> "InBoundsAccessChain" <> df a
        <> df b <> dsf cs
    OpArrayLength a i' b c -> rf i' <> "ArrayLength" <> df a <> df b <> sf c
    OpImagePointer a i' b c d -> rf i' <> "ImagePointer" <> df a <> df b <> df c
        <> df d
    OpGenericPtrMemSemantics a i' b -> rf i' <> "GenericPtrMemSemantics" <> df a
        <> df b
    OpFunction a i' b c -> rf i' <> "Function" <> df a <> sf b <> df c
    OpFunctionParameter a i' -> rf i' <> "FunctionParameter" <> df a
    OpFunctionEnd -> nf <> "FunctionEnd"
    OpFunctionCall a i' b cs -> rf i' <> "FunctionCall" <> df a <> df b
        <> dsf cs
    OpSampler a i' b c -> rf i' <> "Sampler" <> df a <> df b <> df c
    OpTextureSample a i' b c j -> rf i' <> "TextureSampler" <> df a <> df b
        <> df c <> (mf df) j
    OpTextureSampleDref a i' b c d -> rf i' <> "TextureSampleDref" <> df a
        <> df b <> df c <> df d
    OpTextureSampleLod a i' b c d -> rf i' <> "TextureSampleLod" <> df a <> df b
        <> df c <> df d
    OpTextureSampleProj a i' b c j -> rf i' <> "TextureSampleProj" <> df a
        <> df b <> df c <> (mf df) j
    OpTextureSampleGrad a i' b c d e -> rf i' <> "TextureSampleGrad" <> df a
        <> df b <> df c <> df d <> df e
    OpTextureSampleOffset a i' b c d j -> rf i' <> "TextureSampleOffset" <> df a
        <> df b <> df c <> df d <> (mf df) j
    OpTextureSampleProjLod a i' b c d -> rf i' <> "TextureSampleProjLod" <> df a
        <> df b <> df c <> df d
    OpTextureSampleProjGrad a i' b c d e -> rf i' <> "TextureSampleProjGrad"
        <> df a <> df b <> df c <> df d <> df e
    OpTextureSampleLodOffset a i' b c d e -> rf i' <> "TextureSampleLodOffset"
        <> df a <> df b <> df c <> df d <> df e
    OpTextureSampleProjOffset a i' b c d j -> rf i' <> "TextureSampleProjOffset"
        <> df a <> df b <> df c <> df d <> (mf df) j
    OpTextureSampleGradOffset a i' b c d e f -> rf i'
        <> "TextureSampleGradOffset" <> df a <> df b <> df c <> df d <> df e
        <> df f
    OpTextureSampleProjLodOffset a i' b c d e -> rf i'
        <> "TextureSampleProjLodOffset" <> df a <> df b <> df c <> df d <> df e
    OpTextureSampleProjGradOffset a i' b c d e f -> rf i'
        <> "TextureSampleProjGradOffset" <> df a <> df b <> df c <> df d <> df e
        <> df f
    OpTextureFetchTexelLod a i' b c d -> rf i' <> "TextureFetchTexelLod" <> df a
        <> df b <> df c <> df d
    OpTextureFetchTexelOffset a i' b c d -> rf i' <> "TextureFetchTexelOffset"
        <> df a <> df b <> df c <> df d
    OpTextureFetchTexelSample a i' b c d -> rf i' <> "TextureFetchTexelSample"
        <> df a <> df b <> df c <> df d
    OpTextureFetchTexel a i' b c -> rf i' <> "TextureFetchTexel" <> df a <> df b
        <> df c
    OpTextureGather a i' b c d -> rf i' <> "TextureGather" <> df a <> df b
        <> df c <> df d
    OpTextureGatherOffset a i' b c d e -> rf i' <> "TextureGatherOffset" <> df a
        <> df b <> df c <> df d <> df e
    OpTextureGatherOffsets a i' b c d e -> rf i' <> "TextureGatherOffsets"
        <> df a <> df b <> df c <> df d <> df e
    OpTextureQuerySizeLod a i' b c -> rf i' <> "TextureQuerySizeLod" <> df a
        <> df b <> df c
    OpTextureQuerySize a i' b -> rf i' <> "TextureQuerySize" <> df a <> df b
    OpTextureQueryLod a i' b c -> rf i' <> "TextureQueryLod" <> df a <> df b
        <> df c
    OpTextureQueryLevels a i' b -> rf i' <> "TextureQueryLevels" <> df a <> df b
    OpTextureQuerySamples a i' b -> rf i' <> "TextureQuerySamples" <> df a
        <> df b
    OpConvertFToU a i' b -> rf i' <> "ConvertFToU" <> df a <> df b
    OpConvertFToS a i' b -> rf i' <> "ConvertFToS" <> df a <> df b
    OpConvertSToF a i' b -> rf i' <> "ConvertSToF" <> df a <> df b
    OpConvertUToF a i' b -> rf i' <> "ConvertUToF" <> df a <> df b
    OpUConvert a i' b -> rf i' <> "UConvert" <> df a <> df b
    OpSConvert a i' b -> rf i' <> "SConvert" <> df a <> df b
    OpFConvert a i' b -> rf i' <> "FConvert" <> df a <> df b
    OpConvertPtrToU a i' b -> rf i' <> "ConvertPtrToU" <> df a <> df b
    OpConvertUToPtr a i' b -> rf i' <> "ConvertUToPtr" <> df a <> df b
    OpPtrCastToGeneric a i' b -> rf i' <> "PtrCastToGeneric" <> df a <> df b
    OpGenericCastToPtr a i' b -> rf i' <> "GenericCastToPtr" <> df a <> df b
    OpBitcast a i' b -> rf i' <> "Bitcast" <> df a <> df b
    OpGenericCastToPtrExplicit a i' b c -> rf i' <> "GenericCastToPtrExplicit"
        <> df a <> df b <> sf c
    OpSatConvertSToU a i' b -> rf i' <> "SatConvertSToU" <> df a <> df b
    OpSatConvertUToS a i' b -> rf i' <> "SatConvertUToS" <> df a <> df b
    OpVectorExtractDynamic a i' b c -> rf i' <> "VectorExtractDynamic" <> df a
        <> df b <> df c
    OpVectorInsertDynamic a i' b c d -> rf i' <> "VectorInsertDynamic" <> df a
        <> df b <> df c <> df d
    OpVectorShuffle a i' b c ds -> rf i' <> "VectorShuffle" <> df a <> df b
        <> df c <> ssf ds
    OpCompositeConstruct a i' bs -> rf i' <> "CompositeConstruct" <> df a
        <> dsf bs
    OpCompositeExtract a i' b cs -> rf i' <> "CompositeExtract" <> df a <> df b
        <> ssf cs
    OpCompositeInsert a i' b c ds -> rf i' <> "CompositeInsert" <> df a <> df b
        <> df c <> ssf ds
    OpCopyObject a i' b -> rf i' <> "CopyObject" <> df a <> df b
    OpTranspose a i' b -> rf i' <> "Transpose" <> df a <> df b
    OpSNegate a i' b -> rf i' <> "SNegate" <> df a <> df b
    OpFNegate a i' b -> rf i' <> "FNegate" <> df a <> df b
    OpNot a i' b -> rf i' <> "Not" <> df a <> df b
    OpIAdd a i' b c -> rf i' <> "IAdd" <> df a <> df b <> df c
    OpFAdd a i' b c -> rf i' <> "FAdd" <> df a <> df b <> df c
    OpISub a i' b c -> rf i' <> "ISub" <> df a <> df b <> df c
    OpFSub a i' b c -> rf i' <> "FSub" <> df a <> df b <> df c
    OpIMul a i' b c -> rf i' <> "IMul" <> df a <> df b <> df c
    OpFMul a i' b c -> rf i' <> "FMul" <> df a <> df b <> df c
    OpUDiv a i' b c -> rf i' <> "UDiv" <> df a <> df b <> df c
    OpSDiv a i' b c -> rf i' <> "SDiv" <> df a <> df b <> df c
    OpFDiv a i' b c -> rf i' <> "FDiv" <> df a <> df b <> df c
    OpUMod a i' b c -> rf i' <> "UMod" <> df a <> df b <> df c
    OpSRem a i' b c -> rf i' <> "SRem" <> df a <> df b <> df c
    OpSMod a i' b c -> rf i' <> "SMod" <> df a <> df b <> df c
    OpFRem a i' b c -> rf i' <> "FRem" <> df a <> df b <> df c
    OpFMod a i' b c -> rf i' <> "FMod" <> df a <> df b <> df c
    OpVectorTimesScalar a i' b c -> rf i' <> "VectorTimesScalar" <> df a <> df b
        <> df c
    OpMatrixTimesScalar a i' b c -> rf i' <> "MatrixTimesScalar" <> df a <> df b
        <> df c
    OpVectorTimesMatrix a i' b c -> rf i' <> "VectorTimesMatrix" <> df a <> df b
        <> df c
    OpMatrixTimesVector a i' b c -> rf i' <> "MatrixTimesVector" <> df a <> df b
        <> df c
    OpMatrixTimesMatrix a i' b c -> rf i' <> "MatrixTimesMatrix" <> df a <> df b
        <> df c
    OpOuterProduct a i' b c -> rf i' <> "OuterProduct" <> df a <> df b <> df c
    OpDot a i' b c -> rf i' <> "Dot" <> df a <> df b <> df c
    OpShiftRightLogical a i' b c -> rf i' <> "ShiftRightLogical" <> df a <> df b
        <> df c
    OpShiftRightArithmetic a i' b c -> rf i' <> "ShiftRightArithmetic" <> df a
        <> df b <> df c
    OpShiftLeftLogical a i' b c -> rf i' <> "ShiftLeftLogical" <> df a <> df b
        <> df c
    OpBitwiseOr a i' b c -> rf i' <> "BitwiseOr" <> df a <> df b <> df c
    OpBitwiseXor a i' b c -> rf i' <> "BitwiseXor" <> df a <> df b <> df c
    OpBitwiseAnd a i' b c -> rf i' <> "BitwiseAnd" <> df a <> df b <> df c
    OpAny a i' b -> rf i' <> "Any" <> df a <> df b
    OpAll a i' b -> rf i' <> "All" <> df a <> df b
    OpIsNaN a i' b -> rf i' <> "IsNaN" <> df a <> df b
    OpIsInf a i' b -> rf i' <> "IsInf" <> df a <> df b
    OpIsFinite a i' b -> rf i' <> "IsFinite" <> df a <> df b
    OpIsNormal a i' b -> rf i' <> "IsNormal" <> df a <> df b
    OpSignBitSet a i' b -> rf i' <> "SignBitSet" <> df a <> df b
    OpLessOrGreater a i' b c -> rf i' <> "LessOrGreater" <> df a <> df b <> df c
    OpOrdered a i' b c -> rf i' <> "Ordered" <> df a <> df b <> df c
    OpUnordered a i' b c -> rf i' <> "Unordered" <> df a <> df b <> df c
    OpLogicalOr a i' b c -> rf i' <> "LogicalOr" <> df a <> df b <> df c
    OpLogicalXor a i' b c -> rf i' <> "LogicalXor" <> df a <> df b <> df c
    OpLogicalAnd a i' b c -> rf i' <> "LogicalAnd" <> df a <> df b <> df c
    OpSelect a i' b c d -> rf i' <> "Select" <> df a <> df b <> df c <> df d
    OpIEqual a i' b c -> rf i' <> "IEqual" <> df a <> df b <> df c
    OpFOrdEqual a i' b c -> rf i' <> "FOrdEqual" <> df a <> df b <> df c
    OpFUnordEqual a i' b c -> rf i' <> "FUnordEqual" <> df a <> df b <> df c
    OpINotEqual a i' b c -> rf i' <> "INotEqual" <> df a <> df b <> df c
    OpFOrdNotEqual a i' b c -> rf i' <> "FOrdNotEqual" <> df a <> df b <> df c
    OpFUnordNotEqual a i' b c -> rf i' <> "FUnordNotEqual" <> df a <> df b
        <> df c
    OpULessThan a i' b c -> rf i' <> "ULessThan" <> df a <> df b <> df c
    OpSLessThan a i' b c -> rf i' <> "SLessThan" <> df a <> df b <> df c
    OpFOrdLessThan a i' b c -> rf i' <> "FOrdLessThan" <> df a <> df b <> df c
    OpFUnordLessThan a i' b c -> rf i' <> "FUnordLessThan" <> df a <> df b
        <> df c
    OpUGreaterThan a i' b c -> rf i' <> "UGreaterThan" <> df a <> df b <> df c
    OpSGreaterThan a i' b c -> rf i' <> "SGreaterThan" <> df a <> df b <> df c
    OpFOrdGreaterThan a i' b c -> rf i' <> "FOrdGreaterThan" <> df a <> df b
        <> df c
    OpFUnordGreaterThan a i' b c -> rf i' <> "FUnordGreaterThan" <> df a <> df b
        <> df c
    OpULessThanEqual a i' b c -> rf i' <> "ULessThanEqual" <> df a <> df b
        <> df c
    OpSLessThanEqual a i' b c -> rf i' <> "SLessThanEqual" <> df a <> df b
        <> df c
    OpFOrdLessThanEqual a i' b c -> rf i' <> "FOrdLessThanEqual" <> df a <> df b
        <> df c
    OpFUnordLessThanEqual a i' b c -> rf i' <> "FUnordLessThanEqual" <> df a
        <> df b <> df c
    OpUGreaterThanEqual a i' b c -> rf i' <> "UGreaterThanEqual" <> df a <> df b
        <> df c
    OpSGreaterThanEqual a i' b c -> rf i' <> "SGreaterThanEqual" <> df a <> df b
        <> df c
    OpFOrdGreaterThanEqual a i' b c -> rf i' <> "FOrdGreaterThanEqual" <> df a
        <> df b <> df c
    OpFUnordGreaterThanEqual a i' b c -> rf i' <> "FUnordGreaterThanEqual"
        <> df a <> df b <> df c
    OpDPdx a i' b -> rf i' <> "DPdx" <> df a <> df b
    OpDPdy a i' b -> rf i' <> "DPdy" <> df a <> df b
    OpFwidth a i' b -> rf i' <> "Fwidth" <> df a <> df b
    OpDPdxFine a i' b -> rf i' <> "DPdxFine" <> df a <> df b
    OpDPdyFine a i' b -> rf i' <> "DPdyFine" <> df a <> df b
    OpFwidthFine a i' b -> rf i' <> "FwidthFine" <> df a <> df b
    OpDPdxCoarse a i' b -> rf i' <> "DPdxCoarse" <> df a <> df b
    OpDPdyCoarse a i' b -> rf i' <> "DPdyCoarse" <> df a <> df b
    OpFwidthCoarse a i' b -> rf i' <> "FwidthCoarse" <> df a <> df b
    OpPhi a i' bs -> rf i' <> "Phi" <> df a <> (lf (pf df df)) bs
    OpLoopMerge a b -> nf <> "LoopMerge" <> df a <> sf b
    OpSelectionMerge a b -> nf <> "SelectionMerge" <> df a <> sf b
    OpLabel i' -> rf i' <> "Label"
    OpBranch a -> nf <> "Branch" <> df a
    OpBranchConditional a b c j -> nf <> "BranchConditional" <> df a <> df b
        <> df c <> (mf (pf sf sf)) j
    OpSwitch a b cs -> nf <> "Switch" <> df a <> df b <> (lf (pf sf df)) cs
    OpKill -> nf <> "Kill"
    OpReturn -> nf <> "Return"
    OpReturnValue a -> nf <> "ReturnValue" <> df a
    OpUnreachable -> nf <> "Unreachable"
    OpLifetimeStart a b -> nf <> "LifetimeStart" <> df a <> sf b
    OpLifetimeStop a b -> nf <> "LifetimeStop" <> df a <> sf b
    OpAtomicInit a b -> nf <> "AtomicInit" <> df a <> df b
    OpAtomicLoad a i' b c d -> rf i' <> "AtomicLoad" <> df a <> df b <> sf c
        <> sf d
    OpAtomicStore a b c d -> nf <> "AtomicStore" <> df a <> sf b <> sf c <> df d
    OpAtomicExchange a i' b c d e -> rf i' <> "AtomicExchange" <> df a <> df b
        <> sf c <> sf d <> df e
    OpAtomicCompareExchange a i' b c d e f -> rf i' <> "AtomicCompareExchange"
        <> df a <> df b <> sf c <> sf d <> df e <> df f
    OpAtomicCompareExchangeWeak a i' b c d e f -> rf i' <>
        "AtomicCompareExchangeWeak" <> df a <> df b <> sf c <> sf d <> df e
        <> df f
    OpAtomicIIncrement a i' b c d -> rf i' <> "AtomicIIncrement" <> df a <> df b
        <> sf c <> sf d
    OpAtomicIDecrement a i' b c d -> rf i' <> "AtomicIDecrement" <> df a <> df b
        <> sf c <> sf d
    OpAtomicIAdd a i' b c d e -> rf i' <> "AtomicIAdd" <> df a <> df b <> sf c
        <> sf d <> df e
    OpAtomicISub a i' b c d e -> rf i' <> "AtomicISub" <> df a <> df b <> sf c
        <> sf d <> df e
    OpAtomicUMin a i' b c d e -> rf i' <> "AtomicUMin" <> df a <> df b <> sf c
        <> sf d <> df e
    OpAtomicUMax a i' b c d e -> rf i' <> "AtomicUMax" <> df a <> df b <> sf c
        <> sf d <> df e
    OpAtomicAnd a i' b c d e -> rf i' <> "AtomicAnd" <> df a <> df b <> sf c
        <> sf d <> df e
    OpAtomicOr a i' b c d e -> rf i' <> "AtomicOr" <> df a <> df b <> sf c
        <> sf d <> df e
    OpAtomicXor a i' b c d e -> rf i' <> "AtomicXor" <> df a <> df b <> sf c
        <> sf d <> df e
    OpAtomicIMin a i' b c d e -> rf i' <> "AtomicIMin" <> df a <> df b <> sf c
        <> sf d <> df e
    OpAtomicIMax a i' b c d e -> rf i' <> "AtomicIMax" <> df a <> df b <> sf c
        <> sf d <> df e
    OpEmitVertex -> nf <> "EmitVertex"
    OpEndPrimitive -> nf <> "EndPrimitive"
    OpEmitStreamVertex a -> nf <> "EmitStreamVertex" <> df a
    OpEndStreamPrimitive a -> nf <> "EndStreamPrimitive" <> df a
    OpControlBarrier a -> nf <> "ControlBarrier" <> sf a
    OpMemoryBarrier a b -> nf <> "MemoryBarrier" <> sf a <> sf b
    OpAsyncGroupCopy a i' b c d e f g -> rf i' <> "AsyncGroupCopy" <> df a
        <> sf b <> df c <> df d <> df e <> df f <> df g
    OpWaitGroupEvents a i' b c d -> rf i' <> "WaitGroupEvents" <> df a <> sf b
        <> df c <> df d
    OpGroupAll a i' b c -> rf i' <> "GroupAll" <> df a <> sf b <> df c
    OpGroupAny a i' b c -> rf i' <> "GroupAny" <> df a <> sf b <> df c
    OpGroupBroadcast a i' b c d -> rf i' <> "GroupBroadcast" <> df a <> sf b
        <> df c <> df d
    OpGroupIAdd a i' b c d -> rf i' <> "GroupIAdd" <> df a <> sf b <> sf c
        <> df d
    OpGroupFAdd a i' b c d -> rf i' <> "GroupFAdd" <> df a <> sf b <> sf c
        <> df d
    OpGroupFMin a i' b c d -> rf i' <> "GroupFMin" <> df a <> sf b <> sf c
        <> df d
    OpGroupUMin a i' b c d -> rf i' <> "GroupUMin" <> df a <> sf b <> sf c
        <> df d
    OpGroupSMin a i' b c d -> rf i' <> "GroupSMin" <> df a <> sf b <> sf c
        <> df d
    OpGroupFMax a i' b c d -> rf i' <> "GroupFMax" <> df a <> sf b <> sf c
        <> df d
    OpGroupUMax a i' b c d -> rf i' <> "GroupUMax" <> df a <> sf b <> sf c
        <> df d
    OpGroupSMax a i' b c d -> rf i' <> "GroupSMax" <> df a <> sf b <> sf c
        <> df d
    OpEnqueueMarker a i' b c d e -> rf i' <> "EnqueueMarker" <> df a <> df b
        <> df c <> df d <> df e
    OpEnqueueKernel a i' b c d e f g h i j k ls -> rf i' <> "EnqueueKernel"
        <> df a <> df b <> sf c <> df d <> df e <> df f <> df g <> df h <> df i
        <> df j <> df k <> dsf ls
    OpGetKernelNDrangeSubGroupCount a i' b c -> rf i'
        <> "GetKernelNDrangeSubGroupCount" <> df a <> df b <> df c
    OpGetKernelNDrangeMaxSubGroupCount a i' b c -> rf i'
        <> "GetKernelNDrangeMaxSubGroupCount" <> df a <> df b <> df c
    OpGetKernelWorkGroupSize a i' b -> rf i' <> "GetKernelWorkGroupSize" <> df a
        <> df b
    OpGetKernelPreferredWorkGroupSizeMultiple a i' b -> rf i'
        <> "GetKernelPreferredWorkGroupSizeMultiple" <> df a <> df b
    OpRetainEvent a -> nf <> "RetainEvent" <> df a
    OpReleaseEvent a -> nf <> "ReleaseEvent" <> df a
    OpCreateUserEvent a i' -> rf i' <> "CreateUserEvent" <> df a
    OpIsValidEvent a i' b -> rf i' <> "IsValidEvent" <> df a <> df b
    OpSetUserEventStatus a b -> nf <> "SetUserEventStatus" <> df a <> df b
    OpCaptureEventProfilingInfo a b c -> nf <> "CaptureEventProfilingInfo"
        <> df a <> sf b <> df c
    OpGetDefaultQueue a i' -> rf i' <> "GetDefaultQueue" <> df a
    OpBuildNDRange a i' b c d -> rf i' <> "BuildNDRange" <> df a <> df b <> df c
        <> df d
    OpReadPipe a i' b c -> rf i' <> "ReadPipe" <> df a <> df b <> df c
    OpWritePipe a i' b c -> rf i' <> "WritePipe" <> df a <> df b <> df c
    OpReservedReadPipe a i' b c d e -> rf i' <> "ReservedReadPipe" <> df a <> df b
        <> df c <> df d <> df e
    OpReservedWritePipe a i' b c d e -> rf i' <> "ReservedWritePipe" <> df a
        <> df b <> df c <> df d <> df e
    OpReserveReadPipePackets a i' b c -> rf i' <> "ReserveReadPipePackets"
        <> df a <> df b <> df c
    OpReserveWritePipePackets a i' b c -> rf i' <> "ReservedWritePipePackets"
        <> df a <> df b <> df c
    OpCommitReadPipe a b -> nf <> "CommitReadPipe" <> df a <> df b
    OpCommitWritePipe a b -> nf <> "CommitWritePipe" <> df a <> df b
    OpIsValidReserveId a i' b -> rf i' <> "IsValidReserveId" <> df a <> df b
    OpGetNumPipePackets a i' b -> rf i' <> "GetNumPipePackets" <> df a <> df b
    OpGetMaxPipePackets a i' b -> rf i' <> "GetMaxPipePackets" <> df a <> df b
    OpGroupReservedReadPipePackets a i' b c d -> rf i'
        <> "GroupReservedReadPipePackets" <> df a <> sf b <> df c <> df d
    OpGroupReservedWritePipePackets a i' b c d -> rf i'
        <> "GroupReservedWritePipePackets" <> df a <> sf b <> df c <> df d
    OpGroupCommitReadPipe a b c -> nf <> "GroupCommitReadPipe" <> sf a <> df b
        <> df c
    OpGroupCommitWritePipe a b c -> nf <> "GroupCommitWritePipe" <> sf a <> df b
        <> df c
  where
    rf = resultIdFmt
    nf = noResultIdFmt
    df = (" " <>) . idFmt
    lf f is = " [" <> foldr (<>) "]" (intersperse ", " . map f $ is)
    dsf = lf idFmt
    -- This gets the monomorphization treatment without an explicit type sig
    sf :: Show a => a -> Builder
    sf = (" " <>) . showFmt
    ssf :: Show a => [a] -> Builder
    ssf = lf showFmt
    bf = (" " <>) . bsFmt
    mf _ Nothing = mempty
    mf f (Just a) = " " <> f a
    pf f g p = " (" <> f (fst p) <> ", " <> g (snd p) <> ")"
    ppd = (" " <>) . prettyPrintDecoration
    ppem = (" " <>) . prettyPrintExecutionMode
    ppma = (" " <>) . prettyPrintMemoryAccess
