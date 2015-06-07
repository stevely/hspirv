{-
 - Program1.hs
 - By Steven Smith
 -}

{-# LANGUAGE OverloadedStrings, RecursiveDo #-}

module Program1 where

import SpirV.Builder.Raw

program1 :: Builder ()
program1 = mdo
    std450 <- extInstImport "GLSL.std.450"
    memoryModel Logical GLSL450
    progName <- string "program1"

    float32 <- typeFloat 32
    f_1 <- constant float32 [1065353216]
    f_2 <- constant float32 [1073741824]
    vecf4 <- typeVector float32 4
    scaleConst <- constantComposite vecf4 [f_1, f_1, f_2, f_1]

    bool <- typeBool
    ptrBool <- typePointer StorageUniformConstant bool
    cond <- variable ptrBool StorageUniformConstant Nothing

    ptrVecf4Out <- typePointer StorageOutput vecf4
    color <- variable ptrVecf4Out StorageOutput Nothing

    ptrVecf4In <- typePointer StorageInput vecf4
    color1 <- variable ptrVecf4In StorageInput Nothing
    decorate color1 Smooth

    uint32 <- typeInt 32 Unsigned
    ui_5 <- constant uint32 [5]
    array5vecf4 <- typeArray vecf4 ui_5
    int32 <- typeInt 32 Signed
    structS <- typeStruct [bool, array5vecf4, int32]
    ptrStructS <- typePointer StorageUniformConstant structS
    s <- variable ptrStructS StorageUniformConstant Nothing

    color2 <- variable ptrVecf4In StorageInput Nothing
    decorate color2 Noperspective

    ptrVecf4Uni <- typePointer StorageUniformConstant vecf4
    multiplier <- variable ptrVecf4Uni StorageUniformConstant Nothing

    void <- typeVoid
    fnVoid <- typeFunction void []
    main' <- function void [] fnVoid
    entryPoint Fragment main'

    label1 <- label
    ptrVecf4Fn <- typePointer StorageFunction vecf4
    scale <- variable ptrVecf4Fn StorageFunction Nothing
    ptrIntFn <- typePointer StorageFunction int32
    i <- variable ptrIntFn StorageFunction Nothing
    store scale scaleConst MemoryNormal
    ifCond <- load bool cond MemoryNormal
    selectionMerge branchEnd []
    branchConditional ifCond ifTrue ifFalse Nothing

    ifTrue <- label
    color1Val <- load vecf4 color1 MemoryNormal
    i_1 <- constant int32 [1]
    i_2 <- constant int32 [2]
    s_v_2 <- accessChain ptrVecf4Uni s [i_1, i_2]
    v <- load vecf4 s_v_2 MemoryNormal
    color' <- fAdd vecf4 color1Val v
    store color color' MemoryNormal
    branch branchEnd

    ifFalse <- label
    color2Val <- load vecf4 color2 MemoryNormal
    sqrtColor2 <- extInst vecf4 std450 28 [color2Val]
    scaleVal <- load vecf4 scale MemoryNormal
    color'' <- fMul vecf4 sqrtColor2 scaleVal
    store color color'' MemoryNormal
    branch branchEnd

    branchEnd <- label
    i_0 <- constant int32 [0]
    store i i_0 MemoryNormal
    branch forStart

    forStart <- label
    iVal <- load int32 i MemoryNormal
    i_4 <- constant int32 [4]
    forCond <- sLessThan bool iVal i_4
    loopMerge forEnd []
    branchConditional forCond forBody forEnd Nothing

    forBody <- label
    multiplierVal <- load vecf4 multiplier MemoryNormal
    colorVal <- load vecf4 color MemoryNormal
    color''' <- fMul vecf4 multiplierVal colorVal
    store color color''' MemoryNormal
    iVal' <- load int32 i MemoryNormal
    iInc <- iAdd int32 iVal' i_1
    store i iInc MemoryNormal
    branch forStart

    forEnd <- label
    branch mainEnd

    mainEnd <- label
    return_
    functionEnd

