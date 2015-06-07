{-
 - Program2.hs
 - By Steven Smith
 -}

{-# LANGUAGE OverloadedStrings, RecursiveDo #-}

module Program2 where

import SpirV.Builder

program2 :: Builder ()
program2 = mdo
    std450 <- extInstImport "GLSL.std.450"
    memoryModel Logical GLSL450
    progName <- string "program2"

    f_1 <- constantFloat 1
    f_2 <- constantFloat 2
    float32 <- typeFloat 32
    vecf4 <- typeVector float32 4
    scaleConst <- constantComposite vecf4 [f_1, f_1, f_2, f_1]

    bool <- typeBool
    cond <- variable bool StorageUniformConstant Nothing
    color <- variable vecf4 StorageOutput Nothing
    color1 <- decorate Smooth (variable vecf4 StorageInput Nothing)
    color2 <- decorate Noperspective (variable vecf4 StorageInput Nothing)

    array5vecf4 <- typeArray vecf4 5
    int32 <- typeInt 32 Signed
    structS <- typeStruct [bool, array5vecf4, int32]

    s <- variable structS StorageUniformConstant Nothing
    multiplier <- variable vecf4 StorageUniformConstant Nothing

    main' <- entryPointFunction Fragment

    label1 <- label
    scale <- variable vecf4 StorageFunction Nothing
    i <- variable int32 StorageFunction Nothing
    store scale scaleConst MemoryNormal
    ifCond <- load bool cond MemoryNormal
    selectionMerge branchEnd []
    branchConditional ifCond ifTrue ifFalse Nothing

    ifTrue <- label
    color1Val <- load vecf4 color1 MemoryNormal
    i_1 <- constantInt32 1
    i_2 <- constantInt32 2
    s_v_2 <- accessChain vecf4 StorageUniformConstant s [i_1, i_2]
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
    i_0 <- constantInt32 0
    store i i_0 MemoryNormal
    branch forStart

    forStart <- label
    iVal <- load int32 i MemoryNormal
    i_4 <- constantInt32 4
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
