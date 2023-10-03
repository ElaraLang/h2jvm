{-# LANGUAGE LambdaCase #-}

module JVM.Data.Raw.Instruction where

import Data.Binary.Put
import Data.Binary.Write (WriteBinary (writeBinary))
import Data.Word
import JVM.Data.Raw.MagicNumbers qualified as MagicNumbers
import JVM.Data.Raw.Types

data Instruction
    = AALoad
    | AAStore
    | AConstNull
    | ALoad U1
    | ALoad0
    | ALoad1
    | ALoad2
    | ALoad3
    | ANewArray ConstantPoolIndex
    | AReturn
    | ArrayLength
    | AStore U1
    | AStore0
    | AStore1
    | AStore2
    | AStore3
    | AThrow
    | BALoad
    | BAStore
    | BIPush Word8
    | CALoad
    | CAStore
    | CheckCast ConstantPoolIndex
    | D2F
    | D2I
    | D2L
    | DAdd
    | DALoad
    | DAStore
    | DCmpG
    | DCmpL
    | DConst0
    | DConst1
    | DDiv
    | DLoad
    | DLoad0
    | DLoad1
    | DLoad2
    | DLoad3
    | DMul
    | DNeg
    | DRem
    | DReturn
    | DStore
    | DStore0
    | DStore1
    | DStore2
    | DStore3
    | DSub
    | Dup
    | DupX1
    | DupX2
    | Dup2
    | Dup2X1
    | Dup2X2
    | F2D
    | F2I
    | F2L
    | FAdd
    | FALoad
    | FAStore
    | FCmpG
    | FCmpL
    | FConst0
    | FConst1
    | FConst2
    | FDiv
    | FLoad
    | FLoad0
    | FLoad1
    | FLoad2
    | FLoad3
    | FMul
    | FNeg
    | FRem
    | FReturn
    | FStore
    | FStore0
    | FStore1
    | FStore2
    | FStore3
    | FSub
    | GetField ConstantPoolIndex
    | GetStatic ConstantPoolIndex
    | Goto Word16
    | GotoW Word32
    | I2B
    | I2C
    | I2D
    | I2F
    | I2L
    | I2S
    | IAdd
    | IALoad
    | IAnd
    | IAStore
    | IConstM1
    | IConst0
    | IConst1
    | IConst2
    | IConst3
    | IConst4
    | IConst5
    | IDiv
    | -- | if_acmpeq
      IfAcmpEq Word16
    | -- | if_acmpne
      -- Branch if int comparison succeeds
      IfAcmpNe Word16
    | IfIcmpEq Word16
    | IfIcmpNe Word16
    | IfIcmpLt Word16
    | IfIcmpGe Word16
    | IfIcmpGt Word16
    | IfIcmpLe Word16
    | IfEq Word16
    | IfNe Word16
    | IfLt Word16
    | IfGe Word16
    | IfGt Word16
    | IfLe Word16
    | IfNonNull Word16
    | IfNull Word16
    | IInc Word8 Word8
    | ILoad
    | ILoad0
    | ILoad1
    | ILoad2
    | ILoad3
    | IMul
    | INeg
    | InstanceOf ConstantPoolIndex
    | InvokeDynamic ConstantPoolIndex
    | InvokeInterface ConstantPoolIndex Word8
    | InvokeSpecial ConstantPoolIndex
    | InvokeStatic ConstantPoolIndex
    | InvokeVirtual ConstantPoolIndex
    | IOr
    | IRem
    | IReturn
    | IShl
    | IShr
    | IStore
    | IStore0
    | IStore1
    | IStore2
    | IStore3
    | ISub
    | IUShr
    | IXor
    | JSR Word16
    | JSR_W Word32
    | L2D
    | L2F
    | L2I
    | LAdd
    | LALoad
    | LAnd
    | LCmp
    | LConst0
    | LConst1
    | LDC U1
    | LDC2_W ConstantPoolIndex
    | LDC_W ConstantPoolIndex
    | LDiv
    | LLoad
    | LLoad0
    | LLoad1
    | LLoad2
    | LLoad3
    | LMul
    | LNeg
    | LookupSwitch Word32 [(Word32, Word32)]
    | LOr
    | LRem
    | LReturn
    | LShl
    | LShr
    | LStore
    | LStore0
    | LStore1
    | LStore2
    | LStore3
    | LSub
    | LUShr
    | LXor
    | MonitorEnter
    | MonitorExit
    | MultiANewArray ConstantPoolIndex Word8
    | New ConstantPoolIndex
    | NewArray ArrayType
    | NOP
    | Pop
    | Pop2
    | PutField ConstantPoolIndex
    | PutStatic ConstantPoolIndex
    | Ret Word8
    | Return
    | SALoad
    | SAStore
    | SIPush Word16
    | Swap
    | TableSwitch Word32 Word32 Word32 [(Word32, Word32)]
    | Wide Word8 Word16
    deriving (Eq, Show)

putInstruction :: Instruction -> Put
putInstruction = \case
    AALoad -> putWord8 MagicNumbers.instruction_aaLoad
    AAStore -> putWord8 MagicNumbers.instruction_aaStore
    AConstNull -> putWord8 MagicNumbers.instruction_aConstNull
    ALoad n -> putWord8 MagicNumbers.instruction_aLoad *> putWord8 n
    ALoad0 -> putWord8 MagicNumbers.instruction_aLoad0
    ALoad1 -> putWord8 MagicNumbers.instruction_aLoad1
    ALoad2 -> putWord8 MagicNumbers.instruction_aLoad2
    ALoad3 -> putWord8 MagicNumbers.instruction_aLoad3
    (ANewArray index) -> putWord8 MagicNumbers.instruction_aNewArray *> putWord16be index
    AReturn -> putWord8 MagicNumbers.instruction_aReturn
    ArrayLength -> putWord8 MagicNumbers.instruction_arrayLength
    AStore n  -> putWord8 MagicNumbers.instruction_aStore *> putWord8 n
    AStore0 -> putWord8 MagicNumbers.instruction_aStore0
    AStore1 -> putWord8 MagicNumbers.instruction_aStore1
    AStore2 -> putWord8 MagicNumbers.instruction_aStore2
    AStore3 -> putWord8 MagicNumbers.instruction_aStore3
    AThrow -> putWord8 MagicNumbers.instruction_aThrow
    BALoad -> putWord8 MagicNumbers.instruction_baLoad
    BAStore -> putWord8 MagicNumbers.instruction_baStore
    (BIPush i) -> putWord8 MagicNumbers.instruction_biPush *> putWord8 i
    CALoad -> putWord8 MagicNumbers.instruction_caLoad
    CAStore -> putWord8 MagicNumbers.instruction_caStore
    (CheckCast index) -> putWord8 MagicNumbers.instruction_checkCast *> putWord16be index
    D2F -> putWord8 MagicNumbers.instruction_d2f
    D2I -> putWord8 MagicNumbers.instruction_d2i
    D2L -> putWord8 MagicNumbers.instruction_d2l
    DAdd -> putWord8 MagicNumbers.instruction_dAdd
    DALoad -> putWord8 MagicNumbers.instruction_daLoad
    DAStore -> putWord8 MagicNumbers.instruction_daStore
    DCmpG -> putWord8 MagicNumbers.instruction_dCmpG
    DCmpL -> putWord8 MagicNumbers.instruction_dCmpL
    DConst0 -> putWord8 MagicNumbers.instruction_dConst0
    DConst1 -> putWord8 MagicNumbers.instruction_dConst1
    DDiv -> putWord8 MagicNumbers.instruction_dDiv
    DLoad -> putWord8 MagicNumbers.instruction_dLoad
    DLoad0 -> putWord8 MagicNumbers.instruction_dLoad0
    DLoad1 -> putWord8 MagicNumbers.instruction_dLoad1
    DLoad2 -> putWord8 MagicNumbers.instruction_dLoad2
    DLoad3 -> putWord8 MagicNumbers.instruction_dLoad3
    DMul -> putWord8 MagicNumbers.instruction_dMul
    DNeg -> putWord8 MagicNumbers.instruction_dNeg
    DRem -> putWord8 MagicNumbers.instruction_dRem
    DReturn -> putWord8 MagicNumbers.instruction_dReturn
    DStore -> putWord8 MagicNumbers.instruction_dStore
    DStore0 -> putWord8 MagicNumbers.instruction_dStore0
    DStore1 -> putWord8 MagicNumbers.instruction_dStore1
    DStore2 -> putWord8 MagicNumbers.instruction_dStore2
    DStore3 -> putWord8 MagicNumbers.instruction_dStore3
    DSub -> putWord8 MagicNumbers.instruction_dSub
    Dup -> putWord8 MagicNumbers.instruction_dup
    DupX1 -> putWord8 MagicNumbers.instruction_dupX1
    DupX2 -> putWord8 MagicNumbers.instruction_dupX2
    Dup2 -> putWord8 MagicNumbers.instruction_dup2
    Dup2X1 -> putWord8 MagicNumbers.instruction_dup2X1
    Dup2X2 -> putWord8 MagicNumbers.instruction_dup2X2
    F2D -> putWord8 MagicNumbers.instruction_f2d
    F2I -> putWord8 MagicNumbers.instruction_f2i
    F2L -> putWord8 MagicNumbers.instruction_f2l
    FAdd -> putWord8 MagicNumbers.instruction_fAdd
    FALoad -> putWord8 MagicNumbers.instruction_faLoad
    FAStore -> putWord8 MagicNumbers.instruction_faStore
    FCmpG -> putWord8 MagicNumbers.instruction_fCmpG
    FCmpL -> putWord8 MagicNumbers.instruction_fCmpL
    FConst0 -> putWord8 MagicNumbers.instruction_fConst0
    FConst1 -> putWord8 MagicNumbers.instruction_fConst1
    FConst2 -> putWord8 MagicNumbers.instruction_fConst2
    FDiv -> putWord8 MagicNumbers.instruction_fDiv
    FLoad -> putWord8 MagicNumbers.instruction_fLoad
    FLoad0 -> putWord8 MagicNumbers.instruction_fLoad0
    FLoad1 -> putWord8 MagicNumbers.instruction_fLoad1
    FLoad2 -> putWord8 MagicNumbers.instruction_fLoad2
    FLoad3 -> putWord8 MagicNumbers.instruction_fLoad3
    FMul -> putWord8 MagicNumbers.instruction_fMul
    FNeg -> putWord8 MagicNumbers.instruction_fNeg
    FRem -> putWord8 MagicNumbers.instruction_fRem
    FReturn -> putWord8 MagicNumbers.instruction_fReturn
    FStore -> putWord8 MagicNumbers.instruction_fStore
    FStore0 -> putWord8 MagicNumbers.instruction_fStore0
    FStore1 -> putWord8 MagicNumbers.instruction_fStore1
    FStore2 -> putWord8 MagicNumbers.instruction_fStore2
    FStore3 -> putWord8 MagicNumbers.instruction_fStore3
    FSub -> putWord8 MagicNumbers.instruction_fSub
    GetField index -> putWord8 MagicNumbers.instruction_getField *> putWord16be index
    GetStatic index -> putWord8 MagicNumbers.instruction_getStatic *> putWord16be index
    Goto offset -> putWord8 MagicNumbers.instruction_goto *> putWord16be offset
    GotoW offset -> putWord8 MagicNumbers.instruction_gotoW *> putWord32be offset
    I2B -> putWord8 MagicNumbers.instruction_i2b
    I2C -> putWord8 MagicNumbers.instruction_i2c
    I2D -> putWord8 MagicNumbers.instruction_i2d
    I2F -> putWord8 MagicNumbers.instruction_i2f
    I2L -> putWord8 MagicNumbers.instruction_i2l
    I2S -> putWord8 MagicNumbers.instruction_i2s
    IAdd -> putWord8 MagicNumbers.instruction_iAdd
    IALoad -> putWord8 MagicNumbers.instruction_iALoad
    IAStore -> putWord8 MagicNumbers.instruction_iAStore
    IAnd -> putWord8 MagicNumbers.instruction_iAnd
    IConstM1 -> putWord8 MagicNumbers.instruction_iConstM1
    IConst0 -> putWord8 MagicNumbers.instruction_iConst0
    IConst1 -> putWord8 MagicNumbers.instruction_iConst1
    IConst2 -> putWord8 MagicNumbers.instruction_iConst2
    IConst3 -> putWord8 MagicNumbers.instruction_iConst3
    IConst4 -> putWord8 MagicNumbers.instruction_iConst4
    IConst5 -> putWord8 MagicNumbers.instruction_iConst5
    IDiv -> putWord8 MagicNumbers.instruction_iDiv
    IfAcmpEq offset -> putWord8 MagicNumbers.instruction_ifAcmpeq *> putWord16be offset
    IfAcmpNe offset -> putWord8 MagicNumbers.instruction_ifAcmpne *> putWord16be offset
    IfIcmpEq offset -> putWord8 MagicNumbers.instruction_ifIcmpeq *> putWord16be offset
    IfIcmpNe offset -> putWord8 MagicNumbers.instruction_ifIcmpne *> putWord16be offset
    IfIcmpLt offset -> putWord8 MagicNumbers.instruction_ifIcmplt *> putWord16be offset
    IfIcmpGe offset -> putWord8 MagicNumbers.instruction_ifIcmpge *> putWord16be offset
    IfIcmpGt offset -> putWord8 MagicNumbers.instruction_ifIcmpgt *> putWord16be offset
    IfIcmpLe offset -> putWord8 MagicNumbers.instruction_ifIcmple *> putWord16be offset
    IfEq offset -> putWord8 MagicNumbers.instruction_ifEq *> putWord16be offset
    IfNe offset -> putWord8 MagicNumbers.instruction_ifNe *> putWord16be offset
    IfLt offset -> putWord8 MagicNumbers.instruction_ifLt *> putWord16be offset
    IfGe offset -> putWord8 MagicNumbers.instruction_ifGe *> putWord16be offset
    IfGt offset -> putWord8 MagicNumbers.instruction_ifGt *> putWord16be offset
    IfLe offset -> putWord8 MagicNumbers.instruction_ifLe *> putWord16be offset
    IfNonNull offset -> putWord8 MagicNumbers.instruction_ifNonNull *> putWord16be offset
    IfNull offset -> putWord8 MagicNumbers.instruction_ifNull *> putWord16be offset
    IInc index increment -> putWord8 MagicNumbers.instruction_iInc *> putWord8 index *> putWord8 increment
    ILoad -> putWord8 MagicNumbers.instruction_iLoad
    ILoad0 -> putWord8 MagicNumbers.instruction_iLoad0
    ILoad1 -> putWord8 MagicNumbers.instruction_iLoad1
    ILoad2 -> putWord8 MagicNumbers.instruction_iLoad2
    ILoad3 -> putWord8 MagicNumbers.instruction_iLoad3
    IMul -> putWord8 MagicNumbers.instruction_iMul
    INeg -> putWord8 MagicNumbers.instruction_iNeg
    InstanceOf index -> putWord8 MagicNumbers.instruction_instanceOf *> putWord16be index
    InvokeDynamic index -> putWord8 MagicNumbers.instruction_invokeDynamic *> putWord16be index *> putWord8 0 *> putWord8 0
    InvokeInterface index count -> putWord8 MagicNumbers.instruction_invokeInterface *> putWord16be index *> putWord8 count *> putWord8 0
    InvokeSpecial index -> putWord8 MagicNumbers.instruction_invokeSpecial *> putWord16be index
    InvokeStatic index -> putWord8 MagicNumbers.instruction_invokeStatic *> putWord16be index
    InvokeVirtual index -> putWord8 MagicNumbers.instruction_invokeVirtual *> putWord16be index
    IOr -> putWord8 MagicNumbers.instruction_iOr
    IRem -> putWord8 MagicNumbers.instruction_iRem
    IReturn -> putWord8 MagicNumbers.instruction_iReturn
    IShl -> putWord8 MagicNumbers.instruction_iShl
    IShr -> putWord8 MagicNumbers.instruction_iShr
    IStore -> putWord8 MagicNumbers.instruction_iStore
    IStore0 -> putWord8 MagicNumbers.instruction_iStore0
    IStore1 -> putWord8 MagicNumbers.instruction_iStore1
    IStore2 -> putWord8 MagicNumbers.instruction_iStore2
    IStore3 -> putWord8 MagicNumbers.instruction_iStore3
    ISub -> putWord8 MagicNumbers.instruction_iSub
    IUShr -> putWord8 MagicNumbers.instruction_iUShr
    IXor -> putWord8 MagicNumbers.instruction_iXor
    JSR offset -> putWord8 MagicNumbers.instruction_jsr *> putWord16be offset
    JSR_W offset -> putWord8 MagicNumbers.instruction_jsrW *> putWord32be offset
    L2D -> putWord8 MagicNumbers.instruction_l2d
    L2F -> putWord8 MagicNumbers.instruction_l2f
    L2I -> putWord8 MagicNumbers.instruction_l2i
    LAdd -> putWord8 MagicNumbers.instruction_lAdd
    LALoad -> putWord8 MagicNumbers.instruction_lALoad
    LAnd -> putWord8 MagicNumbers.instruction_lAnd
    LCmp -> putWord8 MagicNumbers.instruction_lCmp
    LConst0 -> putWord8 MagicNumbers.instruction_lConst0
    LConst1 -> putWord8 MagicNumbers.instruction_lConst1
    LDC index -> putWord8 MagicNumbers.instruction_ldc *> putWord8 index
    LDC_W index -> putWord8 MagicNumbers.instruction_ldcW *> putWord16be index
    LDC2_W index -> putWord8 MagicNumbers.instruction_ldc2W *> putWord16be index
    LDiv -> putWord8 MagicNumbers.instruction_lDiv
    LLoad -> putWord8 MagicNumbers.instruction_lLoad
    LLoad0 -> putWord8 MagicNumbers.instruction_lLoad0
    LLoad1 -> putWord8 MagicNumbers.instruction_lLoad1
    LLoad2 -> putWord8 MagicNumbers.instruction_lLoad2
    LLoad3 -> putWord8 MagicNumbers.instruction_lLoad3
    LMul -> putWord8 MagicNumbers.instruction_lMul
    LNeg -> putWord8 MagicNumbers.instruction_lNeg
    LookupSwitch defaultOffset npairs -> undefined -- TODO: figure this out
    LOr -> putWord8 MagicNumbers.instruction_lOr
    LRem -> putWord8 MagicNumbers.instruction_lRem
    LReturn -> putWord8 MagicNumbers.instruction_lReturn
    LShl -> putWord8 MagicNumbers.instruction_lShl
    LShr -> putWord8 MagicNumbers.instruction_lShr
    LSub -> putWord8 MagicNumbers.instruction_lSub
    LStore -> putWord8 MagicNumbers.instruction_lStore
    LStore0 -> putWord8 MagicNumbers.instruction_lStore0
    LStore1 -> putWord8 MagicNumbers.instruction_lStore1
    LStore2 -> putWord8 MagicNumbers.instruction_lStore2
    LStore3 -> putWord8 MagicNumbers.instruction_lStore3
    LUShr -> putWord8 MagicNumbers.instruction_lUShr
    LXor -> putWord8 MagicNumbers.instruction_lXor
    MonitorEnter -> putWord8 MagicNumbers.instruction_monitorEnter
    MonitorExit -> putWord8 MagicNumbers.instruction_monitorExit
    MultiANewArray index dimensions -> putWord8 MagicNumbers.instruction_multiANewArray *> putWord16be index *> putWord8 dimensions
    New index -> putWord8 MagicNumbers.instruction_new *> putWord16be index
    NewArray atype -> putWord8 MagicNumbers.instruction_newArray *> putWord8 atype
    NOP -> putWord8 MagicNumbers.instruction_nop
    Pop -> putWord8 MagicNumbers.instruction_pop
    Pop2 -> putWord8 MagicNumbers.instruction_pop2
    PutField index -> putWord8 MagicNumbers.instruction_putField *> putWord16be index
    PutStatic index -> putWord8 MagicNumbers.instruction_putStatic *> putWord16be index
    Ret index -> putWord8 MagicNumbers.instruction_ret *> putWord8 index
    Return -> putWord8 MagicNumbers.instruction_return
    SALoad -> putWord8 MagicNumbers.instruction_sALoad
    SAStore -> putWord8 MagicNumbers.instruction_sAStore
    SIPush value -> putWord8 MagicNumbers.instruction_sIPush *> putWord16be value
    Swap -> putWord8 MagicNumbers.instruction_swap
    TableSwitch defaultOffset low high offsets -> undefined -- TODO: figure this out
    Wide opcode index -> putWord8 MagicNumbers.instruction_wide *> putWord8 opcode *> putWord16be index

instance WriteBinary Instruction where
    writeBinary = putInstruction
