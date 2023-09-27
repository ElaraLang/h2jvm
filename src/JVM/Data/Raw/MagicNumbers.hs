{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}
-- We want to match the JVM spec here

-- | This module contains all the magic numbers used in the JVM class file format.
module JVM.Data.Raw.MagicNumbers where

import Data.Binary (Word16, Word32, Word8)

-- | Magic number for all class files
classMagic :: Word32
classMagic = 0xCAFEBABE

--
-- Access Flags
--
accessFlag_PUBLIC :: Word16
accessFlag_PUBLIC = 0x0001

accessFlag_PRIVATE :: Word16
accessFlag_PRIVATE = 0x0002

accessFlag_PROTECTED :: Word16
accessFlag_PROTECTED = 0x0004

accessFlag_STATIC :: Word16
accessFlag_STATIC = 0x0008

accessFlag_FINAL :: Word16
accessFlag_FINAL = 0x0010

accessFlag_VOLATILE :: Word16
accessFlag_VOLATILE = 0x0040

accessFlag_TRANSIENT :: Word16
accessFlag_TRANSIENT = 0x0080

accessFlag_SUPER :: Word16
accessFlag_SUPER = 0x0020

accessFlag_INTERFACE :: Word16
accessFlag_INTERFACE = 0x0200

accessFlag_ABSTRACT :: Word16
accessFlag_ABSTRACT = 0x0400

accessFlag_SYNTHETIC :: Word16
accessFlag_SYNTHETIC = 0x1000

accessFlag_ANNOTATION :: Word16
accessFlag_ANNOTATION = 0x2000

accessFlag_ENUM :: Word16
accessFlag_ENUM = 0x4000

accessFlag_SYNCHRONIZED :: Word16
accessFlag_SYNCHRONIZED = 0x0020

accessFlag_BRIDGE :: Word16
accessFlag_BRIDGE = 0x0040

accessFlag_VARARGS :: Word16
accessFlag_VARARGS = 0x0080

accessFlag_NATIVE :: Word16
accessFlag_NATIVE = 0x0100

accessFlag_STRICT :: Word16
accessFlag_STRICT = 0x0800

-- 
-- Method Handle Kinds
-- 

_REF_getField :: Word8
_REF_getField = 1

_REF_getStatic :: Word8
_REF_getStatic = 2

_REF_putField :: Word8
_REF_putField = 3

_REF_putStatic :: Word8
_REF_putStatic = 4

_REF_invokeVirtual :: Word8
_REF_invokeVirtual = 5

_REF_invokeStatic :: Word8
_REF_invokeStatic = 6

_REF_invokeSpecial :: Word8
_REF_invokeSpecial = 7

_REF_newInvokeSpecial :: Word8
_REF_newInvokeSpecial = 8

_REF_invokeInterface :: Word8
_REF_invokeInterface = 9



--
-- Constant Pool Tags
--

constant_Class :: Word8
constant_Class = 7

constant_Fieldref :: Word8
constant_Fieldref = 9

constant_Methodref :: Word8
constant_Methodref = 10

constant_InterfaceMethodref :: Word8
constant_InterfaceMethodref = 11

constant_String :: Word8
constant_String = 8

constant_Integer :: Word8
constant_Integer = 3

constant_Float :: Word8
constant_Float = 4

constant_Long :: Word8
constant_Long = 5

constant_Double :: Word8
constant_Double = 6

constant_NameAndType :: Word8
constant_NameAndType = 12

constant_Utf8 :: Word8
constant_Utf8 = 1

constant_MethodHandle :: Word8
constant_MethodHandle = 15

constant_MethodType :: Word8
constant_MethodType = 16

constant_InvokeDynamic :: Word8
constant_InvokeDynamic = 18

--
-- Instructions
--

instruction_aaLoad :: Word8
instruction_aaLoad = 50

instruction_aaStore :: Word8
instruction_aaStore = 83

instruction_aConstNull :: Word8
instruction_aConstNull = 1

instruction_aLoad :: Word8
instruction_aLoad = 25

instruction_aLoad0 :: Word8
instruction_aLoad0 = 42

instruction_aLoad1 :: Word8
instruction_aLoad1 = 43

instruction_aLoad2 :: Word8
instruction_aLoad2 = 44

instruction_aLoad3 :: Word8
instruction_aLoad3 = 45

instruction_aNewArray :: Word8
instruction_aNewArray = 189

instruction_aReturn :: Word8
instruction_aReturn = 176

instruction_aStore :: Word8
instruction_aStore = 58

instruction_aStore0 :: Word8
instruction_aStore0 = 75

instruction_aStore1 :: Word8
instruction_aStore1 = 76

instruction_aStore2 :: Word8
instruction_aStore2 = 77

instruction_aStore3 :: Word8
instruction_aStore3 = 78

instruction_arrayLength :: Word8
instruction_arrayLength = 190

instruction_aThrow :: Word8
instruction_aThrow = 191

instruction_baLoad :: Word8
instruction_baLoad = 51

instruction_baStore :: Word8
instruction_baStore = 84

instruction_biPush :: Word8
instruction_biPush = 16

instruction_breakpoint :: Word8
instruction_breakpoint = 202

instruction_caLoad :: Word8
instruction_caLoad = 52

instruction_caStore :: Word8
instruction_caStore = 85

instruction_checkCast :: Word8
instruction_checkCast = 192

instruction_d2f :: Word8
instruction_d2f = 144

instruction_d2i :: Word8
instruction_d2i = 142

instruction_d2l :: Word8
instruction_d2l = 143

instruction_dAdd :: Word8
instruction_dAdd = 99

instruction_daLoad :: Word8
instruction_daLoad = 49

instruction_daStore :: Word8
instruction_daStore = 82

instruction_dCmpG :: Word8
instruction_dCmpG = 152

instruction_dCmpL :: Word8
instruction_dCmpL = 151

instruction_dConst0 :: Word8
instruction_dConst0 = 14

instruction_dConst1 :: Word8
instruction_dConst1 = 15

instruction_dDiv :: Word8
instruction_dDiv = 111

instruction_dLoad :: Word8
instruction_dLoad = 24

instruction_dLoad0 :: Word8
instruction_dLoad0 = 38

instruction_dLoad1 :: Word8
instruction_dLoad1 = 39

instruction_dLoad2 :: Word8
instruction_dLoad2 = 40

instruction_dLoad3 :: Word8
instruction_dLoad3 = 41

instruction_dMul :: Word8
instruction_dMul = 107

instruction_dNeg :: Word8
instruction_dNeg = 119

instruction_dRem :: Word8
instruction_dRem = 115

instruction_dReturn :: Word8
instruction_dReturn = 175

instruction_dStore :: Word8
instruction_dStore = 57

instruction_dStore0 :: Word8
instruction_dStore0 = 71

instruction_dStore1 :: Word8
instruction_dStore1 = 72

instruction_dStore2 :: Word8
instruction_dStore2 = 73

instruction_dStore3 :: Word8
instruction_dStore3 = 74

instruction_dSub :: Word8
instruction_dSub = 103

instruction_dup :: Word8
instruction_dup = 89

instruction_dup2 :: Word8
instruction_dup2 = 92

instruction_dup2X1 :: Word8
instruction_dup2X1 = 93

instruction_dup2X2 :: Word8
instruction_dup2X2 = 94

instruction_dupX1 :: Word8
instruction_dupX1 = 90

instruction_dupX2 :: Word8
instruction_dupX2 = 91

instruction_f2d :: Word8
instruction_f2d = 141

instruction_f2i :: Word8
instruction_f2i = 139

instruction_f2l :: Word8
instruction_f2l = 140

instruction_fAdd :: Word8
instruction_fAdd = 98

instruction_faLoad :: Word8
instruction_faLoad = 48

instruction_faStore :: Word8
instruction_faStore = 81

instruction_fCmpG :: Word8
instruction_fCmpG = 150

instruction_fCmpL :: Word8
instruction_fCmpL = 149

instruction_fConst0 :: Word8
instruction_fConst0 = 11

instruction_fConst1 :: Word8
instruction_fConst1 = 12

instruction_fConst2 :: Word8
instruction_fConst2 = 13

instruction_fDiv :: Word8
instruction_fDiv = 110

instruction_fLoad :: Word8
instruction_fLoad = 23

instruction_fLoad0 :: Word8
instruction_fLoad0 = 34

instruction_fLoad1 :: Word8
instruction_fLoad1 = 35

instruction_fLoad2 :: Word8
instruction_fLoad2 = 36

instruction_fLoad3 :: Word8
instruction_fLoad3 = 37

instruction_fMul :: Word8
instruction_fMul = 106

instruction_fNeg :: Word8
instruction_fNeg = 118

instruction_fRem :: Word8
instruction_fRem = 114

instruction_fReturn :: Word8
instruction_fReturn = 174

instruction_fStore :: Word8
instruction_fStore = 56

instruction_fStore0 :: Word8
instruction_fStore0 = 67

instruction_fStore1 :: Word8
instruction_fStore1 = 68

instruction_fStore2 :: Word8
instruction_fStore2 = 69

instruction_fStore3 :: Word8
instruction_fStore3 = 70

instruction_fSub :: Word8
instruction_fSub = 102

instruction_getField :: Word8
instruction_getField = 180

instruction_getStatic :: Word8
instruction_getStatic = 178

instruction_goto :: Word8
instruction_goto = 167

instruction_gotoW :: Word8
instruction_gotoW = 200

instruction_i2b :: Word8
instruction_i2b = 145

instruction_i2c :: Word8
instruction_i2c = 146

instruction_i2d :: Word8
instruction_i2d = 135

instruction_i2f :: Word8
instruction_i2f = 134

instruction_i2l :: Word8
instruction_i2l = 133

instruction_i2s :: Word8
instruction_i2s = 147

instruction_iAdd :: Word8
instruction_iAdd = 96

instruction_iALoad :: Word8
instruction_iALoad = 46

instruction_iAStore :: Word8
instruction_iAStore = 79

instruction_iAnd :: Word8
instruction_iAnd = 126

instruction_iCmp :: Word8
instruction_iCmp = 159

instruction_iConstM1 :: Word8
instruction_iConstM1 = 2

instruction_iConst0 :: Word8
instruction_iConst0 = 3

instruction_iConst1 :: Word8
instruction_iConst1 = 4

instruction_iConst2 :: Word8
instruction_iConst2 = 5

instruction_iConst3 :: Word8
instruction_iConst3 = 6

instruction_iConst4 :: Word8
instruction_iConst4 = 7

instruction_iConst5 :: Word8
instruction_iConst5 = 8

instruction_iDiv :: Word8
instruction_iDiv = 108

instruction_iLoad :: Word8
instruction_iLoad = 21

instruction_iLoad0 :: Word8
instruction_iLoad0 = 26

instruction_iLoad1 :: Word8
instruction_iLoad1 = 27

instruction_iLoad2 :: Word8
instruction_iLoad2 = 28

instruction_iLoad3 :: Word8
instruction_iLoad3 = 29

instruction_iMul :: Word8
instruction_iMul = 104

instruction_iNeg :: Word8
instruction_iNeg = 116

instruction_instanceOf :: Word8
instruction_instanceOf = 193

instruction_invokeDynamic :: Word8
instruction_invokeDynamic = 186

instruction_invokeInterface :: Word8
instruction_invokeInterface = 185

instruction_invokeSpecial :: Word8
instruction_invokeSpecial = 183

instruction_invokeStatic :: Word8
instruction_invokeStatic = 184

instruction_invokeVirtual :: Word8
instruction_invokeVirtual = 182

instruction_iOr :: Word8
instruction_iOr = 128

instruction_iRem :: Word8
instruction_iRem = 112

instruction_iReturn :: Word8
instruction_iReturn = 172

instruction_iShl :: Word8
instruction_iShl = 120

instruction_iShr :: Word8
instruction_iShr = 122

instruction_iStore :: Word8
instruction_iStore = 54

instruction_iStore0 :: Word8
instruction_iStore0 = 59

instruction_iStore1 :: Word8
instruction_iStore1 = 60

instruction_iStore2 :: Word8
instruction_iStore2 = 61

instruction_iStore3 :: Word8
instruction_iStore3 = 62

instruction_iSub :: Word8
instruction_iSub = 100

instruction_iUShr :: Word8
instruction_iUShr = 124

instruction_iXor :: Word8
instruction_iXor = 130

instruction_ifAcmpeq :: Word8
instruction_ifAcmpeq = 165

instruction_ifAcmpne :: Word8
instruction_ifAcmpne = 166

instruction_ifIcmpeq :: Word8
instruction_ifIcmpeq = 159

instruction_ifIcmpne :: Word8
instruction_ifIcmpne = 160

instruction_ifIcmplt :: Word8
instruction_ifIcmplt = 161

instruction_ifIcmpge :: Word8
instruction_ifIcmpge = 162

instruction_ifIcmpgt :: Word8
instruction_ifIcmpgt = 163

instruction_ifIcmple :: Word8
instruction_ifIcmple = 164

instruction_ifEq :: Word8
instruction_ifEq = 153

instruction_ifNe :: Word8
instruction_ifNe = 154

instruction_ifLt :: Word8
instruction_ifLt = 155

instruction_ifGe :: Word8
instruction_ifGe = 156

instruction_ifGt :: Word8
instruction_ifGt = 157

instruction_ifLe :: Word8
instruction_ifLe = 158

instruction_ifNonNull :: Word8
instruction_ifNonNull = 199

instruction_ifNull :: Word8
instruction_ifNull = 198

instruction_iInc :: Word8
instruction_iInc = 132

instruction_jsr :: Word8
instruction_jsr = 168

instruction_jsrW :: Word8
instruction_jsrW = 201

instruction_l2d :: Word8
instruction_l2d = 138

instruction_l2f :: Word8
instruction_l2f = 137

instruction_l2i :: Word8
instruction_l2i = 136

instruction_lAdd :: Word8
instruction_lAdd = 97

instruction_lALoad :: Word8
instruction_lALoad = 47

instruction_lAnd :: Word8
instruction_lAnd = 127

instruction_lAStore :: Word8
instruction_lAStore = 80

instruction_lCmp :: Word8
instruction_lCmp = 148

instruction_lConst0 :: Word8
instruction_lConst0 = 9

instruction_lConst1 :: Word8
instruction_lConst1 = 10

instruction_ldc :: Word8
instruction_ldc = 0x12

instruction_ldcW :: Word8
instruction_ldcW = 19

instruction_ldc2W :: Word8
instruction_ldc2W = 20

instruction_lDiv :: Word8
instruction_lDiv = 109

instruction_lLoad :: Word8
instruction_lLoad = 22

instruction_lLoad0 :: Word8
instruction_lLoad0 = 30

instruction_lLoad1 :: Word8
instruction_lLoad1 = 31

instruction_lLoad2 :: Word8
instruction_lLoad2 = 32

instruction_lLoad3 :: Word8
instruction_lLoad3 = 33

instruction_lMul :: Word8
instruction_lMul = 105

instruction_lNeg :: Word8
instruction_lNeg = 117

instruction_lookupSwitch :: Word8
instruction_lookupSwitch = 171

instruction_lOr :: Word8
instruction_lOr = 129

instruction_lRem :: Word8
instruction_lRem = 113

instruction_lReturn :: Word8
instruction_lReturn = 173

instruction_lShl :: Word8
instruction_lShl = 121

instruction_lShr :: Word8
instruction_lShr = 123

instruction_lStore :: Word8
instruction_lStore = 55

instruction_lStore0 :: Word8
instruction_lStore0 = 63

instruction_lStore1 :: Word8
instruction_lStore1 = 64

instruction_lStore2 :: Word8
instruction_lStore2 = 65

instruction_lStore3 :: Word8
instruction_lStore3 = 66

instruction_lSub :: Word8
instruction_lSub = 101

instruction_lUShr :: Word8
instruction_lUShr = 125

instruction_lXor :: Word8
instruction_lXor = 131

instruction_monitorEnter :: Word8
instruction_monitorEnter = 194

instruction_monitorExit :: Word8
instruction_monitorExit = 195

instruction_multiANewArray :: Word8
instruction_multiANewArray = 197

instruction_new :: Word8
instruction_new = 187

instruction_newArray :: Word8
instruction_newArray = 188

instruction_nop :: Word8
instruction_nop = 0

instruction_pop :: Word8
instruction_pop = 87

instruction_pop2 :: Word8
instruction_pop2 = 88

instruction_putField :: Word8
instruction_putField = 181

instruction_putStatic :: Word8
instruction_putStatic = 179

instruction_ret :: Word8
instruction_ret = 169

instruction_return :: Word8
instruction_return = 177

instruction_sALoad :: Word8
instruction_sALoad = 53

instruction_sAStore :: Word8
instruction_sAStore = 86

instruction_sIPush :: Word8
instruction_sIPush = 17

instruction_swap :: Word8
instruction_swap = 95

instruction_tableSwitch :: Word8
instruction_tableSwitch = 170

instruction_wide :: Word8
instruction_wide = 196
