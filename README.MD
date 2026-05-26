# H2JVM

`h2jvm` (Haskell to JVM) is a high-level, type-safe library for building and generating JVM bytecode in Haskell. 

It handles the complex, low-level details of JVM class file generation automatically, including:
* **Label Offset Calculations**.
* **Stack Map Frame** analysis and `StackMapTable` attribute generation.
* **Constant Pool** building.

---

## Build & Test

You can build and test the project using standard Cabal commands:

```bash
cabal build
cabal test
```

---

## Usage Example

Below is a complete example demonstrating how to construct a simple class named `Calculator` containing a public static method `add` that takes two integers and returns their sum.

```haskell
{-# LANGUAGE OverloadedStrings #-}
import Effectful
import H2JVM
import H2JVM.Builder
import H2JVM.Analyse.StackMap
import Data.ByteString.Lazy qualified as LBS

main :: IO ()
main = do
  -- Construct the method body bytecode instructions
  let (_, _, instructions) = runPureEff $ runCodeBuilder $ do
        emit $ ILoad 0
        emit $ ILoad 1
        emit IAdd
        emit IReturn

  -- Define the class name, method descriptor, and access flags
  let className = "Calculator"
      methodDesc = MethodDescriptor -- int (int, int)
        [PrimitiveFieldType JInt, PrimitiveFieldType JInt] 
        (TypeReturn (PrimitiveFieldType JInt))
      accessFlags = [MPublic, MStatic]

  -- Run stack map analysis to calculate frames, max stack, and max locals
  let (frames, maxStack, maxLocals) = calculateStackMapFrames className accessFlags methodDesc instructions
      
      -- Package the results into a Code attribute
      codeAttr = CodeAttributeData
        { maxStack = fromIntegral maxStack
        , maxLocals = fromIntegral maxLocals
        , code = instructions
        , exceptionTable = []
        , codeAttributes = [StackMapTable frames]
        }
        
      addMethodInfo = ClassFileMethod
        { methodAccessFlags = accessFlags
        , methodName = "add"
        , methodDescriptor = methodDesc
        , methodAttributes = fromList [Code codeAttr]
        }

  -- Construct the class using ClassBuilder
  let (classFile, _) = runPureEff $ runClassBuilder className java8 $ do
        addAccessFlag CPublic
        addMethod addMethodInfo

  -- Serialise the class to a file
  let path = classFilePath classFile -- returns "Calculator.class"
  case classFileBytes classFile of
    Left err -> putStrLn $ "Error generating bytecode: " <> show err
    Right bytes -> LBS.writeFile path bytes
```

This will produce a class file, which, after being disassembled looks roughly like:

```java
    public class Calculator {
      public static int add(int, int);
        Code:
             0: iload_0
             1: iload_1
             2: iadd
             3: ireturn
    }
```

