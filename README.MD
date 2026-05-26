# H2JVM
H2JVM (Haskell to JVM) is a high level library for writing JVM bytecode in Haskell.
It handles the messy details such as jump offsets, stack map frames, and more. 


H2JVM does its processing in a pipeline:
1. The user writes a class as a `H2JVM.ClassFile`, potentially using the `H2JVM.Builder` monads
2. This class is analysed using code in the `H2JVM.Analyse` modules - calculating jump offsets, etc.
   This stage may also do some optimisations, such as removing dead code.
3. The class is converted to a low level format (`H2JVM.Internal.Raw`) using the `H2JVM.Internal.Convert` module
4. The low level format is converted to a `ByteString` using the `H2JVM.Internal.Raw` modules, which can be written to a file. or used in a JVM.