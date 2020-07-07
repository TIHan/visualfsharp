module internal FSharp.Compiler.Internal.ArrayExtensions

open System.Runtime.CompilerServices

[<AbstractClass;Sealed;Extension>]
type ArrayExtensions =

    [<Extension>]
    static member BinarySearch(array: int[], value: int) =
        let rec binarySearch low high =
            if low <= high then
                let middle = low + ((high - low) >>> 1)
                let midValue = array.[middle]
 
                if midValue = value then
                    middle
                elif midValue > value then
                    binarySearch low (middle - 1)
                else
                    binarySearch (middle + 1) high
            else
                ~~~low

        binarySearch 0 (array.Length - 1)
