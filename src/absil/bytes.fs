// Copyright (c) Microsoft Corporation.  All Rights Reserved.  See License.txt in the project root for license information.

/// Byte arrays
namespace Microsoft.FSharp.Compiler.AbstractIL.Internal

open System
open System.IO
open System.Runtime.CompilerServices
open Internal.Utilities

open Microsoft.FSharp.Compiler.AbstractIL 
open Microsoft.FSharp.Compiler.AbstractIL.Internal

module internal Bytes = 
    let b0 n =  (n &&& 0xFF)
    let b1 n =  ((n >>> 8) &&& 0xFF)
    let b2 n =  ((n >>> 16) &&& 0xFF)
    let b3 n =  ((n >>> 24) &&& 0xFF)

    let dWw1 n = int32 ((n >>> 32) &&& 0xFFFFFFFFL)
    let dWw0 n = int32 (n          &&& 0xFFFFFFFFL)

    let get (b:byte[]) n = int32 (Array.get b n)  
    let zeroCreate n : byte[] = Array.zeroCreate n      

    let sub ( b:byte[]) s l = Array.sub b s l   
    let blit (a:byte[]) b c d e = Array.blit a b c d e 

    let ofInt32Array (arr:int[]) = Array.init arr.Length (fun i -> byte arr.[i]) 

    let stringAsUtf8NullTerminated (s:string) = 
        Array.append (System.Text.Encoding.UTF8.GetBytes s) (ofInt32Array [| 0x0 |]) 

    let stringAsUnicodeNullTerminated (s:string) = 
        Array.append (System.Text.Encoding.Unicode.GetBytes s) (ofInt32Array [| 0x0;0x0 |])
        
[<Struct>]
type private ChunkedByteBufferImplState =
    {
        position: int
        chunkIndex: int
        length: int
    }

[<Sealed>]
type ChunkedByteBufferImpl =
    
    val mutable private state : ChunkedByteBufferImplState
    val private chunks : ResizeArray<byte []>
    val private chunkSize : int
    val private buffer : byte []

    new () =
        let chunkSize = 1024 * 8 // 8k bytes
        {
            state = { position = 0; chunkIndex = 0; length = 0 }
            chunks = ResizeArray([|Array.zeroCreate chunkSize|])
            chunkSize = chunkSize
            buffer = Array.zeroCreate 64
        }

    member this.Item 
        with [<MethodImpl(MethodImplOptions.AggressiveInlining)>] get i =
            this.chunks.[i / this.chunkSize].[i % this.chunkSize]
        and [<MethodImpl(MethodImplOptions.AggressiveInlining)>] set i value =
            this.chunks.[i / this.chunkSize].[i % this.chunkSize] <- value

    member this.WriteByte(value: byte) =
        let state = this.state

        let position1 = state.position
        let nextPosition = position1 + 1

        if nextPosition >= this.chunkSize then
            this.chunks.Add(Array.zeroCreate this.chunkSize)

            let size = this.chunkSize * state.chunkIndex
            this.[position1 + size] <- byte value

            this.state <- { position = nextPosition % this.chunkSize; chunkIndex = state.chunkIndex + 1; length = state.length + 1 }
        else
            let data = this.chunks.[state.chunkIndex]
            data.[position1] <- value

            this.state <- { state with position = nextPosition; length = state.length + 1 }

    member this.WriteUInt16(value: uint16) =
        let state = this.state

        let position1 = state.position
        let position2 = position1 + 1
        let nextPosition = position2 + 1

        if nextPosition >= this.chunkSize then
            this.chunks.Add(Array.zeroCreate this.chunkSize)

            let size = this.chunkSize * state.chunkIndex
            this.[position1 + size] <- byte value
            this.[position2 + size] <- byte (value >>> 8)

            this.state <- { position = nextPosition % this.chunkSize; chunkIndex = state.chunkIndex + 1; length = state.length + 2 }
        else
            let data = this.chunks.[state.chunkIndex]
            data.[position1] <- byte value
            data.[position2] <- byte (value >>> 8)

            this.state <- { state with position = nextPosition; length = state.length + 2 }

    member this.WriteInt(value: int) =
        let state = this.state

        let position1 = state.position
        let position2 = position1 + 1
        let position3 = position2 + 1
        let position4 = position3 + 1
        let nextPosition = position4 + 1

        if nextPosition >= this.chunkSize then
            this.chunks.Add(Array.zeroCreate this.chunkSize)

            let size = this.chunkSize * state.chunkIndex
            this.[position1 + size] <- byte value
            this.[position2 + size] <- byte (value >>> 8)
            this.[position3 + size] <- byte (value >>> 16)
            this.[position4 + size] <- byte (value >>> 24)

            this.state <- { position = nextPosition % this.chunkSize; chunkIndex = state.chunkIndex + 1; length = state.length + 4 }
        else
            let data = this.chunks.[state.chunkIndex]
            data.[position1] <- byte value
            data.[position2] <- byte (value >>> 8)
            data.[position3] <- byte (value >>> 16)
            data.[position4] <- byte (value >>> 24)

            this.state <- { state with position = nextPosition; length = state.length + 4 }

    member this.ByteArrayOperation(buffer: byte [], offset: int, length: int, f) =
        let mutable remainingLength = length
        let mutable currentOffset = offset

        while remainingLength > 0 do
            let state = this.state

            let operatingLength =
                let operatingLength = this.chunkSize - state.position
                if remainingLength > operatingLength then operatingLength
                else remainingLength

            remainingLength <- remainingLength - operatingLength
            currentOffset <- currentOffset + operatingLength

            let data = this.chunks.[state.chunkIndex]
            f buffer currentOffset data state.position operatingLength

            let nextPosition = (state.position + operatingLength + 1) % this.chunkSize
            if nextPosition = 0 then
                this.chunks.Add(Array.zeroCreate this.chunkSize)
                this.state <- { position = 0; chunkIndex = state.chunkIndex; length = state.length + operatingLength }
            else
                this.state <- { state with position = nextPosition; length = state.length + operatingLength }   

    member this.WriteBytes(buffer: byte [], offset: int, length: int) =
        this.ByteArrayOperation(buffer, offset, length, fun buffer offset data position length ->
            Buffer.BlockCopy(buffer, offset, data, position, length)
        )           

    member this.Position
        with get () = 
            (this.chunkSize * this.state.chunkIndex) + this.state.position
        and set value =
            this.state <- { this.state with position = value % this.chunkSize; chunkIndex = value / this.chunkSize }

    member this.Length = this.state.length

    member this.CopyTo(buffer: byte [], offset: int, length: int) =       
        if length > (this.Length - this.Position) then
            raise (ArgumentOutOfRangeException())

        this.ByteArrayOperation(buffer, offset, length, fun buffer offset data position length ->
            Buffer.BlockCopy(data, position, buffer, offset, length)
        )    

    member this.Write(cbs: ChunkedByteBufferImpl) =
        let cbsLengthMinusOne = cbs.Length - 1
        let lastChunkIndex = cbsLengthMinusOne / this.chunkSize
        let lastChunkPosition = cbsLengthMinusOne % this.chunkSize
        for i = 0 to lastChunkIndex do
            let chunk = cbs.chunks.[i]
            if cbs.state.chunkIndex = i then
                if lastChunkPosition > 0 then
                    this.WriteBytes(chunk, 0, lastChunkPosition)
            else
                this.WriteBytes(chunk, 0, chunk.Length)

[<AbstractClass>]
type AbstractByteBuffer() = 

    abstract EmitIntAsByte : int -> unit

    abstract EmitByte : byte -> unit

    abstract EmitIntsAsBytes : int [] -> unit

    abstract FixupInt32 : pos: int -> int -> unit

    abstract EmitInt32 : int -> unit

    abstract EmitBytes : byte [] -> unit

    abstract EmitInt32AsUInt16 : int -> unit

    member buf.EmitBoolAsByte (b:bool) = buf.EmitIntAsByte (if b then 1 else 0)

    member buf.EmitUInt16 (x:uint16) = buf.EmitInt32AsUInt16 (int32 x)

    member buf.EmitInt64 x = 
        buf.EmitInt32 (Bytes.dWw0 x)
        buf.EmitInt32 (Bytes.dWw1 x)

    abstract Position : int

    abstract Length : int

    abstract Write : AbstractByteBuffer -> unit

[<Sealed>]
type ChunkedByteBuffer() =
    inherit AbstractByteBuffer()

    let impl = ChunkedByteBufferImpl()

    member __.Impl = impl

    override __.EmitIntAsByte n = impl.WriteByte(byte n)

    override __.EmitByte n = impl.WriteByte n

    override __.EmitIntsAsBytes (arr: int []) =
        for i = 0 to arr.Length - 1 do
            impl.WriteByte (byte arr.[i])

    override __.FixupInt32 pos n =
        let lastPos = impl.Position
        impl.Position <- pos
        impl.WriteInt n
        impl.Position <- lastPos

    override __.EmitInt32 n = impl.WriteInt n

    override __.EmitBytes bytes = impl.WriteBytes(bytes, 0, bytes.Length)
    
    override __.EmitInt32AsUInt16 n =
        impl.WriteByte(Bytes.b0 n |> byte)
        impl.WriteByte(Bytes.b1 n |> byte)

    override __.Position = impl.Position

    override __.Length = impl.Length

    override __.Write (abb: AbstractByteBuffer) =
        match abb with
        | :? ChunkedByteBuffer as cbb ->
            cbb.Impl.Write(cbb.Impl)
        | _ -> failwith "not supported"

    static member Create() = ChunkedByteBuffer()

type internal ByteStream = 
    { bytes: byte[] 
      mutable pos: int 
      max: int }
    member b.ReadByte() = 
        if b.pos >= b.max then failwith "end of stream"
        let res = b.bytes.[b.pos]
        b.pos <- b.pos + 1
        res 
    member b.ReadUtf8String n = 
        let res = System.Text.Encoding.UTF8.GetString(b.bytes,b.pos,n)  
        b.pos <- b.pos + n; res 
      
    static member FromBytes (b:byte[],n,len) = 
        if n < 0 || (n+len) > b.Length then failwith "FromBytes"
        { bytes = b; pos = n; max = n+len }

    member b.ReadBytes n  = 
        if b.pos + n > b.max then failwith "ReadBytes: end of stream"
        let res = Bytes.sub b.bytes b.pos n
        b.pos <- b.pos + n
        res 

    member b.Position = b.pos 
#if LAZY_UNPICKLE
    member b.CloneAndSeek = { bytes=b.bytes; pos=pos; max=b.max }
    member b.Skip = b.pos <- b.pos + n
#endif


type internal ByteBuffer = 
    { mutable bbArray: byte[] 
      mutable bbCurrent: int }

    member buf.Ensure newSize = 
        let oldBufSize = buf.bbArray.Length 
        if newSize > oldBufSize then 
            let old = buf.bbArray 
            buf.bbArray <- Bytes.zeroCreate (max newSize (oldBufSize * 2))
            Bytes.blit old 0 buf.bbArray 0 buf.bbCurrent

    member buf.Close () = Bytes.sub buf.bbArray 0 buf.bbCurrent

    member buf.EmitIntAsByte (i:int) = 
        let newSize = buf.bbCurrent + 1 
        buf.Ensure newSize
        buf.bbArray.[buf.bbCurrent] <- byte i
        buf.bbCurrent <- newSize 

    member buf.EmitByte (b:byte) = buf.EmitIntAsByte (int b)

    member buf.EmitIntsAsBytes (arr:int[]) = 
        let n = arr.Length
        let newSize = buf.bbCurrent + n 
        buf.Ensure newSize
        let bbarr = buf.bbArray
        let bbbase = buf.bbCurrent
        for i = 0 to n - 1 do 
            bbarr.[bbbase + i] <- byte arr.[i] 
        buf.bbCurrent <- newSize 

    member bb.FixupInt32 pos n = 
        bb.bbArray.[pos] <- (Bytes.b0 n |> byte)
        bb.bbArray.[pos + 1] <- (Bytes.b1 n |> byte)
        bb.bbArray.[pos + 2] <- (Bytes.b2 n |> byte)
        bb.bbArray.[pos + 3] <- (Bytes.b3 n |> byte)

    member buf.EmitInt32 n = 
        let newSize = buf.bbCurrent + 4 
        buf.Ensure newSize
        buf.FixupInt32 buf.bbCurrent n
        buf.bbCurrent <- newSize 

    member buf.EmitBytes (i:byte[]) = 
        let n = i.Length 
        let newSize = buf.bbCurrent + n 
        buf.Ensure newSize
        Bytes.blit i 0 buf.bbArray buf.bbCurrent n
        buf.bbCurrent <- newSize 

    member buf.EmitInt32AsUInt16 n = 
        let newSize = buf.bbCurrent + 2 
        buf.Ensure newSize
        buf.bbArray.[buf.bbCurrent] <- (Bytes.b0 n |> byte)
        buf.bbArray.[buf.bbCurrent + 1] <- (Bytes.b1 n |> byte)
        buf.bbCurrent <- newSize 
    
    member buf.EmitBoolAsByte (b:bool) = buf.EmitIntAsByte (if b then 1 else 0)

    member buf.EmitUInt16 (x:uint16) = buf.EmitInt32AsUInt16 (int32 x)

    member buf.EmitInt64 x = 
        buf.EmitInt32 (Bytes.dWw0 x)
        buf.EmitInt32 (Bytes.dWw1 x)

    member buf.Position = buf.bbCurrent

    member buf.WriteChunkedByteBuffer (cbb: ChunkedByteBuffer) =
        let length = cbb.Length
        let newSize = buf.bbCurrent + length
        buf.Ensure newSize
        let lastPos = cbb.Position
        cbb.Impl.Position <- 0
        cbb.Impl.CopyTo(buf.bbArray, buf.Position, length)
        cbb.Impl.Position <- lastPos

    static member Create sz = 
        { bbArray=Bytes.zeroCreate sz 
          bbCurrent = 0 }
