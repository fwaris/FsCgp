module XorshiftRng

//xorshift128plus implementation, https://en.wikipedia.org/wiki/Xorshift
type XorshiftPRNG(seed) =
    let mutable s : uint64[] = Array.zeroCreate 2

    do s.[1] <- uint64 seed

    let sample() =
        let mutable x = s.[0]
        let y = s.[1]
        s.[0] <- y
        x <- x ^^^ (x <<< 23)
        s.[1] <- x ^^^ y ^^^ (x >>> 17) ^^^ (y >>> 26)
        let smpl = s.[1] + y
        if smpl = System.UInt64.MaxValue then smpl - 1UL else smpl

    member x.NextDouble() = (float (sample())) / float System.UInt64.MaxValue

    member x.Next(max) = 
        if max < 0 then failwith "max < 0"
        x.NextDouble() * (float max) |> int

    member x.Next(min:int,max:int) = 
        if min > max then failwith "min > max" 
        let r = max - min in (float r) * (x.NextDouble()) + (float min) |> int

    new()=XorshiftPRNG(System.Environment.TickCount)

(*
#load "XorshiftRng.fs"
let rng = XorshiftRng.XorshiftPRNG()
[|for i in 0..100 -> rng.Next(0,2)|]
*)