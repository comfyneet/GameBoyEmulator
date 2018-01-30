namespace GameBoyEmulator

module Cpu =

    let getRegister16 cpu = function
        | AF -> ((uint16 cpu.a) <<< 8) ||| uint16 cpu.f
        | BC -> ((uint16 cpu.b) <<< 8) ||| uint16 cpu.c
        | DE -> ((uint16 cpu.d) <<< 8) ||| uint16 cpu.e
        | HL -> ((uint16 cpu.h) <<< 8) ||| uint16 cpu.l

    let setRegister16 cpu data = function
        | AF -> cpu.a <- byte (data >>> 8); cpu.f <- byte (data &&& 0x00FFus)
        | BC -> cpu.b <- byte (data >>> 8); cpu.c <- byte (data &&& 0x00FFus)
        | DE -> cpu.d <- byte (data >>> 8); cpu.e <- byte (data &&& 0x00FFus)
        | HL -> cpu.h <- byte (data >>> 8); cpu.l <- byte (data &&& 0x00FFus)

    let getFlag cpu = function
        | ZF -> Utils.getBit cpu.f 7
        | NF -> Utils.getBit cpu.f 6
        | HF -> Utils.getBit cpu.f 5
        | CF -> Utils.getBit cpu.f 4

    let setFlag cpu data = function
        | ZF -> cpu.f <- Utils.setBit cpu.f 7 data
        | NF -> cpu.f <- Utils.setBit cpu.f 6 data
        | HF -> cpu.f <- Utils.setBit cpu.f 5 data
        | CF -> cpu.f <- Utils.setBit cpu.f 4 data

    let init () =
        { a = 0x01uy
          f = 0xB0uy
          b = 0x00uy
          c = 0x13uy
          d = 0x00uy
          e = 0xD8uy
          h = 0x01uy
          l = 0x4Duy
          sp = 0xFFFEus
          pc = 0x0100us }

    let instructions registers = [|
    (* 0x00 *) "NOP", 0uy, fun _ -> ()
    (* 0x01 *) "LD BC, 0x%X", 2uy, fun operand -> registers.bc <- operand
    (* 0x02 *) "LD BC, A", 0uy, fun operand -> ()
    |]
