namespace GameBoyEmulator

type Cpu =
    { mutable a : byte
      mutable f : byte
      mutable b : byte
      mutable c : byte
      mutable d : byte
      mutable e : byte
      mutable h : byte
      mutable l : byte
      mutable sp : uint16
      mutable pc : uint16 }

type Register16 =
    | AF
    | BC
    | DE
    | HL

type Flag =
    | ZF
    | NF
    | HF
    | CF

type BankMode =
    | NoBanking
    | MBC1
    | MBC2

type Memory =
    { romBanks : byte[][]
      ramBanks : byte[][]
      rom : byte[]
      vram : byte[]
      eram : byte[]
      wram : byte[]
      oam : byte[]
      io : byte[]
      hram : byte[]
      mutable currentRomBank : int
      mutable currentRamBank : int
      bankMode : BankMode
      mutable enableRamWriting : bool }

type GpuMode =
    | HBlank
    | VBlank
    | OAM
    | VRAM

type Gpu =
    { mode : GpuMode
      mutable ticks : int } 

type System =
    { cpu : Cpu
      gpu : Gpu
      memory : Memory }

