// http://gbdev.gg8.se/wiki/articles/Memory_Bank_Controllers
namespace GameBoyEmulator

type Mbc1Kind =
    | Mbc1
    | Mbc1Ram
    | Mbc1RamBattery

type Mbc2Kind =
    | Mbc2
    | Mbc2Battery

type RomOnly =
    { ram : byte []
      rom : byte [] }

type Mbc1 =
    { kind : Mbc1Kind
      ram : byte []
      rom : byte []
      isRamEnabled : bool
      lowerRomBank : byte
      upperRomOrRamBank : byte
      romOrRamMode : byte }

type Mbc2 =
    { kind : Mbc2Kind
      ram : byte []
      rom : byte []
      isRamEnabled : bool
      romBank : byte }

type Mbc =
    | RomOnly of RomOnly
    | Mbc1 of Mbc1
    | Mbc2 of Mbc2

module Mbc =

    open System.IO

    let private createRom romPath =
        let rom = File.ReadAllBytes romPath
        let romSize = match rom.[0x0148] with
                      | 0x52uy -> 1179648
                      | 0x53uy -> 1310720
                      | 0x54uy -> 1572864
                      | flag -> (32 * 1024) <<< int flag
        if romSize = rom.Length then
            printf "Loaded ROM %s (%d bytes)" romPath romSize
            Ok rom
        else Error (sprintf "Unexpected ROM file size. Got: %d, expected: %d" romSize rom.Length)

    let private getRamSize rom =
        match Array.get rom 0x0149 with
        | 0x00uy -> Ok 0
        | 0x01uy -> Ok (1024 * 2)
        | 0x02uy -> Ok (1024 * 8)
        | 0x03uy -> Ok (1024 * 32)
        | flag -> Error (sprintf "Unexpected RAM size flag: 0x%02X" flag)

    let private loadRam ramSize ramPath =
        if ramSize > 0 && File.Exists ramPath then
            let ram = File.ReadAllBytes ramPath
            if ramSize = ram.Length then
                printf "Loaded RAM %s (%d bytes)" ramPath ramSize
                Ok ram
            else Error (sprintf "Unexpected saved RAM file size. Got: %d, expected : %d" ram.Length ramSize)
        else Ok [| |]

    let private createRam rom ramPath =
        Utils.result {
            let! ramSize = getRamSize rom
            let! ram = loadRam ramSize ramPath
            return ram
        }

    let private createMbc rom ram =
        match Array.get rom 0x0147 with
        | 0x00uy ->
            RomOnly { rom = rom
                      ram = ram } |> Ok
        | 0x01uy | 0x02uy | 0x03uy as t ->
            let kind = if t = 0x01uy then Mbc1Kind.Mbc1
                       elif t = 0x02uy then Mbc1Ram
                       else Mbc1RamBattery
            Mbc1 { kind = kind
                   rom = rom
                   ram = ram
                   isRamEnabled = false
                   lowerRomBank = 0x01uy
                   upperRomOrRamBank = 0x00uy
                   romOrRamMode = 0x00uy } |> Ok
        | 0x05uy | 0x06uy as t->
            let kind = if t = 0x05uy then Mbc2Kind.Mbc2
                       else Mbc2Battery
            Mbc2 { kind = kind
                   rom = rom
                   isRamEnabled = false
                   ram = Array.zeroCreate (0x1FF + 1)
                   romBank = 0x01uy } |> Ok
        | t -> Error (sprintf "Unsupported MBC type: 0x%02X" t)

    let romOnlyReadByte (mbc : RomOnly) address =
        match address with
        | addr when addr <= 0x7FFFus -> Ok mbc.rom.[int addr]
        | addr when 0xA000us <= addr && addr <= 0xBFFFus ->
            Ok mbc.ram.[int (addr - 0xA000us)]
        | addr -> Error (sprintf "ROM Only MBC doesn't support reading from 0x%04X" address)

    let romOnlyWriteByte (mbc : RomOnly) address value =
        if 0xA000us <= address && address <= 0xBFFFus then
            mbc.ram.[int (address - 0xA000us)] <- value
            Ok mbc
        else Error (sprintf "ROM Only MBC doesn't support writing to 0x%04X" address)

    let mbc1ReadByte (mbc : Mbc1) address =
        match address with
        | addr when addr <= 0x3FFFus ->
            (* 0000-3FFF - ROM Bank 00 (Read Only)
               This area always contains the first 16KBytes of the cartridge ROM. *)
            Ok mbc.rom.[int addr]
        | addr when addr <= 0x7FFFus ->
            (* 4000-7FFF - ROM Bank 01-7F (Read Only)
               This area may contain any of the further 16KByte banks of the ROM, allowing to address up to 125 ROM Banks (almost 2MByte).
               As described below, bank numbers 20h, 40h, and 60h cannot be used, resulting in the odd amount of 125 banks. *)            
            let mutable targetBank = mbc.lowerRomBank
            if mbc.romOrRamMode = 0x00uy then
                targetBank <- (mbc.upperRomOrRamBank <<< 4) ||| targetBank
            let target = (address - 0x4000us) + 0x4000us * uint16 targetBank
            Ok mbc.rom.[int target]
        | addr when 0xA000us <= addr && addr <= 0xBFFFus ->
            (* A000-BFFF - RAM Bank 00-03, if any (Read/Write)
               This area is used to address external RAM in the cartridge (if any).
               External RAM is often battery buffered, allowing to store game positions or high score tables,
               even if the gameboy is turned off, or if the cartridge is removed from the gameboy.
               Available RAM sizes are: 2KByte (at A000-A7FF), 8KByte (at A000-BFFF), and 32KByte (in form of four 8K banks at A000-BFFF). *)
            match mbc.isRamEnabled with
            | true ->
                let mutable target = address - 0xA000us
                if mbc.romOrRamMode = 0x01uy then
                    target <- target + 0x2000us * uint16 mbc.upperRomOrRamBank
                Ok mbc.ram.[int target]
            | false -> Error (sprintf "MBC1 doesn't support reading from 0x%04X when RAM is disabled" addr)
        | addr -> Error (sprintf "MBC1 doesn't support reading from 0x%04X" addr)

    let mbc1WriteByte (mbc : Mbc1) address value =
        match address with
        | addr when addr <= 0x1FFFus ->
            (* 0000-1FFF - RAM Enable (Write Only)
               Before external RAM can be read or written, it must be enabled by writing to this address space.
               It is recommended to disable external RAM after accessing it, in order to protect its contents from damage during power down of the gameboy.
               Usually the following values are used:
               00h  Disable RAM (default)
               0Ah  Enable RAM
               Practically any value with 0Ah in the lower 4 bits enables RAM, and any other value disables RAM. *)
            Ok { mbc with isRamEnabled = (value &&& 0x0Fuy) = 0x0Auy }
        | addr when addr <= 0x3FFFus ->
            (* 2000-3FFF - ROM Bank Number (Write Only)
               Writing to this address space selects the lower 5 bits of the ROM Bank Number (in range 01-1Fh).
               When 00h is written, the MBC translates that to bank 01h also.
               That doesn't harm so far, because ROM Bank 00h can be always directly accessed by reading from 0000-3FFF.
               But (when using the register below to specify the upper ROM Bank bits), the same happens for Bank 20h, 40h, and 60h.
               Any attempt to address these ROM Banks will select Bank 21h, 41h, and 61h instead. *)
            let mutable lowerRomBank = value &&& 0x1Fuy
            if lowerRomBank = 0x00uy then lowerRomBank <- 0x01uy
            Ok { mbc with lowerRomBank = lowerRomBank }
        | addr when addr <= 0x5FFFus ->
            (* 4000-5FFF - RAM Bank Number - or - Upper Bits of ROM Bank Number (Write Only)
               This 2bit register can be used to select a RAM Bank in range from 00-03h,
               or to specify the upper two bits (Bit 5-6) of the ROM Bank number, depending on the current ROM/RAM Mode. (See below.) *)
            Ok { mbc with upperRomOrRamBank = value &&& 0x03uy }
        | addr when addr <= 0x7FFFus ->
            (* 6000-7FFF - ROM/RAM Mode Select (Write Only)
               This 1bit Register selects whether the two bits of the above register should be used as upper two bits of the ROM Bank, or as RAM Bank Number.
               00h = ROM Banking Mode (up to 8KByte RAM, 2MByte ROM) (default)
               01h = RAM Banking Mode (up to 32KByte RAM, 512KByte ROM)
               The program may freely switch between both modes, the only limitiation is that only RAM Bank 00h can be used during Mode 0,
               and only ROM Banks 00-1Fh can be used during Mode 1. *)
            Ok { mbc with romOrRamMode = value &&& 0x01uy }
        | addr when 0xA000us <= addr && addr <= 0xBFFFus ->
            (* A000-BFFF - RAM Bank 00-03, if any (Read/Write)
               This area is used to address external RAM in the cartridge (if any).
               External RAM is often battery buffered, allowing to store game positions or high score tables,
               even if the gameboy is turned off, or if the cartridge is removed from the gameboy.
               Available RAM sizes are: 2KByte (at A000-A7FF), 8KByte (at A000-BFFF), and 32KByte (in form of four 8K banks at A000-BFFF). *)
            match mbc.isRamEnabled with
            | true ->
                let mutable target = addr - 0xA000us
                if mbc.romOrRamMode = 0x01uy then
                    target <- target + 0x2000us * uint16 mbc.upperRomOrRamBank
                mbc.ram.[int target] <- value
                Ok mbc
            | false -> Error (sprintf "MBC1 doesn't support writing to 0x%04X when RAM is disabled" addr)
        | addr -> Error (sprintf "MBC1 doesn't support writing to 0x%04X" addr)

    let mbc2ReadByte mbc address =
        match address with
        | addr when addr <= 0x3FFFus ->
            (* 0000-3FFF - ROM Bank 00 (Read Only)
               This area always contains the first 16KBytes of the cartridge ROM. *)
            Ok mbc.rom.[int addr]
        | addr when addr <= 0x7FFFus ->
            (* 4000-7FFF - ROM Bank 01-7F (Read Only)
               This area may contain any of the further 16KByte banks of the ROM, allowing to address up to 16 ROM Banks (almost 256KByte). *)
            let target = (addr - 0x4000us) + 0x4000us * uint16 mbc.romBank
            Ok mbc.rom.[int target]
        | addr when 0xA000us <= addr && addr <= 0xA1FFus ->
            (* A000-A1FF - 512x4bits RAM, built-in into the MBC2 chip (Read/Write)
               The MBC2 doesn't support external RAM, instead it includes 512x4 bits of built-in RAM (in the MBC2 chip itself).
               It still requires an external battery to save data during power-off though.
               As the data consists of 4bit values, only the lower 4 bits of the "bytes" in this memory area are used. *)
            match mbc.isRamEnabled with
            | true -> Ok mbc.ram.[int (addr - 0xA000us)]
            | false -> Error (sprintf "MBC2 doesn't support reading from 0x%04X when RAM is disabled" addr)
        | addr -> Error (sprintf "MBC2 doesn't support reading from 0x%04X" addr)

    let mbc2WriteByte mbc address value : Result<Mbc2, _> =
        match address with
        | addr when addr <= 0x1FFFus ->
            (* 0000-1FFF - RAM Enable (Write Only)
               The least significant bit of the upper address byte must be zero to enable/disable cart RAM.
               For example the following addresses can be used to enable/disable cart RAM: 0000-00FF, 0200-02FF, 0400-04FF, ..., 1E00-1EFF.
               The suggested address range to use for MBC2 ram enable/disable is 0000-00FF. *)
            if (addr &&& 0x0100us) = 0x0000us then
                Ok { mbc with isRamEnabled = (value &&& 0x0Fuy) = 0x0Auy }
            else Error (sprintf "MBC2 doesn't support writing to 0x%04X when the least significant bit of the upper address byte isn't 0" addr)
        | addr when addr <= 0x3FFFus ->
            (* 2000-3FFF - ROM Bank Number (Write Only)
               Writing a value (XXXXBBBB - X = Don't cares, B = bank select bits) into 2000-3FFF area will select an appropriate ROM bank at 4000-7FFF.
               The least significant bit of the upper address byte must be one to select a ROM bank.
               For example the following addresses can be used to select a ROM bank: 2100-21FF, 2300-23FF, 2500-25FF, ..., F00-3FFF.
               The suggested address range to use for MBC2 rom bank selection is 2100-21FF. *)
            if (addr &&& 0x1000us) = 0x0100us then
                Ok { mbc with romBank = value &&& 0x0Fuy }
            else Error (sprintf "MBC2 doesn't support writing to 0x%04X when the least significant bit of the upper address byte isn't 1" addr)
        | addr when 0xA000us <= addr && addr <= 0xA1FFus ->
            (* A000-A1FF - 512x4bits RAM, built-in into the MBC2 chip (Read/Write)
               The MBC2 doesn't support external RAM, instead it includes 512x4 bits of built-in RAM (in the MBC2 chip itself).
               It still requires an external battery to save data during power-off though.
               As the data consists of 4bit values, only the lower 4 bits of the "bytes" in this memory area are used. *)
            match mbc.isRamEnabled with
            | true ->
                mbc.ram.[int (addr - 0xA000us)] <- value &&& 0x0Fuy
                Ok mbc
            | false -> Error (sprintf "MBC2 doesn't support writing to 0x%04X when RAM is disabled" addr)
        | addr -> Error (sprintf "MBC2 doesn't support writing to 0x%04X" addr)

    let init romPath ramPath =
        Utils.result {
            let! rom = createRom romPath
            let! ram = createRam rom ramPath
            let! mbc = createMbc rom ram
            return mbc
        }
