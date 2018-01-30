namespace GameBoyEmulator

type Cartridge =
    { mbc : Mbc
      romPath : string
      ramPath : string option }

module Cartridge =

    open System.IO

    let private getSavableRam mbc =
        match mbc with
        | Mbc1 mbc ->
            match mbc.kind with
            | Mbc1RamBattery -> Some mbc.ram
            | _ -> None
        | Mbc2 mbc ->
            match mbc.kind with
            | Mbc2Battery -> Some mbc.ram
            | _ -> None
        | _ -> None

    let init romPath =
        Utils.result {
            let ramPath = romPath + "_RAM"
            let! mbc = Mbc.init romPath ramPath
            let ramPath = getSavableRam mbc |> Option.bind (fun _ -> Some ramPath)
            return { mbc = mbc
                     romPath = romPath
                     ramPath = ramPath }
        }

    let readByte cartridge address =
        match cartridge.mbc with
        | RomOnly mbc -> Mbc.romOnlyReadByte mbc address
        | Mbc1 mbc -> Mbc.mbc1ReadByte mbc address
        | Mbc2 mbc -> Mbc.mbc2ReadByte mbc address

    let writeByte cartridge address value =
        match cartridge.mbc with
        | RomOnly mbc ->
            Mbc.romOnlyWriteByte mbc address value
            |> Result.map(fun mbc -> RomOnly mbc)
        | Mbc1 mbc ->
            Mbc.mbc1WriteByte mbc address value
            |> Result.map(fun mbc -> Mbc1 mbc)
        | Mbc2 mbc ->
            Mbc.mbc2WriteByte mbc address value
            |> Result.map(fun mbc -> Mbc2 mbc)

    let save cartridge =
        Utils.maybe {
            let! ramPath = cartridge.ramPath
            let! ram = getSavableRam cartridge.mbc
            File.WriteAllBytes (ramPath, ram)
            return true
        } |> Option.defaultValue false
