namespace GameBoyEmulator

module Memory =

    let private toggleRamWriting memory data address =
        match memory.bankMode with
        | NoBanking -> ()
        | MBC1 ->
            match data &&& 0x0Fuy with
            | 0x0Auy -> memory.enableRamWriting <- true
            | _ -> memory.enableRamWriting <- false
        | MBC2 ->
            if Utils.getBit address 8 = false then
                match data &&& 0x0Fuy with
                | 0x0Auy -> memory.enableRamWriting <- true
                | _ -> memory.enableRamWriting <- false        

    let private changeCurrentLowRomBank memory data =
        match memory.bankMode with
        | NoBanking -> ()
        | MBC1 ->
            memory.currentRomBank <- (memory.currentRomBank &&& 0xE0) ||| int (data &&& 0x1Fuy)
            if memory.currentRomBank = 0 then memory.currentRomBank <- 1
        | MBC2 ->
            memory.currentRomBank <- int (data &&& 0x0Fuy)        

    let private changeCurrentHighRomBank memory data =
        match memory.bankMode with
        | NoBanking -> ()
        | MBC1 -> ()

    let readByte memory = function
        | addr when 0x0000us <= addr && addr <= 0x7FFFus ->
            let newAddr = if addr < 0x4000us then addr else addr - 0x4000us
            memory.romBanks.[memory.currentRomBank].[int newAddr] // TODO
        | addr when 0x8000us <= addr && addr <= 0x9FFFus -> memory.vram.[int (addr - 0x8000us)]
        | addr when 0xA000us <= addr && addr <= 0xBFFFus -> memory.ramBanks.[memory.currentRamBank].[int (addr - 0xA000us)]
        | addr when 0xC000us <= addr && addr <= 0xDFFFus -> memory.wram.[int (addr - 0xC000us)]
        | addr when 0xE000us <= addr && addr <= 0xFDFFus -> memory.wram.[int (addr - 0xE000us)]
        | addr when 0xFE00us <= addr && addr <= 0xFE9Fus -> memory.oam.[int (addr - 0x0FE00us)]
        | addr when 0xFF00us <= addr && addr <= 0xFF7Fus -> memory.io.[int (addr - 0xFF00us)]
        | addr when 0xFF80us <= addr && addr <= 0xFFFFus -> memory.hram.[int (addr - 0xFF80us)]
        | addr -> failwithf "Cannot read from address 0x%X" addr

    let writeByte memory data = function
        | addr when 0x0000us <= addr && addr <= 0x1FFFus ->
            toggleRamWriting memory data addr
        | addr when 0x2000us <= addr && addr <= 0x3FFFus ->
            changeCurrentLowRomBank memory data
        | addr when 0x4000us <= addr && addr <= 0x5FFFus ->
            changeCurrentHighRomBank memory data
        | addr when 0x4000us <= addr && addr <= 0x7FFFus -> failwithf "Cannot write to address 0x%X" addr
        | addr when 0x8000us <= addr && addr <= 0x9FFFus -> memory.vram.[int (addr - 0x8000us)] <- data
        | addr when 0xA000us <= addr && addr <= 0xBFFFus -> failwith "TODO"
        | addr when 0xC000us <= addr && addr <= 0xDFFFus -> memory.wram.[int (addr - 0xC000us)] <- data
        | addr when 0xE000us <= addr && addr <= 0xFDFFus -> memory.wram.[int (addr - 0xE000us)] <- data
        | _ -> failwith "TODO"

    let private getBankMode memory =
        match readByte memory 0x0147us with
        | 0uy -> NoBanking
        | 1uy | 2uy | 3uy -> MBC1
        | 5uy | 6uy -> MBC2
        | mode -> failwithf "Unknown ROM bank mode %u" mode

    let init () =
        let memory =
            { romBanks = Array.empty
              ramBanks = Array.empty
              rom = Array.zeroCreate 8000
              vram = Array.zeroCreate 2000
              eram = Array.zeroCreate 2000
              wram = Array.zeroCreate 2000
              oam = Array.zeroCreate 100
              io = Array.zeroCreate 100
              hram = Array.zeroCreate 80
              currentRomBank = 0
              currentRamBank = 0
              bankMode = NoBanking
              enableRamWriting = false }

        writeByte memory 0x00uy 0xFF05us
        writeByte memory 0x00uy 0xFF06us
        writeByte memory 0x00uy 0xFF07us
        writeByte memory 0x80uy 0xFF10us
        writeByte memory 0xBFuy 0xFF11us
        writeByte memory 0xF3uy 0xFF12us
        writeByte memory 0xBFuy 0xFF14us
        writeByte memory 0x3Fuy 0xFF16us
        writeByte memory 0x00uy 0xFF17us
        writeByte memory 0xBFuy 0xFF19us
        writeByte memory 0x7Fuy 0xFF1Aus
        writeByte memory 0xFFuy 0xFF1Bus
        writeByte memory 0x9Fuy 0xFF1Cus
        writeByte memory 0xBFuy 0xFF1Eus
        writeByte memory 0xFFuy 0xFF20us
        writeByte memory 0x00uy 0xFF21us
        writeByte memory 0x00uy 0xFF22us
        writeByte memory 0xBFuy 0xFF23us
        writeByte memory 0x77uy 0xFF24us
        writeByte memory 0xF3uy 0xFF25us
        writeByte memory 0xF1uy 0xFF26us
        writeByte memory 0x91uy 0xFF40us
        writeByte memory 0x00uy 0xFF42us
        writeByte memory 0x00uy 0xFF43us
        writeByte memory 0x00uy 0xFF45us
        writeByte memory 0xFCuy 0xFF47us
        writeByte memory 0xFFuy 0xFF48us
        writeByte memory 0xFFuy 0xFF49us
        writeByte memory 0x00uy 0xFF4Aus
        writeByte memory 0x00uy 0xFF4Bus
        writeByte memory 0x00uy 0xFFFFus

        { memory with bankMode = getBankMode memory }