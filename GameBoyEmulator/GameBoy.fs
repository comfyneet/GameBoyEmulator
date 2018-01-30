namespace GameBoyEmulator

module GameBoy =

    let init () =
        { cpu = Cpu.init ()
          mmu = Mmu.init ()
          gpu = Gpu.init () }