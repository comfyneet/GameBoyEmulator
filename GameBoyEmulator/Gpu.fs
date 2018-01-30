namespace GameBoyEmulator

module Gpu =

    let init () =
        { mode = HBlank
          ticks = 0 }

    let step sys =
        match sys.gpu.mode with
        | HBlank ->
            if ticks >= 204 then