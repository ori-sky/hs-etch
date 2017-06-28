add  = (x, y) -> { x + y }
inc  =  x     -> { x + 1 }
inc2 = (x, y) -> { (x + 1, y + 1) }

partialAdd = x -> y -> { x + y }
partialInc2 = x -> y -> { (x + 1, y + 1) }
