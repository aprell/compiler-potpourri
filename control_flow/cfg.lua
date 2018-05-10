local cfg = {}

--[[
    receive n           // B1
    t0 := 0             // ..
    t1 := 1             // ..
    if n < 2 goto L3    // ..
    i := 2              // B2
L1:                     // B3
    if i > n goto L2    // ..
    t2 := t0 + t1       // B4
    t0 := t1            // ..
    t1 := t2            // ..
    i := i + 1          // ..
    goto L1             // ..
L2:                     // B5
    return t1           // ..
L3:                     // B6
    return n            // ..
--]]

cfg.fib = {
    entry = { succ = { "B1" } },
       B1 = { succ = { "B2", "B6" }, pred = { "entry" } },
       B2 = { succ = { "B3" }, pred = { "B1" } },
       B3 = { succ = { "B4", "B5" }, pred = { "B2", "B4" } },
       B4 = { succ = { "B3" }, pred = { "B3" } },
       B5 = { succ = { "exit" }, pred = { "B3" } },
       B6 = { succ = { "exit" }, pred = { "B1" } },
     exit = { pred = { "B5", "B6" } },
}

cfg.simple = {
    entry = { succ = { "B1" } },
       B1 = { succ = { "B2", "B3" }, pred = { "entry" } },
       B2 = { succ = { "B4" }, pred = { "B1" } },
       B3 = { succ = { "B4" }, pred = { "B1" } },
       B4 = { succ = { "exit" }, pred = { "B2", "B3" } },
     exit = { pred = { "B4" } },
}

return cfg
