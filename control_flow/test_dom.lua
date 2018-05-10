require "dom"

local cfg = require "cfg"

dominators(cfg.fib)

immediate_dominators(cfg.fib)

back_edges(cfg.fib)

print()

dominators(cfg.simple)

immediate_dominators(cfg.simple)

back_edges(cfg.simple)
