-- RUN: lua %s | FileCheck %s
require "dom"

local cfg = require "cfg"

dominators(cfg.fib)

immediate_dominators(cfg.fib)

-- CHECK-DAG: B1 idom B2
-- CHECK-DAG: B1 idom B6
-- CHECK-DAG: B2 idom B3
-- CHECK-DAG: B3 idom B4
-- CHECK-DAG: B3 idom B5

back_edges(cfg.fib)

-- CHECK: B4 => B3

print()

dominators(cfg.simple)

immediate_dominators(cfg.simple)

-- CHECK-DAG: B1 idom B2
-- CHECK-DAG: B1 idom B3
-- CHECK-DAG: B1 idom B4

back_edges(cfg.simple)

-- No back edge
-- CHECK-NOT: =>
