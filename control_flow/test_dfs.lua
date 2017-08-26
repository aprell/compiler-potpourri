require "dfs"

local cfg = require "cfg"

print(table.concat(dfs(cfg.fib), " -> "))

print(table.concat(dfs(invert(cfg.fib)), " -> "))
