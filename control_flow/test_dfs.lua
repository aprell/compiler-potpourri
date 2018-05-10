require "dfs"

local cfg = require "cfg"

print(table.concat(dfs_preorder(cfg.fib), ", "))
print(table.concat(dfs_postorder(cfg.fib), ", "))
print(table.concat(dfs_reverse_postorder(cfg.fib), ", "))
print(table.concat(dfs_reverse_postorder(invert(cfg.fib)), ", "))

print(table.concat(dfs_preorder(cfg.simple), ", "))
print(table.concat(dfs_postorder(cfg.simple), ", "))
print(table.concat(dfs_reverse_postorder(cfg.simple), ", "))
print(table.concat(dfs_reverse_postorder(invert(cfg.simple)), ", "))
