-- RUN: lua %s | FileCheck %s
require "dfs"

local cfg = require "cfg"

print(table.concat(dfs_preorder(cfg.fib), ", "))
-- CHECK: B1, B2, B3, B4, B5, B6

print(table.concat(dfs_postorder(cfg.fib), ", "))
-- CHECK: B4, B5, B3, B2, B6, B1

print(table.concat(dfs_reverse_postorder(cfg.fib), ", "))
-- CHECK: B1, B6, B2, B3, B5, B4

print(table.concat(dfs_reverse_postorder(invert(cfg.fib)), ", "))
-- CHECK: B6, B5, B3, B4, B2, B1

print(table.concat(dfs_preorder(cfg.simple), ", "))
-- CHECK: B1, B2, B4, B3

print(table.concat(dfs_postorder(cfg.simple), ", "))
-- CHECK: B4, B2, B3, B1

print(table.concat(dfs_reverse_postorder(cfg.simple), ", "))
-- CHECK: B1, B3, B2, B4

print(table.concat(dfs_reverse_postorder(invert(cfg.simple)), ", "))
-- CHECK: B4, B3, B2, B1
