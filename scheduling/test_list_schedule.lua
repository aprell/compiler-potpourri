-- RUN: lua %s | FileCheck %s
require "list_schedule"

local graph = {
    { id = "1", pred = {          }, succ = { "2" }, ty = "L/S", delay = 2 },
    { id = "2", pred = { "1"      }, succ = { "4" }, ty = "ADD", delay = 1 },
    { id = "3", pred = {          }, succ = { "4" }, ty = "L/S", delay = 2 },
    { id = "4", pred = { "2", "3" }, succ = { "6" }, ty = "ADD", delay = 1 },
    { id = "5", pred = {          }, succ = { "6" }, ty = "MUL", delay = 3 },
    { id = "6", pred = { "4", "5" }, succ = { "7" }, ty = "ADD", delay = 1 },
    { id = "7", pred = { "6"      }, succ = {     }, ty = "L/S", delay = 2 },
}

functional_units {
    ["L/S"] = { count = 2, avail = 2 },
    ["ADD"] = { count = 2, avail = 2 },
    ["MUL"] = { count = 1, avail = 1 },
}

find_remaining(graph)

print(list_schedule(graph))

--[[
-------------+--------------
CHECK: Cycle | Instructions
-------------+--------------
CHECK: 1
CHECK-SAME: [1, 3, 5]
CHECK: 2
CHECK-SAME: []
CHECK: 3
CHECK-SAME: [2]
CHECK: 4
CHECK-SAME: [4]
CHECK: 5
CHECK-SAME: [6]
CHECK: 6
CHECK-SAME: [7]
------------------------------
CHECK: 7 cycles
------------------------------
--]]
