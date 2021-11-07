-- RUN: lua %s | FileCheck %s
require "strip_mine"

local test =
loop ("i", 1, 16) {
    "..."
}

print(strip_mine(test, 5))

--[[
CHECK:      for (is = 1; is <= 16; is += 5) {
CHECK-NEXT:     for (i = is; i <= min(16, is + 4); i++) {
CHECK-NEXT:         ...
CHECK-NEXT:     }
CHECK-NEXT: }
--]]
