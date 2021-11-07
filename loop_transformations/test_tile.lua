-- RUN: lua %s | FileCheck %s
require "tile"

local test1 =
loop ("i", 1, "n") {
    loop ("j", 1, "m") {
        "..."
    }
}

local test2 =
loop ("i", 1, 50) {
    loop ("j", "i", 60) {
        "..."
    }
}

print(tile(test1, 10, 1))

--[[
CHECK:      for (it = 1; it <= floor((n - 1) / 10) * 10 + 1; it += 10) {
CHECK-NEXT:     for (i = max(1, it); i <= min(n, it + 9); i++) {
CHECK-NEXT:         for (jt = 1; jt <= floor((m - 1) / 10) * 10 + 1; jt += 10) {
CHECK-NEXT:             for (j = max(1, jt); j <= min(m, jt + 9); j++) {
CHECK-NEXT:                 ...
CHECK-NEXT:             }
CHECK-NEXT:         }
CHECK-NEXT:     }
CHECK-NEXT: }
--]]

print(tile(test2, 20, 5))

--[[
CHECK:      for (it = -15; it <= 45; it += 20) {
CHECK-NEXT:     for (i = max(1, it); i <= min(50, it + 19); i++) {
CHECK-NEXT:         for (jt = floor((i - 5) / 20) * 20 + 5; jt <= 45; jt += 20) {
CHECK-NEXT:             for (j = max(i, jt); j <= min(60, jt + 19); j++) {
CHECK-NEXT:                 ...
CHECK-NEXT:             }
CHECK-NEXT:         }
CHECK-NEXT:     }
CHECK-NEXT: }
--]]
