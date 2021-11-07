-- RUN: lua %s | FileCheck %s
require "loop"

local test =
loop ("i", 1, "n") {
    loop ("j", 1, "n") {
        loop ("k", 1, "n") {
            "..."
        }
    }
}

print(test)

--[[
CHECK:      for (i = 1; i <= n; i++) {
CHECK-NEXT:     for (j = 1; j <= n; j++) {
CHECK-NEXT:         for (k = 1; k <= n; k++) {
CHECK-NEXT:             ...
CHECK-NEXT:         }
CHECK-NEXT:     }
CHECK-NEXT: }
--]]
