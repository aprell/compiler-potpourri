-- RUN: lua %s | FileCheck %s
require "skew"

local test =
loop ("i", 1, "n") {
    loop ("j", 1, "m") {
        "..."
    }
}

print(test)

--[[
CHECK:      for (i = 1; i <= n; i++) {
CHECK-NEXT:     for (j = 1; j <= m; j++) {
CHECK-NEXT:         ...
CHECK-NEXT:     }
CHECK-NEXT: }
--]]

print(skew(test, 1))

--[[
CHECK:      for (is = 1; is <= n; is++) {
CHECK-NEXT:     for (js = is + 1; js <= is + m; js++) {
CHECK-NEXT:         i = is; j = js - is; ...
CHECK-NEXT:     }
CHECK-NEXT: }
--]]

print(skew_interchange(test, 1))

--[[
CHECK:      for (js = 2; js <= (n) + m; js++) {
CHECK-NEXT:     for (is = max(1, js - (m)); is <= min(n, js - (1)); is++) {
CHECK-NEXT:         i = is; j = js - is; ...
CHECK-NEXT:     }
CHECK-NEXT: }
--]]
