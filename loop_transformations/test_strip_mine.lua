require "strip_mine"

local test =
loop ("i", 1, 16) {
    "..."
}

print(strip_mine(test, 5))

--[[
> lua test_strip_mine.lua
for (is = 1; is <= 16; is += 5) {
    for (i = is; i <= min(16, is + 4); i++) {
        ...
    }
}
--]]
