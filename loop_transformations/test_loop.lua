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
> lua test_loop.lua
for (i = 1; i <= n; i++) {
    for (j = 1; j <= n; j++) {
        for (k = 1; k <= n; k++) {
            ...
        }
    }
}
--]]
