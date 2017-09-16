require "skew"

local test =
loop ("i", 1, "n") {
    loop ("j", 1, "m") {
        "..."
    }
}

print(test)
print(skew(test, 1))
print(skew_interchange(test, 1))

--[[
> lua test_skew.lua
for (i = 1; i <= n; i++) {
    for (j = 1; j <= m; j++) {
        ...
    }
}
for (is = 1; is <= n; is++) {
    for (js = is + 1; js <= is + m; js++) {
        i = is; j = js - is; ...
    }
}
for (js = 2; js <= (n) + m; js++) {
    for (is = max(1, js - (m)); is <= min(n, js - (1)); is++) {
        i = is; j = js - is; ...
    }
}
--]]
