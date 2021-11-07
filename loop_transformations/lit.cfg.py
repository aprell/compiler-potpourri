import lit.formats

config.name = 'Loop transformations'

config.test_format = lit.formats.ShTest()

config.suffixes = ['.lua']

config.excludes = [
    'loop.lua',
    'simplify.lua',
    'skew.lua',
    'strip_mine.lua',
    'tile.lua'
]
