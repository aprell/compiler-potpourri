import lit.formats

config.name = 'Data dependences'

config.test_format = lit.formats.ShTest()

config.suffixes = ['.lua']

config.excludes = [
    'analysis',
    'array.lua',
    'dependence.lua',
    'iteration_vector.lua',
    'loop.lua',
    'range.lua'
]
