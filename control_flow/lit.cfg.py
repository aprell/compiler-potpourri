import lit.formats

config.name = 'Control flow'

config.test_format = lit.formats.ShTest()

config.suffixes = ['.lua']

config.excludes = [
    'cfg.lua',
    'dfs.lua',
    'dom.lua'
]
