import lit.formats

config.name = 'OpenMP'

config.test_format = lit.formats.ShTest()

config.suffixes = ['.c']

config.excludes = [
    'loop.c',
    'omp.c',
    'task.c',
    'vec4.c'
]

config.environment['OMP_NUM_THREADS'] = '4'
