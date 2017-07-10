import subprocess

env = Environment(CPPPATH='/usr/include/')
env.VariantDir('bin', 'src', duplicate=0)
env.Program(target='bin/aikaterine_desktop', source='bin/aikaterine_desktop.c',
            LIBS=['glfw', 'GL', 'GLEW', 'm'])
env.Library(target='bin/aikaterine', source='bin/aikaterine.c',
            CCFLAGS=subprocess.check_output(
                ['pkg-config', '--cflags', '--libs', 'glib-2.0']).strip())
