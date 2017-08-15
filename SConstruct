import subprocess

env = Environment(CPPPATH='/usr/include/')
env.Append(CFLAGS='-std=c11')
env.VariantDir('bin', 'src', duplicate=0)
env.Program(target='bin/app/aikaterine_desktop', source='bin/app/aikaterine_desktop.c',
            LIBS=['glfw', 'GL', 'GLEW', 'm'])
env.Library(target='bin/lib/aikaterine', source='bin/lib/aikaterine.c',
            CCFLAGS=subprocess.check_output(
                ['pkg-config', '--cflags', '--libs', 'glib-2.0']).strip())
