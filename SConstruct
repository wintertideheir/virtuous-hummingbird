import subprocess

env = Environment(CPPPATH='/usr/include/')
env.Append(CFLAGS='-std=c11')
env.VariantDir('bin', 'src', duplicate=0)
env.Library(target='bin/lib/aikaterine', source='bin/lib/aikaterine.c',
            CCFLAGS=subprocess.check_output(
                ['pkg-config', '--cflags', '--libs', 'glib-2.0']).strip())
env.Program(target='bin/app/aikaterine_desktop',
            source=['bin/app/desktop.c', 'bin/app/drawing.c'],
            LIBS=['glfw', 'GL', 'GLEW', 'm', 'aikaterine'], LIBPATH='#/bin/lib',
            CCFLAGS='-I' + Dir('#').abspath + '/src/lib')
