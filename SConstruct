env = Environment(CPPPATH='/usr/include/')
env.VariantDir('bin', 'src', duplicate=0)
env.Program(target='bin/aikaterine_desktop', source='bin/aikaterine_desktop.c',
            LIBS=['glfw','GL','GLEW'], LIBPATH='/usr/lib/x86_64-linux-gnu/')
