env = Environment(CPPPATH='/usr/include/')
env.VariantDir('bin', 'src', duplicate=0)
env.Program(target='bin/aikaterine_desktop', source='bin/aikaterine_desktop.c',
            LIBS=['glfw','GL','GLEW'])
