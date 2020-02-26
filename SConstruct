import subprocess

if not COMMAND_LINE_TARGETS:
    print("\nThe Aikaterine Project\n" "\n"
          "SCons Aliases:\n"
          " * build         Build the project.\n")
    Exit(0)

env = Environment(CPPPATH=['/usr/include/', 'src'])
env.Append(CFLAGS='-std=c11')
env.VariantDir('bin', 'src', duplicate=0)

app = env.Program(target='bin/aikaterine',
                  source=['bin/desktop.c', 'bin/drawing.c',
                          'bin/shaders.c'],
                  LIBS=['glfw', 'GL', 'GLEW', 'm'])

env.Alias('build', app)
