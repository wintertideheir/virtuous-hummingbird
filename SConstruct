import subprocess
import os

if not COMMAND_LINE_TARGETS:
    print("\nThe Aikaterine Project\n" "\n"
          "SCons Aliases:\n"
          " * build         Build the project.\n"
          " * run           Run the project.\n"
          " * clean         Clean the project")
    Exit(0)

env = Environment(CPPPATH=['/usr/include/', 'src'])
env.Append(ENV={'DISPLAY':os.environ['DISPLAY']}) # Required for the "run" alias
env.Append(CFLAGS='-std=c11')
env.VariantDir('bin', 'src', duplicate=0)

app = env.Program(target='bin/aikaterine',
                  source=['bin/app/app.c', 'bin/draw/draw.c',
                          'bin/draw/shader.c', 'bin/ui/ui.c'],
                  LIBS=['glfw', 'GL', 'GLEW', 'm'])

env.Alias('build', app)
env.Alias('run', env.Command('log/run.log', app, './$SOURCE'))
env.Alias('clean', env.Command('log/clean.log', [],
                               Delete(['bin/aikaterine', 'bin/app', 'bin/draw', 'bin/ui'])))
