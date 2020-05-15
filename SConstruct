import subprocess
import os

if not COMMAND_LINE_TARGETS:
    print("\nThe Aikaterine Project\n" "\n"
          "SCons Aliases:\n"
          " * build         Build the project.\n"
          " * run           Run the project.\n"
          " * clean         Clean the project")
    Exit(0)

freetype2_flags = subprocess.check_output(['pkg-config', '--cflags', 'freetype2']).strip()

env = Environment(CPPPATH=['/usr/include/', 'src'])
env.Append(ENV={'DISPLAY':os.environ['DISPLAY']}) # Required for the "run" alias
env.Append(CFLAGS=['-std=c11', freetype2_flags])
env.VariantDir('bin', 'src', duplicate=0)

app = env.Program(target='bin/aikaterine',
                  source=['bin/app.c',    'bin/draw.c',
                          'bin/shader.c', 'bin/ui.c',
                          'bin/shapes.c', 'bin/text.c'],
                  LIBS=['glfw', 'GL', 'GLEW', 'm', 'freetype'])

env.Alias('build', app)
env.Alias('run', env.Command('log/run.log', app, './$SOURCE'))
env.Alias('clean', env.Command('log/clean.log', [],
                               Delete(['bin/aikaterine', 'bin/app', 'bin/draw', 'bin/ui'])))
