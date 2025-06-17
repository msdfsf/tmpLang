# vi-debugger README

Wrapper for gdb to run .vi files.

If not autodetected, add new debug config in .vscode/launch.json and use predefined Vi Debugger config (type Vi and snippet should appear).

On start of debug session adds debug tasks to .vscode/tasks.json.
If tasks were not added successfully for some reason, command Vi Debugger: Vi Debugger: Ensure Tasks can be used to add them manually.
Doesn't support comments in tasks.json

Don't forget to specify viDebugger.compilerPath in launch.json.